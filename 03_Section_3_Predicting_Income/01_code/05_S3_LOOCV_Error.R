################################################################################
##                                                                            ##
##                         LOOCV - SECCIÓN 3                                  ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

## Descripción:
##   Calcula el Leave-One-Out Cross-Validation (LOOCV) error para el mejor
##   modelo seleccionado en 04_s3_Model_Selection.R, usando la fórmula
##   analítica basada en hat values:
##
##       LOO_error_i = e_i / (1 - h_ii)
##       LOOCV_RMSE  = sqrt( mean( LOO_error_i² ) )
##
##   Donde e_i es el residuo y h_ii es el leverage de la observación i.
##   Esta fórmula es algebraicamente equivalente a correr N regresiones
##   omitiendo una observación cada vez, pero computacionalmente eficiente.
##
##   El LOOCV_RMSE se compara con el RMSE del validation set para evaluar:
##     - Overfitting: si LOOCV >> RMSE val, el modelo depende de obs. específicas
##     - Representatividad: si ambos son similares, el validation es comparable
##       al training en dificultad predictiva

# ==============================================================================
# 0. VERIFICACIÓN DE PREREQUISITOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("   SECTION 3: LOOCV                                             \n")
cat("================================================================\n\n")

if (!exists("best_model") | !exists("best_model_name")) {
  stop("ERROR: Los objetos 'best_model' y/o 'best_model_name' no existen.
       Asegúrese de haber corrido 04_s3_Model_Selection.R primero.")
}

cat(sprintf("Modelo seleccionado: %s\n", best_model_name))
cat(sprintf("Observaciones en training: %s\n\n",
            format(nrow(geih_train), big.mark = ",")))

# ==============================================================================
# 1. CÁLCULO DE LOOCV VÍA FÓRMULA ANALÍTICA
# ==============================================================================

cat("--- Calculando LOOCV via fórmula analítica ---\n\n")

# Residuos del modelo estimado sobre training
residuos <- residuals(best_model)

# Hat values (leverage): diagonal de H = X(X'X)^-1 X'
# lm.influence() los calcula directamente del objeto lm sin reestimar
hat_values <- lm.influence(best_model)$hat

# Verificar que ningún hat value sea exactamente 1 (causaría división por cero)
n_perfect_leverage <- sum(hat_values >= 1)
if (n_perfect_leverage > 0) {
  warning(sprintf("%d observaciones con leverage = 1 excluidas del LOOCV.", 
                  n_perfect_leverage))
  mask <- hat_values < 1
} else {
  mask <- rep(TRUE, length(hat_values))
}

# Error LOO por observación
loo_errors <- residuos[mask] / (1 - hat_values[mask])

# LOOCV RMSE
loocv_rmse <- sqrt(mean(loo_errors^2))

cat(sprintf("LOOCV RMSE (training): %.4f\n", loocv_rmse))

# ==============================================================================
# 2. COMPARACIÓN LOOCV VS VALIDATION RMSE
# ==============================================================================

cat("\n--- Comparación LOOCV vs. Validation RMSE ---\n\n")

# RMSE del validation set para el mejor modelo
best_rmse_val <- all_results_sorted$RMSE_val[all_results_sorted$Modelo == best_model_name]

diferencia    <- loocv_rmse - best_rmse_val
diferencia_pct <- 100 * diferencia / best_rmse_val

cat(sprintf("  LOOCV RMSE   (training): %.4f\n", loocv_rmse))
cat(sprintf("  RMSE validación:         %.4f\n", best_rmse_val))
cat(sprintf("  Diferencia:              %+.4f  (%+.2f%%)\n",
            diferencia, diferencia_pct))

cat("\n  Interpretación:\n")
if (abs(diferencia_pct) <= 5) {
  cat("  ✓ LOOCV y validation RMSE son muy similares (<5% de diferencia).\n")
  cat("    El modelo generaliza bien y el validation set es representativo\n")
  cat("    del training en términos de dificultad predictiva.\n")
} else if (diferencia_pct > 5) {
  cat("  ✗ LOOCV RMSE > Validation RMSE (diferencia > 5%).\n")
  cat("    El modelo es más sensible a la exclusión de observaciones\n")
  cat("    individuales de lo que sugiere el validation set. Posible\n")
  cat("    presencia de observaciones influyentes en el training.\n")
} else {
  cat("  ~ LOOCV RMSE < Validation RMSE.\n")
  cat("    El validation set es más difícil de predecir que el training.\n")
  cat("    Puede reflejar diferencias distribucionales entre chunks.\n")
}

# ==============================================================================
# 3. DISTRIBUCIÓN DE LOS ERRORES LOO
# ==============================================================================

loocv_df <- data.frame(
  loo_error    = loo_errors,
  loo_error_abs = abs(loo_errors),
  residuo      = residuos[mask],
  hat_value    = hat_values[mask]
) %>%
  bind_cols(geih_train[mask, c("age", "female", "log_income",
                               "y_total_m", "indus", "educ",
                               "firm_size", "totalHoursWorked")])

# Guardar para uso en cuaderno 07
loocv_df_full <- data.frame(
  loo_error     = loo_errors,
  loo_error_abs = abs(loo_errors),
  hat_value     = hat_values[mask]
) %>%
  bind_cols(geih_train[mask, ])

cat("\n--- Distribución de errores LOO ---\n\n")
cat(sprintf("  Media:           %+.4f\n", mean(loo_errors)))
cat(sprintf("  Mediana:         %+.4f\n", median(loo_errors)))
cat(sprintf("  SD:               %.4f\n", sd(loo_errors)))
cat(sprintf("  P5:              %+.4f\n", quantile(loo_errors, 0.05)))
cat(sprintf("  P95:             %+.4f\n", quantile(loo_errors, 0.95)))
cat(sprintf("  Máximo absoluto:  %.4f\n", max(abs(loo_errors))))

# ==============================================================================
# 4. FIGURA: DISTRIBUCIÓN DE ERRORES LOO
# ==============================================================================

plot_loo_dist <- ggplot(loocv_df, aes(x = loo_error)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, fill = col_primary, alpha = 0.7, color = "white") +
  geom_density(color = col_secondary, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title    = "Distribución de errores LOO",
    subtitle = sprintf("LOOCV RMSE: %.4f  |  Validation RMSE: %.4f",
                       loocv_rmse, best_rmse_val),
    x        = "Error LOO  (e_i / (1 - h_ii))",
    y        = "Densidad"
  ) +
  theme_minimal()

ggsave("02_output/figures/02_s3_loocv_distribution.png",
       plot_loo_dist, width = 10, height = 5, dpi = 300)
