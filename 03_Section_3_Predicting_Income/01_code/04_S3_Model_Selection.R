################################################################################
##                                                                            ##
##                    SELECCIÓN DE MODELOS - SECCIÓN 3                        ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

## Descripción:
##   Selecciona el mejor modelo comparando todas las especificaciones
##   (baseline + extendidas) usando dos capas de criterios:
##
##     Capa 1 — Evaluación en training :
##       R² ajustado, AIC, BIC
##
##     Capa 2 — Evaluación out-of-sample (predicción):
##       RMSE sobre el validation set (chunks 8-10)
##
##   El modelo seleccionado es el que minimiza el RMSE de validación,
##   documentando si existe tensión con los criterios de training.
##   Este modelo se guarda como 'best_model' y es consumido por los
##   cuadernos de LOOCV, influencia y errores de predicción.

# ==============================================================================
# 0. VERIFICACIÓN DE PREREQUISITOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("   SECTION 3: SELECCIÓN DE MODELOS                              \n")
cat("================================================================\n\n")

if (!exists("all_results")) {
  stop("ERROR: El objeto 'all_results' no existe.
       Asegúrese de haber corrido 02_s3_Baseline_Models.R y
       03_s3_Extended_Models.R primero.")
}

if (!exists("models_list") | !exists("extended_models_list")) {
  stop("ERROR: Las listas de modelos no existen.
       Asegúrese de haber corrido los cuadernos anteriores primero.")
}

# Unir todas las listas de modelos en una sola para acceso uniforme
all_models_list <- c(models_list, extended_models_list)

cat(sprintf("Total de especificaciones evaluadas: %d\n", nrow(all_results)))
cat(sprintf("  Baseline:  %d modelos\n", nrow(baseline_results)))
cat(sprintf("  Extendidos: %d modelos\n", nrow(extended_results)))

# ==============================================================================
# 1. TABLA COMPARATIVA COMPLETA
# ==============================================================================

cat("\n--- Tabla comparativa completa (ordenada por RMSE validación) ---\n\n")

all_results_sorted <- all_results %>%
  arrange(RMSE_val) %>%
  mutate(
    Rank_RMSE = row_number(),
    Rank_AIC  = rank(AIC),
    Rank_BIC  = rank(BIC),
    Rank_R2   = rank(-R2_adj)   # Negativo porque mayor R² es mejor
  )

print(all_results_sorted %>%
        select(Rank_RMSE, Modelo, Descripcion, R2_adj, AIC, BIC, RMSE_val),
      row.names = FALSE)

# ==============================================================================
# 2. ANÁLISIS DE TENSIÓN ENTRE CRITERIOS
# ==============================================================================
# Verifica si el modelo con menor RMSE también es el mejor según AIC y BIC.
# Si no coinciden, documenta la tensión y justifica la elección final.

cat("\n--- Análisis de tensión entre criterios ---\n\n")

mejor_rmse <- all_results_sorted$Modelo[1]
mejor_aic  <- all_results_sorted$Modelo[which.min(all_results_sorted$AIC)]
mejor_bic  <- all_results_sorted$Modelo[which.min(all_results_sorted$BIC)]
mejor_r2   <- all_results_sorted$Modelo[which.max(all_results_sorted$R2_adj)]

cat(sprintf("  Mejor por RMSE validación: %s\n", mejor_rmse))
cat(sprintf("  Mejor por AIC:             %s\n", mejor_aic))
cat(sprintf("  Mejor por BIC:             %s\n", mejor_bic))
cat(sprintf("  Mejor por R² ajustado:     %s\n", mejor_r2))

if (mejor_rmse == mejor_aic & mejor_rmse == mejor_bic) {
  cat("\n  ✓ Sin tensión: todos los criterios apuntan al mismo modelo.\n")
} else {
  cat("\n  ✗ Tensión detectada entre criterios:\n")
  if (mejor_rmse != mejor_aic) {
    cat(sprintf("    - RMSE favorece %s, AIC favorece %s\n", mejor_rmse, mejor_aic))
  }
  if (mejor_rmse != mejor_bic) {
    cat(sprintf("    - RMSE favorece %s, BIC favorece %s\n", mejor_rmse, mejor_bic))
  }
  cat("\n  Criterio de selección final: RMSE de validación.\n")
  cat("  Justificación: el objetivo es predicción out-of-sample para\n")
  cat("  identificar posible sub-reporte de ingresos (DIAN). El RMSE\n")
  cat("  sobre datos no vistos es el criterio más directo para este fin.\n")
}

# ==============================================================================
# 3. SELECCIÓN DEL MEJOR MODELO
# ==============================================================================

best_model_name <- mejor_rmse
best_model      <- all_models_list[[best_model_name]]
best_model_info <- all_results_sorted %>% filter(Modelo == best_model_name)

cat("\n================================================================\n")
cat("   MODELO SELECCIONADO                                          \n")
cat("================================================================\n")
cat(sprintf("  Nombre:           %s\n",   best_model_name))
cat(sprintf("  Descripción:      %s\n",   best_model_info$Descripcion))
cat(sprintf("  R² ajustado:      %.4f\n", best_model_info$R2_adj))
cat(sprintf("  AIC:              %.2f\n", best_model_info$AIC))
cat(sprintf("  BIC:              %.2f\n", best_model_info$BIC))
cat(sprintf("  RMSE validación:  %.4f\n", best_model_info$RMSE_val))
cat(sprintf("  Fórmula:\n"))
print(formula(best_model))

# ==============================================================================
# 4. FIGURA: COMPARACIÓN DE RMSE POR MODELO
# ==============================================================================

all_results_sorted$Modelo <- factor(all_results_sorted$Modelo,
                                    levels = rev(all_results_sorted$Modelo))

plot_rmse <- ggplot(all_results_sorted,
                    aes(x = RMSE_val, y = Modelo,
                        color = Seccion, fill = Seccion)) +
  geom_col(alpha = 0.7, width = 0.6) +
  geom_vline(xintercept = min(all_results_sorted$RMSE_val),
             linetype = "dashed", color = col_secondary, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.4f", RMSE_val)),
            hjust = -0.1, size = 3.2, color = "gray30") +
  scale_color_manual(values = c("S1" = col_primary,
                                "S2" = col_accent,
                                "S3" = col_secondary)) +
  scale_fill_manual(values  = c("S1" = col_primary,
                                "S2" = col_accent,
                                "S3" = col_secondary)) +
  labs(
    title    = "Comparación de RMSE por especificación",
    subtitle = "Evaluado sobre validation set (chunks 8-10)",
    x        = "RMSE (validation)",
    y        = NULL,
    color    = "Sección",
    fill     = "Sección"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("02_output/figures/01_s3_rmse_comparison.png",
       plot_rmse, width = 10, height = 6, dpi = 300)

cat("\n  Figura guardada: 02_output/figures/01_s3_rmse_comparison.png\n")

# ==============================================================================
# 5. RESUMEN FINAL
# ==============================================================================

cat("\n================================================================\n")
cat("   OBJETOS GENERADOS PARA SCRIPTS SIGUIENTES:                   \n")
cat("================================================================\n")
cat(sprintf("  best_model        → Objeto lm() del modelo seleccionado (%s)\n",
            best_model_name))
cat(sprintf("  best_model_name   → Nombre del modelo seleccionado\n"))
cat(sprintf("  all_results_sorted → Tabla completa ordenada por RMSE\n"))
cat("================================================================\n")