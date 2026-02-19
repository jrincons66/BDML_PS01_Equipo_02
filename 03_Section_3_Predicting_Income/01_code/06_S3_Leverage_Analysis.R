################################################################################
##                                                                            ##
##                   ANÁLISIS DE INFLUENCIA - SECCIÓN 3                       ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

## Descripción:
##   Calcula la medida de influencia de cada observación sobre el vector de
##   coeficientes del mejor modelo, definida como:
##
##       D_i = || β̂ - β̂_(i) ||₂
##
##   Donde β̂_(i) son los coeficientes estimados omitiendo la observación i.
##   Implementada via la descomposición FWL usando la fórmula analítica:
##
##       β̂_(i) = β̂ - (X'X)^{-1} x_i * e_i / (1 - h_ii)
##
##   Lo que evita re-estimar el modelo N veces.
##
##   ADVERTENCIA del enunciado: esta norma NO es invariante al escalado de
##   los regresores. Por eso se estandarizan todas las variables continuas
##   antes del cálculo, de forma que D_i refleje influencia real y no
##   diferencias en unidades de medida.

# ==============================================================================
# 0. VERIFICACIÓN DE PREREQUISITOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("   SECTION 3: ANÁLISIS DE INFLUENCIA                            \n")
cat("================================================================\n\n")

if (!exists("best_model") | !exists("best_model_name")) {
  stop("ERROR: 'best_model' no existe.
       Asegúrese de haber corrido 04_s3_Model_Selection.R primero.")
}

if (!exists("loo_errors") | !exists("hat_values")) {
  stop("ERROR: 'loo_errors' y/o 'hat_values' no existen.
       Asegúrese de haber corrido 05_s3_LOOCV.R primero.")
}

cat(sprintf("Modelo: %s\n", best_model_name))
cat(sprintf("Observaciones en training: %s\n\n",
            format(nrow(geih_train), big.mark = ",")))

# ==============================================================================
# 1. CONSTRUCCIÓN DE LA MATRIZ X ESTANDARIZADA
# ==============================================================================
# Extraemos la matriz de diseño del modelo y estandarizamos las variables
# continuas (media 0, sd 1). Las dummies y factores se dejan sin cambio
# ya que su escala es fija (0/1).

cat("--- Construyendo matriz X estandarizada ---\n\n")

# Matriz de diseño completa (igual a la usada por lm internamente)
X <- model.matrix(best_model)

# Identificar columnas continuas (más de 2 valores únicos)
cols_continuas <- apply(X, 2, function(col) length(unique(col)) > 2)

# Estandarizar solo columnas continuas (excluye intercept y dummies)
X_std <- X
X_std[, cols_continuas] <- scale(X[, cols_continuas])

cat(sprintf("  Variables en el modelo:       %d\n", ncol(X)))
cat(sprintf("  Variables continuas escaladas: %d\n", sum(cols_continuas)))
cat(sprintf("  Variables dummy/factor:        %d\n", sum(!cols_continuas)))

# ==============================================================================
# 2. CÁLCULO DE INFLUENCIA VÍA FÓRMULA ANALÍTICA
# ==============================================================================
# Usando la actualización de Sherman-Morrison:
#   β̂_(i) = β̂ - (X'X)^{-1} x_i * e_i / (1 - h_ii)
#
# Por lo tanto:
#   β̂ - β̂_(i) = (X'X)^{-1} x_i * e_i / (1 - h_ii)
#              = (X'X)^{-1} x_i * LOO_error_i
#
# Y la norma:
#   D_i = || (X'X)^{-1} x_i * LOO_error_i ||₂

cat("\n--- Calculando medidas de influencia ---\n")

# Excluir observaciones con leverage = 1
mask <- hat_values < 1

X_std_masked <- X_std[mask, ]
n_obs        <- nrow(X_std_masked)

# (X'X)^{-1} sobre la matriz estandarizada
XtX_inv <- solve(t(X_std_masked) %*% X_std_masked)

# Calcular D_i para cada observación
influence_scores <- numeric(n_obs)

for (i in seq_len(n_obs)) {
  xi        <- X_std_masked[i, ]                    # Vector fila i
  delta_beta <- as.numeric(XtX_inv %*% xi) * loo_errors[i]  # Cambio en β
  influence_scores[i] <- sqrt(sum(delta_beta^2))    # Norma L2
}

cat(sprintf("  Calculadas %s medidas de influencia\n\n",
            format(n_obs, big.mark = ",")))

# ==============================================================================
# 3. CONSTRUIR DATA FRAME DE RESULTADOS
# ==============================================================================

influence_df <- geih_train[mask, ] %>%
  mutate(
    influence  = influence_scores,
    hat_value  = hat_values[mask],
    loo_error  = loo_errors,
    residuo    = residuals(best_model)[mask]
  )

# ==============================================================================
# 4. ESTADÍSTICAS DE INFLUENCIA
# ==============================================================================

cat("--- Distribución de medidas de influencia ---\n\n")

cat(sprintf("  Media:    %.6f\n", mean(influence_scores)))
cat(sprintf("  Mediana:  %.6f\n", median(influence_scores)))
cat(sprintf("  P90:      %.6f\n", quantile(influence_scores, 0.90)))
cat(sprintf("  P95:      %.6f\n", quantile(influence_scores, 0.95)))
cat(sprintf("  P99:      %.6f\n", quantile(influence_scores, 0.99)))
cat(sprintf("  Máximo:   %.6f\n", max(influence_scores)))

# Umbral: observaciones en el top 1% de influencia
umbral_influencia <- quantile(influence_scores, 0.99)
n_high_influence  <- sum(influence_scores >= umbral_influencia)

cat(sprintf("\n  Observaciones en top 1%% (D_i >= %.6f): %d\n",
            umbral_influencia, n_high_influence))

# ==============================================================================
# 5. PERFIL DE OBSERVACIONES MÁS INFLUYENTES
# ==============================================================================

cat("\n--- Perfil de observaciones más influyentes (top 1%%) ---\n\n")

high_influence <- influence_df %>%
  filter(influence >= umbral_influencia)

low_influence <- influence_df %>%
  filter(influence < umbral_influencia)

# Comparación de características observables
comparacion <- data.frame(
  Variable         = c("Edad (media)", "% Mujeres", "Log Ingreso (media)",
                       "Ingreso mensual (media)", "Horas trabajadas (media)",
                       "Leverage (media)"),
  Alta_Influencia  = c(
    round(mean(high_influence$age,              na.rm = TRUE), 1),
    round(100 * mean(high_influence$female,     na.rm = TRUE), 1),
    round(mean(high_influence$log_income,       na.rm = TRUE), 3),
    round(mean(high_influence$y_total_m,        na.rm = TRUE), 0),
    round(mean(high_influence$totalHoursWorked, na.rm = TRUE), 1),
    round(mean(high_influence$hat_value,        na.rm = TRUE), 4)
  ),
  Baja_Influencia  = c(
    round(mean(low_influence$age,              na.rm = TRUE), 1),
    round(100 * mean(low_influence$female,     na.rm = TRUE), 1),
    round(mean(low_influence$log_income,       na.rm = TRUE), 3),
    round(mean(low_influence$y_total_m,        na.rm = TRUE), 0),
    round(mean(low_influence$totalHoursWorked, na.rm = TRUE), 1),
    round(mean(low_influence$hat_value,        na.rm = TRUE), 4)
  )
)

print(comparacion, row.names = FALSE)

# ==============================================================================
# 6. FIGURAS
# ==============================================================================

# --- 6.1 Influencia vs. Leverage ---
plot_influence_leverage <- ggplot(influence_df,
                                  aes(x = hat_value, y = influence)) +
  geom_point(aes(color = influence >= umbral_influencia),
             alpha = 0.4, size = 1.2) +
  geom_hline(yintercept = umbral_influencia,
             linetype = "dashed", color = col_secondary) +
  scale_color_manual(values = c("FALSE" = col_accent,
                                "TRUE"  = col_secondary),
                     labels = c("Baja influencia", "Alta influencia (top 1%)")) +
  labs(
    title    = "Influencia sobre coeficientes vs. Leverage",
    subtitle = "Línea punteada = umbral top 1% de influencia",
    x        = "Leverage (h_ii)",
    y        = "Influencia (|| β̂ - β̂_(i) ||₂)",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("02_output/figures/03_s3_influence_leverage.png",
       plot_influence_leverage, width = 10, height = 6, dpi = 300)

# --- 6.2 Influencia vs. Log Ingreso ---
plot_influence_income <- ggplot(influence_df,
                                aes(x = log_income, y = influence)) +
  geom_point(aes(color = influence >= umbral_influencia),
             alpha = 0.4, size = 1.2) +
  geom_hline(yintercept = umbral_influencia,
             linetype = "dashed", color = col_secondary) +
  scale_color_manual(values = c("FALSE" = col_accent,
                                "TRUE"  = col_secondary),
                     labels = c("Baja influencia", "Alta influencia (top 1%)")) +
  labs(
    title    = "Influencia sobre coeficientes vs. Log Ingreso",
    subtitle = "Línea punteada = umbral top 1% de influencia",
    x        = "Log Ingreso Mensual",
    y        = "Influencia (|| β̂ - β̂_(i) ||₂)",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("02_output/figures/04_s3_influence_income.png",
       plot_influence_income, width = 10, height = 6, dpi = 300)

cat("\n  Figuras guardadas en 02_output/figures/\n")
