################################################################################
##                                                                            ##
##                   ERRORES DE PREDICCIÓN - SECCIÓN 3                        ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

## Descripción:
##   Analiza los errores de predicción del mejor modelo sobre el validation
##   set (chunks 8-10), identificando qué observaciones son más difíciles de
##   predecir y si comparten características comunes.
##
##   Cruza los errores de predicción con las medidas de influencia del
##   cuaderno anterior para distinguir dos tipos de observaciones problemáticas:
##     - Alta influencia + alto error en training: observaciones que distorsionan
##       los coeficientes Y son difíciles de predecir
##     - Bajo influencia + alto error en validation: grupos sistemáticamente
##       mal predichos, independiente de su peso en la estimación
##
##   Cierra con la discusión de política para la DIAN: ¿los errores grandes
##   reflejan potencial sub-reporte de ingresos o limitaciones del modelo?

# ==============================================================================
# 0. VERIFICACIÓN DE PREREQUISITOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("   SECTION 3: ERRORES DE PREDICCIÓN                             \n")
cat("================================================================\n\n")

if (!exists("best_model") | !exists("best_model_name")) {
  stop("ERROR: 'best_model' no existe.
       Asegúrese de haber corrido 04_s3_Model_Selection.R primero.")
}

if (!exists("influence_df") | !exists("umbral_influencia")) {
  stop("ERROR: 'influence_df' y/o 'umbral_influencia' no existen.
       Asegúrese de haber corrido 06_s3_Influence_Analysis.R primero.")
}

cat(sprintf("Modelo: %s\n", best_model_name))
cat(sprintf("Validation set: %s observaciones\n\n",
            format(nrow(geih_validation), big.mark = ",")))

# ==============================================================================
# 1. ERRORES DE PREDICCIÓN EN EL VALIDATION SET
# ==============================================================================

cat("--- Calculando errores de predicción en validation ---\n\n")

pred_val <- predict(best_model, newdata = geih_validation)

errors_df <- geih_validation %>%
  mutate(
    pred        = pred_val,
    error       = log_income - pred,       # Residuo: positivo = sub-predicción
    error_abs   = abs(error),
    error_sq    = error^2,
    sub_pred    = error > 0,               # TRUE: modelo subestima el ingreso
    sobre_pred  = error < 0               # TRUE: modelo sobreestima el ingreso
  )

# Quitar NAs de predict (obs con niveles no vistos en training)
errors_df <- errors_df %>% filter(!is.na(pred))

rmse_val <- sqrt(mean(errors_df$error_sq))

cat(sprintf("  Observaciones con predicción válida: %s\n",
            format(nrow(errors_df), big.mark = ",")))
cat(sprintf("  RMSE validación:                     %.4f\n", rmse_val))
cat(sprintf("  % obs sub-predichas (error > 0):    %.1f%%\n",
            100 * mean(errors_df$sub_pred)))
cat(sprintf("  % obs sobre-predichas (error < 0):  %.1f%%\n",
            100 * mean(errors_df$sobre_pred)))

# ==============================================================================
# 2. DISTRIBUCIÓN DE ERRORES
# ==============================================================================

cat("\n--- Distribución de errores de predicción ---\n\n")

cat(sprintf("  Media:    %+.4f\n", mean(errors_df$error)))
cat(sprintf("  Mediana:  %+.4f\n", median(errors_df$error)))
cat(sprintf("  SD:        %.4f\n", sd(errors_df$error)))
cat(sprintf("  P5:       %+.4f\n", quantile(errors_df$error, 0.05)))
cat(sprintf("  P95:      %+.4f\n", quantile(errors_df$error, 0.95)))

# ==============================================================================
# 3. PERFIL DE OBSERVACIONES CON MAYOR ERROR
# ==============================================================================

cat("\n--- Perfil de observaciones con mayor error absoluto (top 5%%) ---\n\n")

umbral_error <- quantile(errors_df$error_abs, 0.95)

high_error <- errors_df %>% filter(error_abs >= umbral_error)
low_error  <- errors_df %>% filter(error_abs <  umbral_error)

comparacion_error <- data.frame(
  Variable        = c("Edad (media)", "% Mujeres", "Log Ingreso (media)",
                      "Ingreso mensual (media)", "Horas trabajadas (media)",
                      "% Sub-predichos (error > 0)"),
  Alto_Error      = c(
    round(mean(high_error$age,              na.rm = TRUE), 1),
    round(100 * mean(high_error$female,     na.rm = TRUE), 1),
    round(mean(high_error$log_income,       na.rm = TRUE), 3),
    round(mean(high_error$y_total_m,        na.rm = TRUE), 0),
    round(mean(high_error$totalHoursWorked, na.rm = TRUE), 1),
    round(100 * mean(high_error$sub_pred,   na.rm = TRUE), 1)
  ),
  Bajo_Error      = c(
    round(mean(low_error$age,              na.rm = TRUE), 1),
    round(100 * mean(low_error$female,     na.rm = TRUE), 1),
    round(mean(low_error$log_income,       na.rm = TRUE), 3),
    round(mean(low_error$y_total_m,        na.rm = TRUE), 0),
    round(mean(low_error$totalHoursWorked, na.rm = TRUE), 1),
    round(100 * mean(low_error$sub_pred,   na.rm = TRUE), 1)
  )
)

print(comparacion_error, row.names = FALSE)

# ==============================================================================
# 4. CRUCE: ERRORES DE PREDICCIÓN VS. INFLUENCIA
# ==============================================================================
# Compara las observaciones del training según su nivel de influencia
# y su error LOO, para identificar cuáles son problemáticas en ambas
# dimensiones simultáneamente.

cat("\n--- Cruce: influencia (training) vs. error LOO ---\n\n")

cruce_df <- loocv_df_full %>%
  mutate(
    alta_influencia = influence_scores >= umbral_influencia,
    alto_error_loo  = loo_error_abs >= quantile(loo_error_abs, 0.95)
  )

tabla_cruce <- cruce_df %>%
  group_by(alta_influencia, alto_error_loo) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Tipo = case_when(
      alta_influencia & alto_error_loo  ~ "Alta influencia + Alto error LOO",
      alta_influencia & !alto_error_loo ~ "Alta influencia + Bajo error LOO",
      !alta_influencia & alto_error_loo ~ "Baja influencia + Alto error LOO",
      TRUE                              ~ "Baja influencia + Bajo error LOO"
    ),
    pct = round(100 * n / nrow(cruce_df), 2)
  ) %>%
  select(Tipo, n, pct)

print(tabla_cruce, row.names = FALSE)

# ==============================================================================
# 5. CASOS ILUSTRATIVOS
# ==============================================================================
# Examina en detalle las 10 observaciones del validation con mayor error
# absoluto para ilustrar los mecanismos económicos y estadísticos en juego.

cat("\n--- Casos ilustrativos: top 10 errores absolutos en validation ---\n\n")

top10_errors <- errors_df %>%
  arrange(desc(error_abs)) %>%
  slice(1:10) %>%
  select(age, female, log_income, y_total_m,
         totalHoursWorked, educ, indus,
         pred, error, error_abs)

print(top10_errors, row.names = FALSE)

# ==============================================================================
# 6. FIGURAS
# ==============================================================================

# --- 6.1 Distribución de errores en validation ---
plot_error_dist <- ggplot(errors_df, aes(x = error)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, fill = col_primary, alpha = 0.7, color = "white") +
  geom_density(color = col_secondary, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title    = "Distribución de errores de predicción — Validation set",
    subtitle = sprintf("RMSE: %.4f  |  n = %s obs.",
                       rmse_val, format(nrow(errors_df), big.mark = ",")),
    x        = "Error de predicción (log ingreso real − predicho)",
    y        = "Densidad"
  ) +
  theme_minimal()

ggsave("02_output/figures/05_s3_error_distribution.png",
       plot_error_dist, width = 10, height = 5, dpi = 300)

# --- 6.2 Error absoluto por edad ---
plot_error_age <- ggplot(errors_df, aes(x = age, y = error_abs)) +
  geom_point(alpha = 0.3, size = 1, color = col_accent) +
  geom_smooth(method = "loess", color = col_secondary,
              linewidth = 1, se = TRUE) +
  geom_hline(yintercept = umbral_error,
             linetype = "dashed", color = "gray40") +
  labs(
    title    = "Error absoluto de predicción por edad",
    subtitle = "Línea punteada = umbral top 5% de error",
    x        = "Edad",
    y        = "Error absoluto (|log ingreso real − predicho|)"
  ) +
  theme_minimal()

ggsave("02_output/figures/06_s3_error_by_age.png",
       plot_error_age, width = 10, height = 5, dpi = 300)

# --- 6.3 Error absoluto por género ---
plot_error_gender <- ggplot(errors_df,
                            aes(x = factor(female, labels = c("Hombre", "Mujer")),
                                y = error_abs, fill = factor(female))) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(yintercept = umbral_error,
             linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("0" = col_primary, "1" = col_secondary)) +
  labs(
    title    = "Error absoluto de predicción por género",
    subtitle = "Línea punteada = umbral top 5% de error",
    x        = NULL,
    y        = "Error absoluto (|log ingreso real − predicho|)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("02_output/figures/07_s3_error_by_gender.png",
       plot_error_gender, width = 7, height = 5, dpi = 300)

cat("\n  Figuras guardadas en 02_output/figures/\n")

# ==============================================================================
# 7. DISCUSIÓN DE POLÍTICA: IMPLICACIONES PARA LA DIAN
# ==============================================================================

cat("\n================================================================\n")
cat("   IMPLICACIONES PARA LA DIAN                                   \n")
cat("================================================================\n\n")

# Observaciones sub-predichas con alto error: candidatos a auditoría
# (modelo predice menos ingreso del reportado → posible sub-reporte)
candidatos_auditoria <- errors_df %>%
  filter(sub_pred & error_abs >= umbral_error)

cat(sprintf("  Obs. sub-predichas con alto error (top 5%%): %d\n",
            nrow(candidatos_auditoria)))
cat(sprintf("  Ingreso mensual promedio de este grupo:      COP %s\n",
            format(round(mean(candidatos_auditoria$y_total_m)), big.mark = ",")))
cat(sprintf("  Edad promedio:                               %.1f años\n",
            mean(candidatos_auditoria$age)))
cat(sprintf("  %% Mujeres:                                  %.1f%%\n",
            100 * mean(candidatos_auditoria$female)))

cat("\n  Interpretación:\n")
cat("  Las observaciones con error positivo grande (modelo subestima\n")
cat("  el ingreso reportado) son candidatas naturales para auditoría,\n")
cat("  pero deben interpretarse con cautela: el error puede reflejar\n")
cat("  (a) potencial sub-reporte hacia arriba (ingreso real > predicho),\n")
cat("  (b) grupos para los cuales el modelo es estructuralmente inadecuado\n")
cat("  (ej: trabajadores con características atípicas en edad o sector),\n")
cat("  (c) varianza residual genuina no explicada por los regresores.\n")
cat("  Una estrategia de auditoría robusta debe combinar la señal del\n")
cat("  modelo con información adicional sobre el sector y tipo de empleo.\n")

cat("\n================================================================\n")
cat("                    FIN DE LA SECCIÓN 3                         \n")
cat("================================================================\n")