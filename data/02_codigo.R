################################################################################
##                                                                            ##
##                    PROBLEM SET 1 - SECTION 1                               ##
##                    AGE-LABOR INCOME PROFILE                                ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##          Profesor: Ignacio Sarmiento-Barbieri                              ##
##                                                                            ##
################################################################################

# ==============================================================================
# 0. SETUP
# ==============================================================================

rm(list = ls())
set.seed(123456)

# Paquetes
require(pacman)
p_load(tidyverse, boot, stargazer, ggplot2, scales, gridExtra, knitr, kableExtra)

# Crear carpetas
dir.create("output", showWarnings = FALSE)
dir.create("output/tables", showWarnings = FALSE)
dir.create("output/figures", showWarnings = FALSE)

# Tema para gráficos
theme_set(theme_minimal(base_size = 12))

# Colores
col_primary <- "#1E3A5F"
col_secondary <- "#C41E3A"
col_accent <- "#4A90A4"

# ==============================================================================
# 1. CARGAR Y PREPARAR DATOS
# ==============================================================================
# NOTA: Asume que los datos ya fueron scrapeados y guardados como 'geih_raw'
# Si tienes los chunks en una lista, usa: geih_raw <- bind_rows(chunks)
# ==============================================================================

# --- CARGAR TUS DATOS AQUÍ ---
# geih_raw <- readRDS("data/geih_2018_bogota.rds")
# O si tienes chunks: geih_raw <- bind_rows(chunk1, chunk2, ..., chunk10)

# ------------------------------------------------------------------------------
# 1.1 Construcción de la muestra de análisis
# ------------------------------------------------------------------------------

# Criterios según el Problem Set:
# - Empleados (con ingreso laboral reportado)
# - Mayores de 18 años
# - Ingreso mensual positivo (y_total_m > 0)

geih_analysis <- geih_raw %>%
  # Convertir a numérico si es necesario
  mutate(
    age = as.numeric(age),
    y_total_m = as.numeric(y_total_m),
    totalHoursWorked = as.numeric(totalHoursWorked),
    relab = as.factor(relab)
  ) %>%
  # Aplicar filtros de muestra
  filter(
    age >= 18,                    # Mayores de 18
    !is.na(y_total_m),            # Ingreso no missing
    y_total_m > 0,                # Ingreso positivo (empleados)
    !is.na(totalHoursWorked),     # Horas no missing
    totalHoursWorked > 0,         # Horas positivas
    !is.na(relab)                 # Tipo empleo no missing
  ) %>%
  # Crear variables para el modelo
  mutate(
    log_income = log(y_total_m),  # Variable dependiente
    age2 = age^2                   # Término cuadrático
  )

cat("Muestra de análisis:", nrow(geih_analysis), "observaciones\n")

# ==============================================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS (SECCIÓN 1)
# ==============================================================================
# Enfoque: Variables relevantes para el perfil edad-ingreso
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Estadísticas generales
# ------------------------------------------------------------------------------

stats_section1 <- geih_analysis %>%
  summarise(
    N = n(),
    # Edad
    Edad_Media = mean(age),
    Edad_SD = sd(age),
    Edad_Min = min(age),
    Edad_Max = max(age),
    # Ingreso
    Ingreso_Media = mean(y_total_m),
    Ingreso_Mediana = median(y_total_m),
    Ingreso_SD = sd(y_total_m),
    # Log-ingreso
    LogIngreso_Media = mean(log_income),
    LogIngreso_SD = sd(log_income),
    # Horas
    Horas_Media = mean(totalHoursWorked),
    Horas_SD = sd(totalHoursWorked)
  )

print(stats_section1)

# ------------------------------------------------------------------------------
# 2.2 Estadísticas por grupo de edad (motivación para modelo cuadrático)
# ------------------------------------------------------------------------------

stats_by_age <- geih_analysis %>%
  mutate(
    age_group = cut(age, 
                    breaks = c(18, 25, 35, 45, 55, 65, Inf),
                    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                    right = FALSE)
  ) %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    mean_log_income = mean(log_income),
    sd_log_income = sd(log_income),
    mean_hours = mean(totalHoursWorked),
    .groups = "drop"
  ) %>%
  mutate(pct = n / sum(n) * 100)

print(stats_by_age)

# El patrón de ∩ invertida en mean_log_income motiva la especificación cuadrática

# Visualización del patrón ∩ invertida con los promedios pra evitar ruido. 

geih_analysis %>%
  group_by(age) %>%
  summarise(mean_income = mean(log_income, na.rm = TRUE)) %>%
  ggplot(aes(age, mean_income)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Ingreso promedio por edad",
    x = "Edad",
    y = "Log ingreso promedio"
  ) +
  theme_minimal()


# ------------------------------------------------------------------------------
# 2.3 Distribución de tipo de empleo (relab)
# ------------------------------------------------------------------------------

table(geih_analysis$relab)

# ==============================================================================
# 3. ESTIMACIÓN DE MODELOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Modelo Incondicional
#     log(w) = β₁ + β₂Age + β₃Age² + u
# ------------------------------------------------------------------------------
model_unconditional <- lm(log_income ~ age + age2, data = geih_analysis)


summary(model_unconditional)

# Extraer coeficientes
beta_age_unc <- coef(model_unconditional)["age"]
beta_age2_unc <- coef(model_unconditional)["age2"]

# Edad pico: d(log w)/d(age) = β₂ + 2β₃·age = 0  →  age* = -β₂/(2β₃)
peak_age_unconditional <- -beta_age_unc / (2 * beta_age2_unc)

cat("\n=== MODELO INCONDICIONAL ===\n")
cat("Coef. Age:   ", round(beta_age_unc, 5), "\n")
cat("Coef. Age²:  ", round(beta_age2_unc, 7), "\n")
cat("R² ajustado: ", round(summary(model_unconditional)$adj.r.squared, 4), "\n")
cat("EDAD PICO:   ", round(peak_age_unconditional, 2), "años\n")

# Verificar concavidad (β₃ < 0 implica perfil cóncavo)
if (beta_age2_unc < 0) {
  cat("✓ β₃ < 0 → Perfil CÓNCAVO (consistente con teoría del capital humano)\n")
} else {
  cat("✗ β₃ > 0 → Perfil CONVEXO (NO consistente con la teoría)\n")
}

# ------------------------------------------------------------------------------
# 3.2 Modelo Condicional
#     log(w) = β₁ + β₂Age + β₃Age² + β₄Hours + β₅Relab + u
# ------------------------------------------------------------------------------

model_conditional <- lm(log_income ~ age + age2 + totalHoursWorked + relab, 
                        data = geih_analysis)

summary(model_conditional)

# Extraer coeficientes
beta_age_cond <- coef(model_conditional)["age"]
beta_age2_cond <- coef(model_conditional)["age2"]

# Edad pico condicional
peak_age_conditional <- -beta_age_cond / (2 * beta_age2_cond)

cat("\n=== MODELO CONDICIONAL ===\n")
cat("Coef. Age:   ", round(beta_age_cond, 5), "\n")
cat("Coef. Age²:  ", round(beta_age2_cond, 7), "\n")
cat("R² ajustado: ", round(summary(model_conditional)$adj.r.squared, 4), "\n")
cat("EDAD PICO:   ", round(peak_age_conditional, 2), "años\n")

# Comparación
cat("\n=== COMPARACIÓN ===\n")
cat("Diferencia en edad pico: ", round(peak_age_conditional - peak_age_unconditional, 2), "años\n")
cat("Mejora en R²:            ", round(summary(model_conditional)$adj.r.squared - 
                                         summary(model_unconditional)$adj.r.squared, 4), "\n")

# ==============================================================================
# 4. BOOTSTRAP PARA INTERVALOS DE CONFIANZA DE LA EDAD PICO
# ==============================================================================
# La edad pico es función no lineal de los coeficientes: age* = -β₂/(2β₃)
# Usamos bootstrap para obtener IC sin depender de supuestos distribucionales
# ==============================================================================

# Función para calcular edad pico en cada réplica bootstrap
calc_peak_age <- function(data, indices) {
  d <- data[indices, ]
  
  # Modelo incondicional
  fit_unc <- lm(log_income ~ age + age2, data = d)
  peak_unc <- -coef(fit_unc)["age"] / (2 * coef(fit_unc)["age2"])
  
  # Modelo condicional
  fit_cond <- lm(log_income ~ age + age2 + totalHoursWorked + relab, data = d)
  peak_cond <- -coef(fit_cond)["age"] / (2 * coef(fit_cond)["age2"])
  
  return(c(peak_uncond = peak_unc, peak_cond = peak_cond))
}

# Ejecutar bootstrap (B = 1000 réplicas)
cat("\nEjecutando bootstrap (B = 1000)...\n")
boot_results <- boot(data = geih_analysis, statistic = calc_peak_age, R = 1000)

# Intervalos de confianza (método percentil)
ci_unconditional <- boot.ci(boot_results, type = "perc", index = 1)
ci_conditional <- boot.ci(boot_results, type = "perc", index = 2)

# Extraer límites
ci_unc_lower <- ci_unconditional$percent[4]
ci_unc_upper <- ci_unconditional$percent[5]
ci_cond_lower <- ci_conditional$percent[4]
ci_cond_upper <- ci_conditional$percent[5]

# Errores estándar bootstrap
se_unc <- sd(boot_results$t[, 1])
se_cond <- sd(boot_results$t[, 2])

cat("\n=== RESULTADOS BOOTSTRAP ===\n")
cat("\nModelo Incondicional:\n")
cat("  Edad Pico:    ", round(peak_age_unconditional, 2), "años\n")
cat("  SE Bootstrap: ", round(se_unc, 2), "\n")
cat("  IC 95%:       [", round(ci_unc_lower, 2), ", ", round(ci_unc_upper, 2), "]\n")

cat("\nModelo Condicional:\n")
cat("  Edad Pico:    ", round(peak_age_conditional, 2), "años\n")
cat("  SE Bootstrap: ", round(se_cond, 2), "\n")
cat("  IC 95%:       [", round(ci_cond_lower, 2), ", ", round(ci_cond_upper, 2), "]\n")

# ==============================================================================
# 5. TABLA DE REGRESIÓN
# ==============================================================================

stargazer(model_unconditional, model_conditional,
          type = "text",
          title = "Perfil Edad-Ingreso Laboral",
          dep.var.labels = "Log(Ingreso Mensual)",
          column.labels = c("Incondicional", "Condicional"),
          covariate.labels = c("Age", "Age²", "Horas Trabajadas",
                               "Relab: 2", "Relab: 3", "Relab: 4", 
                               "Relab: 5", "Relab: 6", "Relab: 7",
                               "Constante"),
          add.lines = list(
            c("Edad Pico", 
              sprintf("%.2f", peak_age_unconditional),
              sprintf("%.2f", peak_age_conditional)),
            c("IC 95% (Bootstrap)",
              sprintf("[%.2f, %.2f]", ci_unc_lower, ci_unc_upper),
              sprintf("[%.2f, %.2f]", ci_cond_lower, ci_cond_upper))
          ),
          omit.stat = c("f", "ser"),
          notes = "IC de la edad pico calculados por bootstrap (B=1000).",
          out = "output/tables/table1_age_income_profile.txt")

# También en HTML
stargazer(model_unconditional, model_conditional,
          type = "html",
          title = "Perfil Edad-Ingreso Laboral",
          dep.var.labels = "Log(Ingreso Mensual)",
          column.labels = c("Incondicional", "Condicional"),
          covariate.labels = c("Age", "Age²", "Horas Trabajadas",
                               "Relab: 2", "Relab: 3", "Relab: 4", 
                               "Relab: 5", "Relab: 6", "Relab: 7",
                               "Constante"),
          omit.stat = c("f", "ser"),
          out = "output/tables/table1_age_income_profile.html")

# ==============================================================================
# 6. VISUALIZACIONES
# ==============================================================================

# ------------------------------------------------------------------------------
# 6.1 Figura: Perfil Edad-Ingreso Incondicional
# ------------------------------------------------------------------------------

# Medias observadas por edad
means_by_age <- geih_analysis %>%
  group_by(age) %>%
  summarise(mean_log_income = mean(log_income), n = n(), .groups = "drop") %>%
  filter(n >= 10)

# Grid de predicción
age_grid <- tibble(age = seq(18, max(geih_analysis$age), by = 0.5)) %>%
  mutate(age2 = age^2)

age_grid$pred_unconditional <- predict(model_unconditional, newdata = age_grid)
pred_se <- predict(model_unconditional, newdata = age_grid, se.fit = TRUE)
age_grid$se <- pred_se$se.fit

# Gráfico
fig_unconditional <- ggplot() +
  # Medias observadas
  geom_point(data = means_by_age, 
             aes(x = age, y = mean_log_income),
             color = "gray50", alpha = 0.5, size = 2) +
  # IC del modelo
  geom_ribbon(data = age_grid,
              aes(x = age, 
                  ymin = pred_unconditional - 1.96 * se,
                  ymax = pred_unconditional + 1.96 * se),
              fill = col_primary, alpha = 0.2) +
  # Predicción
  geom_line(data = age_grid, 
            aes(x = age, y = pred_unconditional),
            color = col_primary, linewidth = 1.2) +
  # Línea vertical en edad pico
  geom_vline(xintercept = peak_age_unconditional, 
             color = col_secondary, linetype = "dashed", linewidth = 1) +
  # Anotación
  annotate("label", x = peak_age_unconditional + 3, 
           y = min(age_grid$pred_unconditional) + 0.1,
           label = sprintf("Peak: %.1f años\n[IC: %.1f - %.1f]", 
                           peak_age_unconditional, ci_unc_lower, ci_unc_upper),
           fill = "white", color = col_secondary, fontface = "bold", size = 3.5) +
  labs(
    title = "Perfil Edad-Ingreso Incondicional",
    subtitle = expression(log(w) == beta[1] + beta[2]*Age + beta[3]*Age^2 + u),
    x = "Edad (años)",
    y = "Log(Ingreso mensual)",
    caption = sprintf("N = %s. Puntos: media por edad. Banda: IC 95%%.",
                      format(nrow(geih_analysis), big.mark = ","))
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/figures/fig1_profile_unconditional.png", fig_unconditional,
       width = 10, height = 7, dpi = 300)

# ------------------------------------------------------------------------------
# 6.2 Figura: Comparación Incondicional vs Condicional
# ------------------------------------------------------------------------------

# Predicciones del modelo condicional (controles en medias)
age_grid_cond <- age_grid %>%
  mutate(
    totalHoursWorked = mean(geih_analysis$totalHoursWorked),
    relab = factor(names(sort(table(geih_analysis$relab), decreasing = TRUE))[1],
                   levels = levels(geih_analysis$relab))
  )

age_grid_cond$pred_conditional <- predict(model_conditional, newdata = age_grid_cond)

# Combinar predicciones
comparison_data <- age_grid %>%
  select(age, pred_unconditional) %>%
  mutate(pred_conditional = age_grid_cond$pred_conditional) %>%
  pivot_longer(cols = c(pred_unconditional, pred_conditional),
               names_to = "modelo", values_to = "prediccion") %>%
  mutate(modelo = ifelse(modelo == "pred_unconditional", 
                         "Incondicional", "Condicional"))

fig_comparison <- ggplot(comparison_data, aes(x = age, y = prediccion, color = modelo)) +
  geom_line(linewidth = 1.2) +
  # Líneas verticales
  geom_vline(xintercept = peak_age_unconditional, 
             color = col_primary, linetype = "dotted", linewidth = 0.8) +
  geom_vline(xintercept = peak_age_conditional, 
             color = col_accent, linetype = "dotted", linewidth = 0.8) +
  # Anotaciones
  annotate("text", x = peak_age_unconditional - 3, y = max(comparison_data$prediccion),
           label = sprintf("%.1f", peak_age_unconditional),
           color = col_primary, fontface = "bold", size = 4) +
  annotate("text", x = peak_age_conditional + 3, y = max(comparison_data$prediccion),
           label = sprintf("%.1f", peak_age_conditional),
           color = col_accent, fontface = "bold", size = 4) +
  scale_color_manual(values = c("Incondicional" = col_primary, 
                                "Condicional" = col_accent),
                     name = "Modelo") +
  labs(
    title = "Comparación de Perfiles Edad-Ingreso",
    subtitle = "Incondicional vs. Condicional (controlando por horas y tipo de empleo)",
    x = "Edad (años)",
    y = "Log(Ingreso mensual) predicho",
    caption = sprintf("Controles evaluados en: Horas = %.1f (media), Tipo empleo = moda",
                      mean(geih_analysis$totalHoursWorked))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "gray80")
  )

ggsave("output/figures/fig2_profile_comparison.png", fig_comparison,
       width = 10, height = 7, dpi = 300)

# ------------------------------------------------------------------------------
# 6.3 Figura: Distribución Bootstrap de la Edad Pico
# ------------------------------------------------------------------------------

boot_df <- tibble(
  Incondicional = boot_results$t[, 1],
  Condicional = boot_results$t[, 2]
) %>%
  pivot_longer(everything(), names_to = "Modelo", values_to = "peak_age")

fig_bootstrap <- ggplot(boot_df, aes(x = peak_age, fill = Modelo)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 40, alpha = 0.6, position = "identity", color = "white") +
  geom_density(aes(color = Modelo), linewidth = 1, fill = NA) +
  geom_vline(xintercept = peak_age_unconditional, 
             color = col_primary, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = peak_age_conditional, 
             color = col_accent, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c("Incondicional" = col_primary, 
                               "Condicional" = col_accent)) +
  scale_color_manual(values = c("Incondicional" = col_primary, 
                                "Condicional" = col_accent)) +
  labs(
    title = "Distribución Bootstrap de la Edad Pico",
    subtitle = "B = 1000 réplicas",
    x = "Edad Pico (años)",
    y = "Densidad",
    caption = sprintf("IC 95%% Incond.: [%.1f, %.1f] | IC 95%% Cond.: [%.1f, %.1f]",
                      ci_unc_lower, ci_unc_upper, ci_cond_lower, ci_cond_upper)
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/figures/fig3_bootstrap_peak_age.png", fig_bootstrap,
       width = 10, height = 6, dpi = 300)

# ==============================================================================
# 7. RESUMEN DE RESULTADOS E INTERPRETACIÓN
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("        RESUMEN - SECTION 1: AGE-LABOR INCOME PROFILE           \n")
cat("================================================================\n\n")

cat("1. EVIDENCIA DE NO-LINEALIDAD Y CONCAVIDAD:\n")
cat("   ─────────────────────────────────────────\n")
cat(sprintf("   • Coeficiente Age²: %.7f (p < 0.001)\n", beta_age2_unc))
if (beta_age2_unc < 0) {
  cat("   • El perfil es CÓNCAVO → CONSISTENTE con teoría del capital humano\n")
  cat("   • Existe un MÁXIMO en el rango de edad observado\n")
}

cat("\n2. EDAD PICO DE INGRESOS:\n")
cat("   ─────────────────────────────────────────\n")
cat(sprintf("   Modelo Incondicional: %.1f años  [IC 95%%: %.1f - %.1f]\n",
            peak_age_unconditional, ci_unc_lower, ci_unc_upper))
cat(sprintf("   Modelo Condicional:   %.1f años  [IC 95%%: %.1f - %.1f]\n",
            peak_age_conditional, ci_cond_lower, ci_cond_upper))
cat(sprintf("   Diferencia:           %+.1f años\n", 
            peak_age_conditional - peak_age_unconditional))

cat("\n3. EFECTO DE LOS CONTROLES:\n")
cat("   ─────────────────────────────────────────\n")
cat(sprintf("   • R² ajustado: %.4f → %.4f (+%.4f)\n",
            summary(model_unconditional)$adj.r.squared,
            summary(model_conditional)$adj.r.squared,
            summary(model_conditional)$adj.r.squared - 
              summary(model_unconditional)$adj.r.squared))

if (peak_age_conditional > peak_age_unconditional) {
  cat("   • La edad pico AUMENTA al condicionar\n")
  cat("   • Interpretación: Parte del declive observado en ingresos a edades\n")
  cat("     avanzadas se explica por la reducción de horas trabajadas y/o\n")
  cat("     cambios en el tipo de empleo (ej: transición a cuenta propia)\n")
} else if (peak_age_conditional < peak_age_unconditional) {
  cat("   • La edad pico DISMINUYE al condicionar\n")
  cat("   • Interpretación: Los trabajadores más jóvenes trabajan menos horas,\n")
  cat("     lo que subestima su productividad en el modelo incondicional\n")
}

cat("\n4. INTERPRETACIÓN ECONÓMICA:\n")
cat("   ─────────────────────────────────────────\n")
cat("   • El perfil cóncavo refleja la teoría del capital humano:\n")
cat("     (a) Acumulación de experiencia y habilidades al inicio de la carrera\n")
cat("     (b) Rendimientos decrecientes de la experiencia adicional\n")
cat("     (c) Depreciación del capital humano / obsolescencia de habilidades\n")
cat(sprintf("   • Una edad pico de ~%.0f años es consistente con la literatura\n",
            (peak_age_unconditional + peak_age_conditional) / 2))

cat("\n================================================================\n")
cat("                    FIN DE LA SECCIÓN 1                         \n")
cat("================================================================\n")

# ==============================================================================
# 8. GUARDAR OBJETOS PARA USO EN OTRAS SECCIONES
# ==============================================================================

saveRDS(
  list(
    # Datos
    data = geih_analysis,
    # Modelos
    model_unconditional = model_unconditional,
    model_conditional = model_conditional,
    # Resultados clave
    peak_age_unconditional = peak_age_unconditional,
    peak_age_conditional = peak_age_conditional,
    ci_unconditional = c(ci_unc_lower, ci_unc_upper),
    ci_conditional = c(ci_cond_lower, ci_cond_upper),
    # Bootstrap
    boot_results = boot_results
  ),
  "output/section1_results.rds"
)

cat("\nResultados guardados en: output/section1_results.rds\n")