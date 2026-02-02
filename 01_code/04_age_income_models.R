################################################################################
##                                                                            ##
##                        ESTIMACIÓN DE MODELOS                               ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

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

# Verificmos la concavidad (β₃ < 0 implica perfil cóncavo)
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

# Comparación entre los modelos
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
          out = "02_output/tables/table1_age_income_profile.txt")


