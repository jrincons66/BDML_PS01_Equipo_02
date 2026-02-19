################################################################################
##                                                                            ##
##                        ESTIMACIÓN DE MODELOS                               ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

#=============================================================================
# 6. Gráfica y diferencia de edades pico por género
#=============================================================================

cat("\n")
cat("========================================================================\n")
cat("  6. GRAFICA Y DIFERENCIA DE EDADES PICO POR GENERO                    \n")
cat("========================================================================\n")
cat("\n")

# 1. ESTIMAR MODELO POR GRUPO----

cat("------------------------------------------------------------------------\n")
cat("  6.1 Estimamos los modelos por genero                                  \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

cat("  Estimando modelo para hombres...\n")
model_men   <- lm(log_income ~ age + age2 + educ + usual_hours + tenure + firm_size,
                  data = geih_analysis %>% filter(female == 0))

cat("  Observaciones:", nobs(model_men), "\n")
cat("  R2 ajustado:", round(summary(model_men)$adj.r.squared, 4), "\n")
cat("  Coef age:", round(coef(model_men)["age"], 6), "\n")
cat("  Coef age2:", round(coef(model_men)["age2"], 6), "\n")
cat("\n")

cat("  Estimando modelo para mujeres...\n")
model_women <- lm(log_income ~ age + age2 + educ + usual_hours + tenure + firm_size,
                  data = geih_analysis %>% filter(female == 1))

cat("  Observaciones:", nobs(model_women), "\n")
cat("  R2 ajustado:", round(summary(model_women)$adj.r.squared, 4), "\n")
cat("  Coef age:", round(coef(model_women)["age"], 6), "\n")
cat("  Coef age2:", round(coef(model_women)["age2"], 6), "\n")
cat("\n")


# 2. GRILLA DE PREDICCIÓN----

cat("------------------------------------------------------------------------\n")
cat("  6.2 Creando grilla de prediccion                                     \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

age_grid <- data.frame(
  age         = 18:65,
  age2        = (18:65)^2,
  educ        = factor(names(sort(table(geih_analysis$educ), decreasing = TRUE)[1]),
                       levels = levels(geih_analysis$educ)),
  usual_hours = mean(geih_analysis$usual_hours, na.rm = TRUE),
  tenure      = mean(geih_analysis$tenure,      na.rm = TRUE),
  firm_size   = factor(names(sort(table(geih_analysis$firm_size), decreasing = TRUE)[1]),
                       levels = levels(geih_analysis$firm_size))
)

cat("  Grilla de edad: 18 a 65 anos\n")
cat("  Educacion fijada en:", as.character(age_grid$educ[1]), "\n")
cat("  Horas usuales fijadas en:", round(age_grid$usual_hours[1], 2), "\n")
cat("  Tenure fijado en:", round(age_grid$tenure[1], 2), "\n")
cat("  Tamano firma fijado en:", as.character(age_grid$firm_size[1]), "\n")
cat("\n")

cat("  Generando predicciones para hombres...\n")
pred_men   <- predict(model_men,   newdata = age_grid, interval = "confidence")

cat("  Rango prediccion hombres: [", round(min(pred_men[,"fit"]), 4), ",", round(max(pred_men[,"fit"]), 4), "]\n")
cat("\n")

cat("  Generando predicciones para mujeres...\n")
pred_women <- predict(model_women, newdata = age_grid, interval = "confidence")

cat("  Rango prediccion mujeres: [", round(min(pred_women[,"fit"]), 4), ",", round(max(pred_women[,"fit"]), 4), "]\n")
cat("\n")

pred_df <- bind_rows(
  data.frame(age_grid, pred_men,   gender = "Men"),
  data.frame(age_grid, pred_women, gender = "Women")
)


# 3. EDAD PICO VÍA BOOTSTRAP----

cat("------------------------------------------------------------------------\n")
cat("  6.3 Calculando edad pico via Bootstrap (B = 1000)                    \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

peak_age <- function(model) {
  b1 <- coef(model)["age"]
  b2 <- coef(model)["age2"]
  -b1 / (2 * b2)
}

boot_peak <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(log_income ~ age + age2 + educ + usual_hours + tenure + firm_size, data = d)
  peak_age(m)
}

set.seed(123)

cat("  Bootstrap para hombres...\n")
boot_men   <- boot(geih_analysis %>% filter(female == 0), statistic = boot_peak, R = 1000)

cat("  Bootstrap para mujeres...\n")
boot_women <- boot(geih_analysis %>% filter(female == 1), statistic = boot_peak, R = 1000)

ci_men   <- boot.ci(boot_men,   type = "perc")$percent[4:5]
ci_women <- boot.ci(boot_women, type = "perc")$percent[4:5]

peak_df <- data.frame(
  gender   = c("Men", "Women"),
  peak     = c(peak_age(model_men), peak_age(model_women)),
  ci_lower = c(ci_men[1],  ci_women[1]),
  ci_upper = c(ci_men[2],  ci_women[2])
)

cat("\n")
cat("  Resultados edad pico:\n")
cat("    Hombres:", round(peak_df$peak[1], 2), "anos (IC 95%:", round(ci_men[1], 2), "-", round(ci_men[2], 2), ")\n")
cat("    Mujeres:", round(peak_df$peak[2], 2), "anos (IC 95%:", round(ci_women[1], 2), "-", round(ci_women[2], 2), ")\n")
cat("    Diferencia:", round(peak_df$peak[1] - peak_df$peak[2], 2), "anos\n")
cat("\n")

# 4. GRÁFICO----

cat("------------------------------------------------------------------------\n")
cat("  6.4 Generando grafico de perfiles                                    \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

plot_profiles <- ggplot(pred_df, aes(x = age, y = fit, color = gender, fill = gender)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_vline(data = peak_df, aes(xintercept = peak, color = gender),
             linetype = "dashed", linewidth = 0.8) +
  geom_rect(data = peak_df,
            aes(xmin = ci_lower, xmax = ci_upper, ymin = -Inf, ymax = Inf, fill = gender),
            alpha = 0.08, inherit.aes = FALSE) +
  # Etiquetas con el valor de la edad pico
  geom_label(data = peak_df,
             aes(x = peak, y     = ifelse(gender == "Men", 
                                          max(pred_df$upr) * 0.95, 
                                          max(pred_df$upr) * 0.85),
                 label = paste0("Peak: ", round(peak, 1)),
                 color = gender),
             fill  = "white",
             size  = 3.5,
             show.legend = FALSE) +
  scale_color_manual(values = c("Men" = "steelblue", "Women" = "coral")) +
  scale_fill_manual( values = c("Men" = "steelblue", "Women" = "coral")) +
  labs(
    title    = "Predicted Age-Income Profiles by Gender",
    subtitle = "Controls fixed at sample means | Dashed lines indicate peak age (shaded band = 95% CI)",
    x        = "Age",
    y        = "Predicted Log Monthly Income",
    color    = NULL,
    fill     = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# 5. GUARDAR----

cat("------------------------------------------------------------------------\n")
cat("  6.5 Guardando grafico                                                \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

ggsave("02_output/figures/01_age_profiles_by_gender.png",
       plot_profiles, width = 10, height = 6, dpi = 300)

cat("  Grafico guardado en: 02_output/figures/01_age_profiles_by_gender.png\n")
cat("\n")