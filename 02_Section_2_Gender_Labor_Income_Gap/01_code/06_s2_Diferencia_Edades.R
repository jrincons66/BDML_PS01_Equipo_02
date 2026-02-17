#=============================================================================
# 6. Gráfica y diferencia de edades pico por género
#=============================================================================



# 1. ESTIMAR MODELO POR GRUPO----

model_men   <- lm(log_income ~ age + age2 + educ + usual_hours + tenure + firm_size,
                  data = geih_analysis %>% filter(female == 0))

model_women <- lm(log_income ~ age + age2 + educ + usual_hours + tenure + firm_size,
                  data = geih_analysis %>% filter(female == 1))

# 2. GRILLA DE PREDICCIÓN----

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

pred_men   <- predict(model_men,   newdata = age_grid, interval = "confidence")
pred_women <- predict(model_women, newdata = age_grid, interval = "confidence")

pred_df <- bind_rows(
  data.frame(age_grid, pred_men,   gender = "Men"),
  data.frame(age_grid, pred_women, gender = "Women")
)

# 3. EDAD PICO VÍA BOOTSTRAP----

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
boot_men   <- boot(geih_analysis %>% filter(female == 0), statistic = boot_peak, R = 1000)
boot_women <- boot(geih_analysis %>% filter(female == 1), statistic = boot_peak, R = 1000)

ci_men   <- boot.ci(boot_men,   type = "perc")$percent[4:5]
ci_women <- boot.ci(boot_women, type = "perc")$percent[4:5]

peak_df <- data.frame(
  gender   = c("Men", "Women"),
  peak     = c(peak_age(model_men), peak_age(model_women)),
  ci_lower = c(ci_men[1],  ci_women[1]),
  ci_upper = c(ci_men[2],  ci_women[2])
)

# 4. GRÁFICO----

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

ggsave("02_output/figures/01_age_profiles_by_gender.png",
       plot_profiles, width = 10, height = 6, dpi = 300)