################################################################################
##                                                                            ##
##                     VISUALIZACION DE LOS MODELOS                           ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

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

ggsave("02_output/figures/fig1_profile_unconditional.png", fig_unconditional,
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

ggsave("02_output/figures/fig2_profile_comparison.png", fig_comparison,
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

ggsave("02_output/figures/fig3_bootstrap_peak_age.png", fig_bootstrap,
       width = 10, height = 6, dpi = 300)
