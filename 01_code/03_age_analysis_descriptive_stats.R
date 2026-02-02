################################################################################
##                                                                            ##
##                     ESTADÍSTICAS DESCRIPTIVAS                              ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

# ==============================================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS
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
stargazer(stats_section1)

cat("┌──────────────────────────────────────────────────────────────────────┐\n")
cat("│                    RESUMEN DE LA MUESTRA                            │\n")
cat("├──────────────────────────────────────────────────────────────────────┤\n")
cat(sprintf("│  Observaciones:            %s                              │\n", 
            format(stats_section1$N, big.mark = ",")))
cat("├──────────────────────────────────────────────────────────────────────┤\n")
cat("│  EDAD                                                               │\n")
cat(sprintf("│    Media (SD):             %.1f (%.1f) años                        │\n", 
            stats_section1$Edad_Media, stats_section1$Edad_SD))
cat(sprintf("│    Rango:                  [%d, %d] años                           │\n", 
            stats_section1$Edad_Min, stats_section1$Edad_Max))
cat("├──────────────────────────────────────────────────────────────────────┤\n")
cat("│  INGRESO MENSUAL                                                    │\n")
cat(sprintf("│    Media:                  $%s                           │\n", 
            format(round(stats_section1$Ingreso_Media), big.mark = ",")))
cat(sprintf("│    Mediana:                $%s                           │\n", 
            format(round(stats_section1$Ingreso_Mediana), big.mark = ",")))
cat(sprintf("│    SD:                     $%s                           │\n", 
            format(round(stats_section1$Ingreso_SD), big.mark = ",")))
cat("├──────────────────────────────────────────────────────────────────────┤\n")
cat("│  LOG-INGRESO                                                        │\n")
cat(sprintf("│    Media (SD):             %.3f (%.3f)                            │\n", 
            stats_section1$LogIngreso_Media, stats_section1$LogIngreso_SD))
cat("├──────────────────────────────────────────────────────────────────────┤\n")
cat("│  HORAS TRABAJADAS (semana)                                          │\n")
cat(sprintf("│    Media (SD):             %.1f (%.1f) horas                       │\n", 
            stats_section1$Horas_Media, stats_section1$Horas_SD))
cat("└──────────────────────────────────────────────────────────────────────┘\n")

# ------------------------------------------------------------------------------
# 2.2 Estadísticas por grupo de edad (motivación para modelo cuadrático)
# ------------------------------------------------------------------------------

cat("\n")
cat("┌──────────────────────────────────────────────────────────────────────┐\n")
cat("│           ESTADÍSTICAS POR GRUPO DE EDAD                            │\n")
cat("│     (Evidencia del patrón ∩ invertida en log-ingreso)               │\n")
cat("└──────────────────────────────────────────────────────────────────────┘\n")
cat("\n")

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

cat("  ┌─────────┬──────────┬───────────┬────────────┬──────────┬─────────┐\n")
cat("  │  Grupo  │     N    │  % Muestra│ Log(Y) Med │ Log(Y) SD│  Horas  │\n")
cat("  ├─────────┼──────────┼───────────┼────────────┼──────────┼─────────┤\n")

for (i in 1:nrow(stats_by_age)) {
  cat(sprintf("  │  %-6s │ %8s │   %5.1f%%  │   %6.3f   │  %6.3f  │  %5.1f  │\n",
              stats_by_age$age_group[i],
              format(stats_by_age$n[i], big.mark = ","),
              stats_by_age$pct[i],
              stats_by_age$mean_log_income[i],
              stats_by_age$sd_log_income[i],
              stats_by_age$mean_hours[i]))
}

cat("  └─────────┴──────────┴───────────┴────────────┴──────────┴─────────┘\n")

cat("\n")
cat("  NOTA: El patrón de ∩ invertida en Log(Y) Media motiva la especificación\n")
cat("        cuadrática: log(w) = β₁ + β₂·Age + β₃·Age² + u\n")
cat("\n")

# Visualización del patrón ∩ invertida con los promedios para evitar ruido
cat("  Generando gráfico del patrón ∩ invertida...\n")

patron_invertido <- geih_analysis %>%
  group_by(age) %>%
  summarise(mean_income = mean(log_income, na.rm = TRUE)) %>%
  ggplot(aes(age, mean_income)) +
  geom_line(color = "#1E3A5F", linewidth = 0.8) +
  geom_point(color = "#C41E3A", size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = TRUE, color = "#4A90A4", linetype = "dashed") +
  labs(
    title = "Patrón ∩ Invertida: Log-Ingreso Promedio por Edad",
    subtitle = "Evidencia que motiva la especificación cuadrática",
    x = "Edad (años)",
    y = "Log(Ingreso) promedio",
    caption = "Línea punteada: ajuste cuadrático"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

plot(patron_invertido)
ggsave("02_output/figures/section1_patron_invertido.png", patron_invertido,
       width = 10, height = 6, dpi = 300, bg = "white")

# ------------------------------------------------------------------------------
# 2.3 Distribución de tipo de empleo (relab)
# ------------------------------------------------------------------------------

cat("\n")
cat("┌──────────────────────────────────────────────────────────────────────┐\n")
cat("│              DISTRIBUCIÓN POR TIPO DE EMPLEO (relab)                │\n")
cat("└──────────────────────────────────────────────────────────────────────┘\n")
cat("\n")

relab_dist <- geih_analysis %>%
  mutate(
    relab_label = case_when(
      relab == 1 ~ "1. Empleado particular",
      relab == 2 ~ "2. Empleado gobierno",
      relab == 3 ~ "3. Empleado doméstico ",
      relab == 4 ~ "4. Cuenta propia",
      relab == 5 ~ "5. Patrón/Empleador ",
      relab == 6 ~ "6. Trab. familiar",
      relab == 7 ~ "7. Trab. sin remun.",
      relab == 8 ~ "8. Jornalero.",
      TRUE ~ "Otro"
    )
  ) %>%
  count(relab_label, name = "n") %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(n))

cat("  ┌────────────────────────────┬──────────┬───────────┐\n")
cat("  │       Tipo de Empleo       │     N    │  % Muestra│\n")
cat("  ├────────────────────────────┼──────────┼───────────┤\n")

for (i in 1:nrow(relab_dist)) {
  cat(sprintf("  │  %-25s │ %8s │   %5.1f%%  │\n",
              relab_dist$relab_label[i],
              format(relab_dist$n[i], big.mark = ","),
              relab_dist$pct[i]))
}

cat("  └────────────────────────────┴──────────┴───────────┘\n")

cat("\n")
cat("══════════════════════════════════════════════════════════════════════\n")
cat("                  FIN DE ESTADÍSTICAS DESCRIPTIVAS                    \n")
cat("══════════════════════════════════════════════════════════════════════\n")
