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
stargazer(stats_by_age)

# El patrón de ∩ invertida en mean_log_income motiva la especificación cuadrática

# Visualización del patrón ∩ invertida con los promedios pra evitar ruido. 

patron_invertido <- geih_analysis %>%
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

plot(patron_invertido)
# ------------------------------------------------------------------------------
# 2.3 Distribución de tipo de empleo (relab)
# ------------------------------------------------------------------------------

table(geih_analysis$relab)
