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
# 2. ESTIMACIÓN DE MODELO INCONDICIONAL
# ==============================================================================

# --- 1. Construir la tabla de estadísticas por género ---

geih_analysis <- geih_analysis %>%
  mutate(
    `Log Salario`        = log_income,
    `Salario Mensual`    = y_total_m,
    `Edad`               = age,
    `Horas Trabajadas`   = totalHoursWorked,
    Género             = if_else(female == 1, "Mujer", "Hombre")
  )

datasummary(
  `Salario Mensual` + `Log Salario` + `Edad` + `Horas Trabajadas`+ educ ~
    Género * (N + Mean + SD + Min + Max),
  data    = geih_analysis,
  title   = "Estadísticas descriptivas por género",
  fmt     = function(x) format(round(x,2), big.mark = ".", decimal.mark = ",", scientific = FALSE),
  output  = "02_output/tables/01_Estadísticas_Descriptivas_Género.png"
)

## histograma de log ingreso separado por género
hist_log <- ggplot(geih_analysis,
                   aes(x = log_income,
                       fill = factor(sex))) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = 40) +
  labs(title = "Distribución del log ingreso por sexo",
       x = "Log ingreso mensual",
       y = "Frecuencia",
       fill = "Sexo") +
  theme_minimal()
ggsave(
  filename = "02_output/figures/01_Histograma_Log_Ingreso_Sexo.png",
  plot = hist_log,
  width = 8,
  height = 6,
  dpi = 300
)

## grafica de deciles de ingreso por genero
percentiles_plot <- geih_analysis %>%
  group_by(sex) %>%
  summarise(
    P10 = quantile(y_total_m, 0.10, na.rm = TRUE),
    P20 = quantile(y_total_m, 0.20, na.rm = TRUE),
    P30 = quantile(y_total_m, 0.30, na.rm = TRUE),
    P40 = quantile(y_total_m, 0.40, na.rm = TRUE),
    P50 = quantile(y_total_m, 0.50, na.rm = TRUE),
    P60 = quantile(y_total_m, 0.60, na.rm = TRUE),
    P70 = quantile(y_total_m, 0.70, na.rm = TRUE),
    P80 = quantile(y_total_m, 0.80, na.rm = TRUE),
    P90 = quantile(y_total_m, 0.90, na.rm = TRUE)
  ) %>%
  pivot_longer(-sex,
               names_to = "Percentil",
               values_to = "Ingreso")

deciles_fig <- ggplot(percentiles_plot,
                      aes(x = Percentil,
                          y = Ingreso,
                          group = factor(sex),
                          color = factor(sex))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Deciles del ingreso mensual por sexo",
       x = "Percentil",
       y = "Ingreso mensual",
       color = "Sexo") +
  theme_minimal()
ggsave(
  filename = "02_output/figures/02_Deciles_Ingreso_Sexo.png",
  plot = deciles_fig,
  width = 8,
  height = 6,
  dpi = 300
)
