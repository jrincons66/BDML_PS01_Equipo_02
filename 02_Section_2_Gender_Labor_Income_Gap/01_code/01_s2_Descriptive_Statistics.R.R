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
  `Salario Mensual` + `Log Salario` + `Edad` + `Horas Trabajadas` ~
    Género * (N + Mean + SD + Min + Max),
  data    = geih_analysis,
  title   = "Estadísticas descriptivas por género",
  fmt     = function(x) format(round(x,2), big.mark = ".", decimal.mark = ",", scientific = FALSE),
  output  = "02_output/tables/01_Estadísticas_Descriptivas_Género.png"
)

ggplot(geih_analysis, aes(x = log_income, fill = factor(sex))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40) +
  labs(fill = "sex")
