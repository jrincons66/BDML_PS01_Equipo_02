################################################################################
##                                                                            ##
##                     LIMPIEZA DE BASE DE DATOS                              ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

# ------------------------------------------------------------------------------
# 1 Construcción de la muestra de análisis
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
  # Agregamos los criterios del Problem Set
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
cat("Aplicamos los siguientes criterios:1. Mayores de 18 , 2.Ingreso positivo (empleados), 3. Horas positivas ")

