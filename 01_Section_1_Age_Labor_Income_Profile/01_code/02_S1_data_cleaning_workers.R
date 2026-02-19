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
    usual_hours = as.numeric(hoursWorkUsual),
    relab = as.factor(relab),
    tenure = p6426,        
    educ = as.factor(maxEducLevel),      
    indus = as.factor(oficio),
    firm_size = as.factor(sizeFirm),
    chunk_id = as.numeric(chunk_id),
    formal = as.factor(formal),
    female = if_else(sex == 1, 0, 1) 
  ) %>%
  # Agregamos los criterios del Problem Set
  drop_na(y_total_m, totalHoursWorked, relab, sex, age, educ, usual_hours, tenure) %>%
  filter(
    age >= 18,
    y_total_m > 0,
    totalHoursWorked > 0,
    relab %in% c(1,2)
  ) %>%
  # Crear variables para el modelo
  mutate(
    log_income = log(y_total_m),
    age2 = age^2
  )%>%
  select(sex, 
         female,
         y_total_m,
         log_income,
         age,
         age2,
         tenure,
         educ,
         indus, 
         totalHoursWorked,
         usual_hours,
         relab,
         formal,
         firm_size,
         chunk_id,
         directorio
  ) %>%
  mutate(
    relab     = factor(relab,     levels = levels(relab)),
    educ      = factor(educ,      levels = levels(educ)),
    indus     = factor(indus,     levels = levels(indus)),
    firm_size = factor(firm_size, levels = levels(firm_size)),
    formal    = factor(formal,    levels = levels(formal))
  )


cat("Muestra de análisis:", nrow(geih_analysis), "observaciones\n")
cat("Aplicamos los siguientes criterios:1. Mayores de 18 , 2.Ingreso superior a 120000 (Solo tomamos empleados), 3. Horas positivas ")

# ------------------------------------------------------------------------------
# 2 Limpieza de datos anómalos para variable dependiente
# ------------------------------------------------------------------------------

up <- quantile(geih_analysis$y_total_m, 0.99, na.rm = T)
down <- quantile(geih_analysis$y_total_m, 0.01, na.rm = T)

# Eliminación de percentiles más extremos del ingreso

geih_analysis <- geih_analysis %>%
  filter(
    y_total_m >= quantile(y_total_m, 0.01, na.rm = TRUE),
    y_total_m <= quantile(y_total_m, 0.99, na.rm = TRUE)
  )

## Limpieza de environment

rm(list = setdiff(ls(), c("geih_raw", "geih_analysis","col_primary","col_secondary","col_accent")))