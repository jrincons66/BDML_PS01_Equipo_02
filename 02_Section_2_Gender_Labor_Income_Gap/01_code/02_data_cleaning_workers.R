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

# Criterios adicionales para la sección
# - relab incluye solo a 1, 2. Esto incluye solo a trabajadores asalariados y excluye a empresas unipersonales, patrones, no remeunerados, entre otros.
# - Se usa como variable objetivo log(p6500/p6800), que es la medición de salario base por hora trabajada, en lugar de la compensación total, para evitar bonos por rendimiento y subsidios.
# - No se toma variable rural/urbana ya que el dominio solo incluye respuestas de Bogotá
# - No se usan los weights entregados por el DANE ya que estos no necesariamente se van a trasladar bien a la muestra scrapeada

geih_analysis2 <- geih_raw %>%
  # Convertir a numérico si es necesario
  mutate(
    age = as.numeric(age),
    base_salary = as.numeric(p6500),
    usual_hours = as.numeric(hoursWorkUsual),
    relab = as.factor(relab),
    formal = as.factor(formal),
    chunk_id = as.numeric(chunk_id),
    firm_size = as.factor(sizeFirm)
  ) %>%
  # Agregamos los criterios del Problem Set
  filter(
    age >= 18,                    # Mayores de 18
    !is.na(base_salary),            # Ingreso no missing
    base_salary > 0,                # Ingreso positivo (empleados)
    !is.na(hoursWorkUsual),     # Horas no missing
    totalHoursWorked > 0,         # Horas positivas
    !is.na(relab),        # Tipo empleo no missing
    relab %in% c(1,2)  # Empleados de sector público y privado
  ) %>%
  # Crear variables para el modelo
  mutate(
    hwage = base_salary/usual_hours,
    log_hwage = log(hwage),  # Variable dependiente
    age2 = age^2,              # Término cuadrático
    work_exp = p6426,        # Experiencia dentro de la empresa
    educ = as.factor(maxEducLevel),      # Nivel educativo alcanzado más alto
    indus = as.factor(oficio),     # La industria a la que pertenece
  ) %>%
  select(sex, 
         hwage,
         log_hwage,
         age,
         age2,
         work_exp,
         educ,
         indus, 
         base_salary, 
         usual_hours,
         relab,
         formal,
         firm_size,
         chunk_id,
         directorio
  )

cat("Muestra de análisis:", nrow(geih_analysis2), "observaciones\n")
cat("Aplicamos los siguientes criterios:1. Mayores de 18 , 2.Ingreso positivo (empleados), 3. Horas positivas, 4. Empleados públicos y privados ")

