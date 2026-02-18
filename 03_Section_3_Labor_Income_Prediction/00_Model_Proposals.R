# =====================================
# Section 3 — Labor Income Prediction
# Model Proposals
# =====================================

# =======================================================
# Modelo 1 - Retornos a educación heterogéneos por género

log_income ~ age + age2 + female + educ + female:educ + usual_hours + tenure + firm_size

# Intuición
# La educación no “paga” igual para hombres y mujeres si hay segmentación ocupacional, discriminación en 
# promociones, o diferencias en campo de estudio/ocupación condicionadas a educación. La interacción 
# female:educ permite que el “premium” educativo sea distinto por género. 

# Mejora predicción por qué captura heterogeneidad sistemática en retornos, no solo un shift promedio.
# =======================================================

# ==============================================================================
# Modelo 2 — Perfil edad–ingreso distinto por género (ciclo de vida heterogéneo)

log_income ~ age + age2 + female + female:age + female:age2 + usual_hours + tenure + educ + firm_size 

# Intuición
# La brecha no tiene por qué ser constante en la vida laboral: puede abrirse con la edad (penalización 
# por maternidad, techos de cristal) o cerrarse. Interacciones con age y age2 permiten formas distintas 
# del perfil.

# Mejora predicción pues reducimos sesgo sistemático si el modelo base “promedia” dos perfiles diferentes.
# ==============================================================================

# =================================================================================
# Modelo 3 — No linealidad en experiencia (tenure²) y heterogeneidad por formalidad

# Tenemos que crear 
tenure2 = tenure^2

log_income ~ age + age2 + educ + usual_hours + tenure + tenure2 + formal + formal:tenure + firm_size (+ indus opcional)

# Intuición
# Tenure suele tener retornos decrecientes: al inicio se aprende pero luego se estabiliza. Además, en el sector formal,
# la experiencia puede traducirse más en aumentos (escalas salariales, promociones, negociación colectiva) que en el 
# informal.

# Mejoramos la predicción por que captura curvatura e interacción institucional (formalidad).
# =================================================================================

# =======================================================================================
# Modelo 4 — Estructura de horas: “retornos” distintos a horas usuales vs horas efectivas

log_income ~ age + age2 + educ + tenure + firm_size + indus + totalHoursWorked + hoursWorkUsual + totalHoursWorked:hoursWorkUsual

# En la base ya tenemso totalHoursWorked y hoursWorkUsual lo renombramos como usual_hours (lo puse en el modelo como está en el dicc)

# Intuición
# Dos personas pueden trabajar igual “usual”, pero una tuvo una semana atípica (o viceversa). La interacción permite captar que el 
# efecto de trabajar más horas en la semana previa depende del nivel usual (p.ej. horas extra vs jornada estándar).

# Mejoramos la predicción por que reduce error para observaciones con semanas atípicas, y captura mejor el componente variable 
# del ingreso.
# =======================================================================================

# ===========================================================================================
# Modelo 5 — Segmentación por tipo de ocupación/relación laboral (cuenta propia vs asalariado)

# Para este modelo vamos a incluir relab. 

# Para relab, la idea es entonces grupar en bloques económicos,
  # Asalariados formales
    # 1 Empresa privada
    # 2 Gobierno
  #Asalariados precarios
    # 3 Servicio doméstico
    # 8 Jornalero
  # Independientes
    # 4 Cuenta propia
    # 5 Empleador
  # Sin remuneración
    # 6 y 7
  # Otros
    # 9

# Con eso creamos entonces una variable que se llame relab_group
# Que sería más o menos así:

geih_analysis <- geih_analysis %>%
  mutate(
    # Asegurar que relab es numérico para comparar con 1:9
    relab_num = as.integer(as.character(relab)),
    
    relab_group = case_when(
      relab_num %in% c(1, 2) ~ "Salaried",     # privado + gobierno
      relab_num %in% c(3, 8) ~ "Precarious",   # doméstico + jornalero
      relab_num %in% c(4, 5) ~ "Independent",  # cuenta propia + empleador
      relab_num %in% c(6, 7) ~ "Unpaid",       # sin remuneración
      relab_num == 9         ~ "Other",
      TRUE                   ~ NA_character_
    ),
    
    relab_group = factor(
      relab_group,
      levels = c("Salaried", "Independent", "Precarious", "Unpaid", "Other")
    )
  ) %>%

# Y el modelo quedaría 

  log_income ~ age + age2 + educ + tenure + usual_hours +
    relab_group +
    relab_group:educ +
    relab_group:usual_hours,
  data = geih_analysis

# Intuición
# Aquí la variable relab no es un simple control, sino que segmentos estructurales del mercado laboral.
# Entonces cada segmento difiere en Estos segmentos difieren en mecanismos de fijación salarial, estabilidad 
# del empleo, productividad marginal del trabajo y acceso a capital y negociación. 

# En este modelo permitimos que los retornos a educación sean distintos que el efecto de las horas sea distinto
# que la estructura de ingresos varíe por segmento. O sea que no solo cambia el intercepto sino que también 
# cambia la pendiente de la función de ingresos. 

