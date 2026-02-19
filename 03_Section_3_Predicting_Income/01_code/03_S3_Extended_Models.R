################################################################################
##                                                                            ##
##                   MODELOS EXTENDIDOS - SECCIÓN 3                           ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

## Descripción:
##   Estima al menos 5 especificaciones adicionales diseñadas para mejorar
##   la predicción out-of-sample respecto a los modelos baseline.
##
##   Las especificaciones deben estar económicamente motivadas y construirse
##   sobre la estructura de las ecuaciones de ingreso de las secciones
##   anteriores. Posibles extensiones:
##     - No linealidades adicionales (ej: polinomios de orden mayor en edad)
##     - Interacciones entre variables (ej: age × female, age × educ)
##     - Controles adicionales justificados por teoría económica
##

# ==============================================================================
# 0. VERIFICACIÓN DE PREREQUISITOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("   SECTION 3: MODELOS EXTENDIDOS                                \n")
cat("================================================================\n\n")

if (!exists("geih_train") | !exists("geih_validation")) {
  stop("ERROR: Los objetos 'geih_train' y/o 'geih_validation' no existen.
       Asegúrese de haber corrido 01_s3_Train_Test_Split.R primero.")
}

if (!exists("baseline_results")) {
  stop("ERROR: El objeto 'baseline_results' no existe.
       Asegúrese de haber corrido 02_s3_Baseline_Models.R primero.")
}

cat(sprintf("Training set:    %s observaciones (chunks 1-7)\n",
            format(nrow(geih_train), big.mark = ",")))
cat(sprintf("Validation set:  %s observaciones (chunks 8-10)\n\n",
            format(nrow(geih_validation), big.mark = ",")))

# ==============================================================================
# 1. VARIABLES AUXILIARES PARA ESPECIFICACIONES EXTENDIDAS
# ==============================================================================
# Si alguna especificación requiere variables que no están en geih_analysis
# (ej: polinomios de orden 3, interacciones), créalas aquí antes del loop
# para que estén disponibles tanto en training como en validation.

new_vars <- function(df) {
  df %>% mutate(
    tenure2 = tenure^2
  )
}

for (df_name in c("geih_train", "geih_validation")) {
  assign(df_name, new_vars(get(df_name)))
}

# ==============================================================================
# 2. DEFINICIÓN DE ESPECIFICACIONES EXTENDIDAS
# ==============================================================================
# Estructura de cada elemento:
#   - formula:     fórmula del modelo (COMPLETAR)
#   - label:       descripción corta para tablas (COMPLETAR)
#   - motivacion:  justificación económica (COMPLETAR)

extended_specs <- list(

  # ----------------------------------------------------------------------------
  # E1: Retornos a educación heterogéneos por género
  #
  # Motivación económica: La educación no “paga” igual para hombres y mujeres si hay
  #   segmentación ocupacional, discriminación en promociones, o diferencias en campo
  #   de estudio/ocupación condicionadas a educación. La interacción female:educ permite
  #   que el “premium” educativo sea distinto por género, capturando heterogeneidad
  #   sistemática en retornos y no solo un shift promedio.
  # ----------------------------------------------------------------------------
  E1 = list(
    formula    = log_income ~ age + age2 + female + educ + female:educ +
      usual_hours + tenure + firm_size + indus,
    label      = "Retornos a educación por género (female × educ)",
    motivacion = "Permite que los retornos a la educación difieran por género 
    (female × educ), capturando segmentación ocupacional y posibles diferencias en
    promociones o tipos de ocupación. Esto introduce heterogeneidad en retornos 
    y puede mejorar la predicción al no imponer un efecto uniforme de educ para 
    hombres y mujeres."
  ),

  # ----------------------------------------------------------------------------
  # E2: Perfil edad–ingreso distinto por género (ciclo de vida heterogéneo)
  #
  # Motivación económica: La brecha salarial no tiene por qué ser constante a lo
  #   largo de la vida laboral. Puede ampliarse con la edad debido a interrupciones
  #   laborales, penalizaciones por maternidad o techos de cristal, o reducirse en
  #   etapas tempranas de la carrera. Las interacciones female×age y female×age2
  #   permiten que la forma del perfil edad–ingreso difiera por género.
  # ----------------------------------------------------------------------------
  E2 = list(
    formula    = log_income ~ age + age2 + female + female:age + female:age2 +
      usual_hours + tenure + educ + firm_size + indus,
    label      = "Perfil edad distinto por género (female × age, age²)",
    motivacion = "Permite que el perfil del ciclo de vida difiera por género, 
    capturando posibles divergencias en trayectorias laborales. Esto reduce sesgos 
    derivados de promediar perfiles distintos y puede mejorar la predicción al 
    permitir pendientes diferenciadas en edad."
  ),

  # ----------------------------------------------------------------------------
  # E3: No linealidad en experiencia (tenure²) y heterogeneidad por formalidad
  #
  # Motivación económica: Los retornos a la experiencia en el empleo suelen ser
  #   decrecientes: al inicio se acumula capital humano específico y el salario
  #   aumenta rápidamente, pero luego el crecimiento se estabiliza. Además, en 
  #   el sector formal la experiencia puede traducirse más en aumentos salariales
  #   (escalas, promociones, negociación colectiva) que en el informal. La inclusión
  #   de tenure² captura la curvatura, mientras que formal × tenure permite que la
  #   pendiente difiera según el régimen institucional.
  # ----------------------------------------------------------------------------
  E3 = list(
    formula    = log_income ~ age + age2 + educ + usual_hours + tenure + tenure2 +
      formal + formal:tenure + firm_size + indus,
    label      = "Tenure no lineal y heterogeneidad por formalidad",
    motivacion = "Introduce rendimientos decrecientes a la experiencia mediante 
    tenure² y permite que el efecto de la experiencia difiera entre el sector formal 
    e informal. Esto captura curvatura y diferencias institucionales en la formación 
    de ingresos, mejorando la capacidad predictiva."
  ),

  # ----------------------------------------------------------------------------
  # E4: Heterogeneidad del gap de género por industria
  #
  # Motivación económica: La brecha salarial de género no es homogénea entre
  #   sectores productivos. Diferencias en composición ocupacional, normas
  #   institucionales, cultura organizacional, niveles de formalización y
  #   mecanismos de promoción pueden hacer que el efecto del género sobre el
  #   ingreso sea más pronunciado en algunas industrias que en otras. La
  #   interacción indus×female permite que el diferencial salarial asociado al
  #   género varíe por sector, capturando segmentación horizontal del mercado
  #   laboral y posibles diferencias en discriminación o retornos a habilidades.
  # ----------------------------------------------------------------------------
  E4 = list(
    formula    = log_income ~ age + age2 + educ + tenure + firm_size +
      indus + indus:female + usual_hours,
    label      = "Industria × género (heterogeneidad sectorial del gap)",
    motivacion = "Permite que el efecto del género sobre el ingreso difiera entre 
    industrias, capturando segmentación sectorial, diferencias en estructuras 
    salariales y posibles variaciones en normas laborales o promoción interna. 
    Esto introduce heterogeneidad estructural que puede reducir errores sistemáticos 
    de predicción."
  ),
  
  # ----------------------------------------------------------------------------
  # E5: Kitchen sink (interacciones de segundo orden)
  # Motivación económica: Esta especificación es más flexible, y permite 
  #   complementariedades entre capital humano (educación), características laborales 
  #   (horas, formalidad), y segmentación (industria, tamaño de firma). Al incluir 
  #   todas las interacciones de segundo orden se reduce el riesgo de error de 
  #   especificación funcional y se aproxima mejor una función de ingresos f(X) en un 
  #   contexto donde los retornos pueden depender del sector y del tipo de empleo. 
  #   Sacrifica interpretabilidad, pero es apropiado cuando el objetivo principal es 
  #   desempeño predictivo out-of-sample.
  # ----------------------------------------------------------------------------
  E5 = list(
    formula    = log_income ~ (female + firm_size + educ + indus + formal + age + usual_hours)^2,
    label      = "Kitchen sink (2nd-order interactions)",
    motivacion = "Permite efectos cruzados entre capital humano, formalidad y segmentación por 
    industria/tamaño de firma. Esta flexibilidad reduce el riesgo de misspecification y puede 
    mejorar RMSE out-of-sample, aunque con menor interpretabilidad y potencial riesgo de 
    sobreajuste."
  )

)

# ==============================================================================
# 3. FUNCIÓN AUXILIAR PARA CALCULAR RMSE
# ==============================================================================

calc_rmse <- function(model, newdata, outcome = "log_income") {
  pred   <- predict(model, newdata = newdata)
  actual <- newdata[[outcome]]
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}

# ==============================================================================
# 4. LOOP DE ESTIMACIÓN Y EVALUACIÓN
# ==============================================================================

cat("--- Estimando modelos extendidos ---\n\n")

extended_models_list  <- list()   # Guarda los objetos lm()
extended_results      <- list()   # Guarda las métricas de cada modelo

for (nombre in names(extended_specs)) {
  
  spec <- extended_specs[[nombre]]
  
  # Estimación sobre training
  modelo <- lm(spec$formula, data = geih_train)
  
  # Métricas
  r2_adj <- summary(modelo)$adj.r.squared
  aic    <- AIC(modelo)
  bic    <- BIC(modelo)
  rmse   <- calc_rmse(modelo, geih_validation)
  
  # Guardar modelo
  extended_models_list[[nombre]] <- modelo
  
  # Guardar métricas
  extended_results[[nombre]] <- data.frame(
    Modelo      = nombre,
    Descripcion = spec$label,
    Seccion     = "S3",
    R2_adj      = round(r2_adj, 4),
    AIC         = round(aic, 2),
    BIC         = round(bic, 2),
    RMSE_val    = round(rmse, 4)
  )
  
  cat(sprintf("  %-6s | R² aj: %.4f | AIC: %10.2f | BIC: %10.2f | RMSE val: %.4f | %s\n",
              nombre, r2_adj, aic, bic, rmse, spec$label))
}

# Consolidar resultados en un único data frame
extended_results <- do.call(rbind, extended_results)
rownames(extended_results) <- NULL

# ==============================================================================
# 5. TABLA COMPARATIVA: BASELINE VS EXTENDIDOS
# ==============================================================================

cat("\n--- Tabla comparativa: baseline vs. extendidos ---\n\n")

all_results <- rbind(baseline_results, extended_results)
print(all_results[order(all_results$RMSE_val), ], row.names = FALSE)

# ==============================================================================
# 6. RESUMEN
# ==============================================================================

best_extended <- extended_results$Modelo[which.min(extended_results$RMSE_val)]
best_rmse_ext <- min(extended_results$RMSE_val)
best_overall  <- all_results$Modelo[which.min(all_results$RMSE_val)]

cat("\n================================================================\n")
cat("   RESULTADOS MODELOS EXTENDIDOS                                \n")
cat("================================================================\n")
cat(sprintf("  Mejor modelo extendido: %s (RMSE: %.4f)\n", best_extended, best_rmse_ext))
cat(sprintf("  Mejor modelo overall:   %s (RMSE: %.4f)\n",
            best_overall, min(all_results$RMSE_val)))
cat("\n  Objetos generados para scripts siguientes:\n")
cat("    extended_models_list → Lista con todos los objetos lm() extendidos\n")
cat("    extended_results     → Data frame con R², AIC, BIC y RMSE\n")
cat("    all_results          → Tabla unificada baseline + extendidos\n")
cat("================================================================\n")
