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
    age3        = age^3,
    age4        = age^4,
    age_female  = age  * female,
    age2_female = age2 * female
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
  # E1: [ESPECIFICACIÓN 1]
  # Motivación económica: ...
  # ----------------------------------------------------------------------------
  E1 = list(
    formula    = log_income ~ age + age2 + age3,   # COMPLETAR
    label      = "Polinomio cúbico en edad",        # COMPLETAR
    motivacion = "..."                              # COMPLETAR
  ),
  
  # ----------------------------------------------------------------------------
  # E2: [ESPECIFICACIÓN 2]
  # Motivación económica: ...
  # ----------------------------------------------------------------------------
  E2 = list(
    formula    = log_income ~ age + age2 + age3 + age4,  # COMPLETAR
    label      = "Polinomio cuártico en edad",            # COMPLETAR
    motivacion = "..."                                    # COMPLETAR
  ),
  
  # ----------------------------------------------------------------------------
  # E3: [ESPECIFICACIÓN 3]
  # Motivación económica: ...
  # ----------------------------------------------------------------------------
  E3 = list(
    formula    = log_income ~ age + age2 + female + age_female + age2_female +
      educ + usual_hours + tenure + firm_size,  # COMPLETAR
    label      = "Perfiles edad-ingreso diferenciados por género",       # COMPLETAR
    motivacion = "..."                                                   # COMPLETAR
  ),
  
  # ----------------------------------------------------------------------------
  # E4: [ESPECIFICACIÓN 4]
  # Motivación económica: ...
  # ----------------------------------------------------------------------------
  E4 = list(
    formula    = log_income ~ age + age2,  # COMPLETAR
    label      = "...",                    # COMPLETAR
    motivacion = "..."                     # COMPLETAR
  ),
  
  # ----------------------------------------------------------------------------
  # E5: [ESPECIFICACIÓN 5]
  # Motivación económica: ...
  # ----------------------------------------------------------------------------
  E5 = list(
    formula    = log_income ~ age + age2,  # COMPLETAR
    label      = "...",                    # COMPLETAR
    motivacion = "..."                     # COMPLETAR
  )
  
  # ----------------------------------------------------------------------------
  # Agregar especificaciones adicionales aquí si se desea (E6, E7, ...)
  # ----------------------------------------------------------------------------
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