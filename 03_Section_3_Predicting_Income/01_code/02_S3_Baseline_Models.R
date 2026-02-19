################################################################################
##                                                                            ##
##                     MODELOS BASELINE - SECCIÓN 3                           ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

## Descripción:
##   Re-estima sobre el TRAINING SET (chunks 1–7) todas las especificaciones
##   de las Secciones 1 y 2, y calcula el RMSE sobre el VALIDATION SET
##   (chunks 8–10). Estos resultados son el punto de referencia contra el cual
##   se compararán las especificaciones extendidas.
##
##   Las especificaciones se definen al inicio como una lista de fórmulas.
##   Un loop ejecuta la estimación y el cálculo de métricas para todas ellas,
##   evitando código repetitivo y facilitando la extensión en el siguiente
##   cuaderno.

# ==============================================================================
# 0. VERIFICACIÓN DE PREREQUISITOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("   SECTION 3: MODELOS BASELINE                                  \n")
cat("================================================================\n\n")

if (!exists("geih_train") | !exists("geih_validation")) {
  stop("ERROR: Los objetos 'geih_train' y/o 'geih_validation' no existen.
       Asegúrese de haber corrido 01_s3_Train_Test_Split.R primero.")
}

cat(sprintf("Training set:    %s observaciones (chunks 1-7)\n",
            format(nrow(geih_train), big.mark = ",")))
cat(sprintf("Validation set:  %s observaciones (chunks 8-10)\n\n",
            format(nrow(geih_validation), big.mark = ",")))

# ==============================================================================
# 1. DEFINICIÓN DE ESPECIFICACIONES BASELINE
# ==============================================================================
# Cada elemento de la lista define una especificación con:
#   - formula:     fórmula del modelo
#   - label:       descripción para tablas y gráficos
#   - seccion:     sección de origen (S1 o S2)

baseline_specs <- list(
  
  S1_M1 = list(
    formula = log_income ~ age + age2,
    label   = "Incondicional edad",
    seccion = "S1"
  ),
  
  S1_M2 = list(
    formula = log_income ~ age + age2 + totalHoursWorked + relab,
    label   = "Condicional edad (hours + relab)",
    seccion = "S1"
  ),
  
  S2_M1 = list(
    formula = log_income ~ female,
    label   = "Brecha incondicional",
    seccion = "S2"
  ),
  
  # Nota: S2_M2 y S2_M3 se estimaron en Sección 2 via FWL para obtener el
  # coeficiente de female. Aquí se re-estiman como OLS directo (algebraicamente
  # equivalente) para poder usar predict() sobre el validation set.
  
  S2_M2 = list(
    formula = log_income ~ female + age + age2 + educ + usual_hours +
      tenure + indus + firm_size,
    label   = "Brecha condicional con industrias",
    seccion = "S2"
  ),
  
  S2_M3 = list(
    formula = log_income ~ female + age + age2 + educ + usual_hours +
      tenure + firm_size,
    label   = "Brecha condicional sin industrias",
    seccion = "S2"
  )
)

# ==============================================================================
# 2. FUNCIÓN AUXILIAR PARA CALCULAR RMSE
# ==============================================================================

calc_rmse <- function(model, newdata, outcome = "log_income") {
  pred   <- predict(model, newdata = newdata)
  actual <- newdata[[outcome]]
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}

# ==============================================================================
# 3. LOOP DE ESTIMACIÓN Y EVALUACIÓN
# ==============================================================================

cat("--- Estimando modelos baseline ---\n\n")

models_list      <- list()   # Guarda los objetos lm()
baseline_results <- list()   # Guarda las métricas de cada modelo

for (nombre in names(baseline_specs)) {
  
  spec <- baseline_specs[[nombre]]
  
  # Estimación sobre training
  modelo <- lm(spec$formula, data = geih_train)
  
  # Métricas
  r2_adj <- summary(modelo)$adj.r.squared
  aic    <- AIC(modelo)
  bic    <- BIC(modelo)
  rmse   <- calc_rmse(modelo, geih_validation)
  
  # Guardar modelo
  models_list[[nombre]] <- modelo
  
  # Guardar métricas
  baseline_results[[nombre]] <- data.frame(
    Modelo      = nombre,
    Descripcion = spec$label,
    Seccion     = spec$seccion,
    R2_adj      = round(r2_adj, 4),
    AIC         = round(aic, 2),
    BIC         = round(bic, 2),
    RMSE_val    = round(rmse, 4)
  )
  
  cat(sprintf("  %-6s | R² aj: %.4f | AIC: %10.2f | BIC: %10.2f | RMSE val: %.4f | %s\n",
              nombre, r2_adj, aic, bic, rmse, spec$label))
}

# Consolidar resultados en un único data frame
baseline_results <- do.call(rbind, baseline_results)
rownames(baseline_results) <- NULL

# ==============================================================================
# 4. RESUMEN
# ==============================================================================

cat("\n--- Tabla resumen baseline ---\n\n")
print(baseline_results, row.names = FALSE)

best_baseline <- baseline_results$Modelo[which.min(baseline_results$RMSE_val)]
best_rmse     <- min(baseline_results$RMSE_val)

cat("\n================================================================\n")
cat("   RESULTADOS BASELINE                                          \n")
cat("================================================================\n")
cat(sprintf("  Mejor modelo baseline:  %s\n", best_baseline))
cat(sprintf("  RMSE validación:        %.4f\n", best_rmse))
cat("\n  Objetos generados para scripts siguientes:\n")
cat("    models_list        → Lista con todos los objetos lm() baseline\n")
cat("    baseline_results   → Data frame con R², AIC, BIC y RMSE\n")
cat("================================================================\n")