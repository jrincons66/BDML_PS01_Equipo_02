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
##   (chunks 8–10). Estos resultados constituyen el punto de referencia 
##   (baseline) contra el cual se compararán las especificaciones extendidas
##   del siguiente cuaderno.
##
##   Modelos re-estimados:
##     S1-M1: Incondicional edad          log(w) = β0 + β1·Age + β2·Age²
##     S1-M2: Condicional edad            log(w) = β0 + β1·Age + β2·Age² + β3·Hours + β4·Relab
##     S2-M1: Brecha incondicional        log(w) = β0 + β1·Female
##     S2-M2: Brecha condicional con ind. log(w) = β0 + β1·Female + controles + industria
##     S2-M3: Brecha condicional sin ind. log(w) = β0 + β1·Female + controles
##
##   IMPORTANTE: La estimación usa ÚNICAMENTE geih_train.
##               El RMSE se calcula ÚNICAMENTE sobre geih_validation.
##               Ningún dato de validación entra en la estimación.

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
# 1. FUNCIÓN AUXILIAR PARA CALCULAR RMSE
# ==============================================================================

calc_rmse <- function(model, newdata, outcome = "log_income") {
  pred <- predict(model, newdata = newdata)
  actual <- newdata[[outcome]]
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}

# ==============================================================================
# 2. SECCIÓN 1: PERFIL EDAD-INGRESO
# ==============================================================================

cat("--- Sección 1: Perfil Edad-Ingreso ---\n\n")

# ------------------------------------------------------------------------------
# S1-M1: Modelo Incondicional
#         log(w) = β0 + β1·Age + β2·Age²
# ------------------------------------------------------------------------------

s3_s1_m1 <- lm(log_income ~ age + age2, data = geih_train)

rmse_s1_m1 <- calc_rmse(s3_s1_m1, geih_validation)

cat(sprintf("S1-M1 Incondicional edad     | R² aj: %.4f | RMSE val: %.4f\n",
            summary(s3_s1_m1)$adj.r.squared, rmse_s1_m1))

# ------------------------------------------------------------------------------
# S1-M2: Modelo Condicional
#         log(w) = β0 + β1·Age + β2·Age² + β3·Hours + β4·Relab
# ------------------------------------------------------------------------------

s3_s1_m2 <- lm(log_income ~ age + age2 + totalHoursWorked + relab,
               data = geih_train)

rmse_s1_m2 <- calc_rmse(s3_s1_m2, geih_validation)

cat(sprintf("S1-M2 Condicional edad       | R² aj: %.4f | RMSE val: %.4f\n",
            summary(s3_s1_m2)$adj.r.squared, rmse_s1_m2))

# ==============================================================================
# 3. SECCIÓN 2: BRECHA DE GÉNERO
# ==============================================================================

cat("\n--- Sección 2: Brecha de Género ---\n\n")

# ------------------------------------------------------------------------------
# S2-M1: Modelo Incondicional género
#         log(w) = β0 + β1·Female
# ------------------------------------------------------------------------------

s3_s2_m1 <- lm(log_income ~ female, data = geih_train)

rmse_s2_m1 <- calc_rmse(s3_s2_m1, geih_validation)

cat(sprintf("S2-M1 Brecha incondicional   | R² aj: %.4f | RMSE val: %.4f\n",
            summary(s3_s2_m1)$adj.r.squared, rmse_s2_m1))

# ------------------------------------------------------------------------------
# S2-M2: Modelo Condicional con Industrias (especificación preferida con FWL)
#         log(w) = β0 + β1·Female + β2·Age + β3·Age² + β4·Educ + 
#                  β5·Hours + β6·Tenure + β7·Indus + β8·FirmSize
#
# Nota: Para predicción re-estimamos la forma reducida directamente (OLS),
#       que es algebraicamente equivalente al FWL pero permite usar predict().
# ------------------------------------------------------------------------------

s3_s2_m2 <- lm(log_income ~ female + age + age2 + educ + usual_hours +
                 tenure + indus + firm_size,
               data = geih_train)

rmse_s2_m2 <- calc_rmse(s3_s2_m2, geih_validation)

cat(sprintf("S2-M2 Brecha cond. con ind.  | R² aj: %.4f | RMSE val: %.4f\n",
            summary(s3_s2_m2)$adj.r.squared, rmse_s2_m2))

# ------------------------------------------------------------------------------
# S2-M3: Modelo Condicional sin Industrias
#         log(w) = β0 + β1·Female + β2·Age + β3·Age² + β4·Educ + 
#                  β5·Hours + β6·Tenure + β7·FirmSize
# ------------------------------------------------------------------------------

s3_s2_m3 <- lm(log_income ~ female + age + age2 + educ + usual_hours +
                 tenure + firm_size,
               data = geih_train)

rmse_s2_m3 <- calc_rmse(s3_s2_m3, geih_validation)

cat(sprintf("S2-M3 Brecha cond. sin ind.  | R² aj: %.4f | RMSE val: %.4f\n",
            summary(s3_s2_m3)$adj.r.squared, rmse_s2_m3))

# ==============================================================================
# 4. TABLA RESUMEN DE BASELINE
# ==============================================================================

cat("\n--- Tabla resumen baseline ---\n\n")

baseline_results <- data.frame(
  Modelo = c("S1-M1", "S1-M2", "S2-M1", "S2-M2", "S2-M3"),
  Descripcion = c(
    "Incondicional edad",
    "Condicional edad (hours + relab)",
    "Brecha incondicional",
    "Brecha condicional con industrias",
    "Brecha condicional sin industrias"
  ),
  R2_adj_train = round(c(
    summary(s3_s1_m1)$adj.r.squared,
    summary(s3_s1_m2)$adj.r.squared,
    summary(s3_s2_m1)$adj.r.squared,
    summary(s3_s2_m2)$adj.r.squared,
    summary(s3_s2_m3)$adj.r.squared
  ), 4),
  AIC_train = round(c(
    AIC(s3_s1_m1),
    AIC(s3_s1_m2),
    AIC(s3_s2_m1),
    AIC(s3_s2_m2),
    AIC(s3_s2_m3)
  ), 2),
  BIC_train = round(c(
    BIC(s3_s1_m1),
    BIC(s3_s1_m2),
    BIC(s3_s2_m1),
    BIC(s3_s2_m2),
    BIC(s3_s2_m3)
  ), 2),
  RMSE_validation = round(c(
    rmse_s1_m1,
    rmse_s1_m2,
    rmse_s2_m1,
    rmse_s2_m2,
    rmse_s2_m3
  ), 4)
)

print(baseline_results, row.names = FALSE)

# ==============================================================================
# 5. RESUMEN FINAL
# ==============================================================================

best_baseline <- baseline_results$Modelo[which.min(baseline_results$RMSE_validation)]
best_rmse     <- min(baseline_results$RMSE_validation)

cat("\n================================================================\n")
cat("   RESULTADOS BASELINE                                          \n")
cat("================================================================\n")
cat(sprintf("  Mejor modelo baseline:  %s\n", best_baseline))
cat(sprintf("  RMSE validación:        %.4f\n", best_rmse))
cat("\n  Objetos generados para scripts siguientes:\n")
cat("    s3_s1_m1, s3_s1_m2  → Modelos Sección 1\n")
cat("    s3_s2_m1, s3_s2_m2, s3_s2_m3 → Modelos Sección 2\n")
cat("    baseline_results     → Tabla resumen con R², AIC, BIC, RMSE\n")
cat("================================================================\n")