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
# 2. ESTIMACIÓN DE MODELO CONDICIONAL
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Modelo Condicional con Industrias
#     log(w) = β0 + β1female + β₂Age + β₃Age² + β4education + β5Hours + β6tenure 
# + β7industry + β8firm_size  + u
# ------------------------------------------------------------------------------

cat("------------------------------------------------------------------------\n")
cat("  3.1 Modelo Condicional CON Industrias (FWL)                          \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

# Primera regresión FWL de female contra controles
cat("  1: Regresion FWL de female contra controles...\n")
geih_analysis <- geih_analysis %>% mutate(resid_female_con=lm(female~age+age2+educ+usual_hours+tenure+indus+firm_size,data=geih_analysis)$residuals)

# Segunda regresión FWL de log salario contra controles
cat("  2: Regresion FWL de log salario contra controles...\n")
geih_analysis<- geih_analysis %>% mutate(resid_salario_con=lm(log_income~age+age2+educ+usual_hours+tenure+indus+firm_size, data=geih_analysis)$residuals)

# Tercera regresión FWL de resid_salario contra resid_female para obtener β1
cat("  3: Regresion de residuos para obtener b1...\n")
s2model_conditional_con <-lm(resid_salario_con~resid_female_con, geih_analysis)

# Extraemos coeficiente
beta_S2con <- coef(s2model_conditional_con)["resid_female_con"]

cat("\n")
cat("  Coeficiente beta_S2con:", beta_S2con, "\n")
cat("\n")

# ------------------------------------------------------------------------------
# 3.2 Modelo Condicional sin Industrias
#     log(w) = β0 + β1female + β₂Age + β₃Age² + β4education + β5Hours + β6tenure 
# + β7firm_size  + u
# ------------------------------------------------------------------------------

cat("------------------------------------------------------------------------\n")
cat("  3.2 Modelo Condicional SIN Industrias (FWL)                          \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

# Primera regresión FWL de female contra controles
cat("  1: Regresion FWL de female contra controles...\n")
geih_analysis <- geih_analysis %>% mutate(resid_female_sin=lm(female~age+age2+educ+usual_hours+tenure+firm_size,data=geih_analysis)$residuals)

# Segunda regresión FWL de log salario contra controles
cat("  2: Regresion FWL de log salario contra controles...\n")
geih_analysis<- geih_analysis %>% mutate(resid_salario_sin=lm(log_income~age+age2+educ+usual_hours+tenure+firm_size, data=geih_analysis)$residuals)

# Tercera regresión FWL de resid_salario contra resid_female para obtener β1
cat("  3: Regresion de residuos para obtener b1...\n")
s2model_conditional_sin <-lm(resid_salario_sin~resid_female_sin, geih_analysis)

# Extraemos coeficiente
beta_S2sin <- coef(s2model_conditional_sin)["resid_female_sin"]

cat("\n")
cat("  Coeficiente beta_S2sin:", beta_S2sin, "\n")
cat("\n")

# ------------------------------------------------------------------------------
# 3.3 Calculo Wage Gap
# ------------------------------------------------------------------------------

cat("------------------------------------------------------------------------\n")
cat("  3.3 Calculo Wage Gap en Pesos                                        \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

salario_ref <- mean(geih_analysis$y_total_m[geih_analysis$female == 0], na.rm = TRUE)

cat("  Salario de referencia (hombres):", salario_ref, "\n")
cat("\n")

# Diferencia en pesos para cada modelo
dif_uncond <- (exp(beta_S2unc) - 1) * salario_ref
dif_con    <- (exp(beta_S2con) - 1) * salario_ref
dif_sin    <- (exp(beta_S2sin) - 1) * salario_ref

cat("  Diferencia incondicional:", dif_uncond, "\n")
cat("  Diferencia con industrias:", dif_con, "\n")
cat("  Diferencia sin industrias:", dif_sin, "\n")
cat("\n")

cat("------------------------------------------------------------------------\n")
cat("  Tabla de Regresiones                                                 \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

stargazer(s2model_unconditional, s2model_conditional_indus,s2model_conditional_sin, type="text", digits=7)
