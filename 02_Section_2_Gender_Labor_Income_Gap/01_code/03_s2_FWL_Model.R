#ANOTACION: Este codigo lo escribió Lucas Rodriguez

# ==============================================================================
# 2. ESTIMACIÓN DE MODELO INCONDICIONAL
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Modelo Condicional con Industrias
#     log(w) = β0 + β1female + β₂Age + β₃Age² + β4education + β5Hours + β6tenure 
# + β7industry + β8firm_size  + u
# ------------------------------------------------------------------------------

# Primera regresión FWL de female contra controles
geih_analysis <- geih_analysis %>% mutate(resid_female_con=lm(female~age+age2+educ+usual_hours+tenure+indus+firm_size,data=geih_analysis)$residuals)

# Segunda regresión FWL de log salario contra controles
geih_analysis<- geih_analysis %>% mutate(resid_salario_con=lm(log_income~age+age2+educ+usual_hours+tenure+indus+firm_size, data=geih_analysis)$residuals)

# Tercera regresión FWL de resid_salario contra resid_female para obtener β1
s2model_conditional_con <-lm(resid_salario_con~resid_female_con, geih_analysis)

# Extraemos coeficiente
beta_S2con <- coef(s2model_conditional_con)["resid_female_con"]
# ------------------------------------------------------------------------------
# 3.2 Modelo Condicional sin Industrias
#     log(w) = β0 + β1female + β₂Age + β₃Age² + β4education + β5Hours + β6tenure 
# + β7firm_size  + u
# ------------------------------------------------------------------------------

# Primera regresión FWL de female contra controles
geih_analysis <- geih_analysis %>% mutate(resid_female_sin=lm(female~age+age2+educ+usual_hours+tenure+firm_size,data=geih_analysis)$residuals)

# Segunda regresión FWL de log salario contra controles
geih_analysis<- geih_analysis %>% mutate(resid_salario_sin=lm(log_income~age+age2+educ+usual_hours+tenure+firm_size, data=geih_analysis)$residuals)

# Tercera regresión FWL de resid_salario contra resid_female para obtener β1
s2model_conditional_sin <-lm(resid_salario_sin~resid_female_sin, geih_analysis)

# Extraemos coeficiente
beta_S2sin <- coef(s2model_conditional_sin)["resid_female_sin"]

# ------------------------------------------------------------------------------
# 3.3 Calculo Wage Gap
# ------------------------------------------------------------------------------

salario_ref <- mean(geih_analysis$y_total_m[geih_analysis$female == 0], na.rm = TRUE)

# Diferencia en pesos para cada modelo
dif_uncond <- (exp(beta_S2unc) - 1) * salario_ref
dif_con    <- (exp(beta_S2con) - 1) * salario_ref
dif_sin    <- (exp(beta_S2sin) - 1) * salario_ref

stargazer(s2model_unconditional, s2model_conditional_con,s2model_conditional_sin, type="text", digits=7)
