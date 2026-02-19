################################################################################
##                                                                            ##
##                        ESTIMACIÓN DE MODELOS                               ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

#=============================================================================
# 4. BOOTSTRAP PARA INTERVALOS DE CONFIANZA
#==============================================================================

cat("\n")
cat("========================================================================\n")
cat("  4. BOOTSTRAP PARA INTERVALOS DE CONFIANZA                            \n")
cat("========================================================================\n")
cat("\n")

cat("------------------------------------------------------------------------\n")
cat("  Definimos la funcion boot_FWL                                          \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

# Función para calcular coeficiente para cada bootstrap
boot_FWL <- function(data, indices) {
  d <- data[indices, ]
  d$female <- as.numeric(d$female)
  
  # Modelo incondicional
  
  model_unconditional_FWL <- lm(log_income ~ female, data = d)
  beta_uncond <- coef(model_unconditional_FWL)["female"]
  
  # Modelo condicional FWL con industrias
  
  d$resid_female_con=lm(female~age+age2+educ+usual_hours+tenure+indus+firm_size,data=d)$residuals
  
  d$resid_salario_con=lm(log_income~age+age2+educ+usual_hours+tenure+indus+firm_size+indus, data=d)$residuals
  model_conditional_con <- lm(resid_salario_con ~ resid_female_con, data = d)
  
  beta_cond_con <- coef(model_conditional_con)["resid_female_con"]
  
  # Modelo condicional FWL sin industrias
  
  d$resid_female_sin=lm(female~age+age2+educ+usual_hours+tenure+firm_size,data=d)$residuals
  
  d$resid_salario_sin=lm(log_income~age+age2+educ+usual_hours+tenure+firm_size, data=d)$residuals
  
  model_conditional_sin <- lm(resid_salario_sin ~ resid_female_sin, data = d)
  
  beta_cond_sin <- coef(model_conditional_sin)["resid_female_sin"]
  
  # Resultado
  return(c(
    beta_uncond = beta_uncond,
    beta_cond_con   = beta_cond_con,
    beta_cond_sin = beta_cond_sin
  ))
}

cat("  Funcion boot_FWL definida y procedemos a ejecutar\n")
cat("\n")

cat("------------------------------------------------------------------------\n")
cat("  Ejecutando Bootstrap (B = 1000 replicas)                             \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

# Ejecutar bootstrap (B = 1000 réplicas)

boot_results <- boot(data = geih_analysis, statistic = boot_FWL, R = 1000)

cat("------------------------------------------------------------------------\n")
cat("  Resultados: Errores Estandar Bootstrap                               \n")
cat("------------------------------------------------------------------------\n")
cat("\n")

# Obtenemos errores
se_uncond <- sd(boot_results$t[,1])
se_cond_con   <- sd(boot_results$t[,2])
se_cond_sin   <- sd(boot_results$t[,3])

cat("  SE Bootstrap (Incondicional):", round(se_uncond, 6), "\n")
cat("  SE Bootstrap (Condicional con industrias - FWL):", round(se_cond_con, 6), "\n")
cat("  SE Bootstrap (Condicional sin industrias - FWL):", round(se_cond_sin, 6), "\n")
cat("\n")

