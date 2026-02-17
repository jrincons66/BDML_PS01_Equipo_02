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
# 2. ESTIMACIÓN DE MODELO INCONDICIONAL
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Modelo Incondicional
#     log(w) = β0 + β1*Female + u
# ------------------------------------------------------------------------------
s2model_unconditional <- lm(log_income ~ female, data = geih_analysis)
summary(s2model_unconditional)

# Extraemos coeficientes -----

beta_S2unc <- coef(s2model_unconditional)["female"]


cat("\n=== MODELO INCONDICIONAL ===\n")
cat("Coef. female:   ", round(beta_S2Unc, 5), "\n")
cat("R² ajustado: ", round(summary(s2model_unconditional)$adj.r.squared, 4), "\n")
