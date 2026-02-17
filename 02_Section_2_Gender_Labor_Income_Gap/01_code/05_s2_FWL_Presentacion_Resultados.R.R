#=============================================================================
# 5. Tabla de resultados
#==============================================================================

tabla_gap <- stargazer(
  s2model_unconditional,
  s2model_conditional_con,
  s2model_conditional_sin,
  
  # Errores estándar bootstrap
  se = list(
    c(NA, se_uncond),
    c(NA, se_cond_con),
    c(NA, se_cond_sin)
  ),
  
  # Etiquetas
  covariate.labels = c(
    "Female",
    "Female <br> (partialled out with industries)",
    "Female <br> (partialled out no industries)"
  ),
  dep.var.labels = c(
    "Log Monthly Income",
    "Log Monthly Income <br> (partialled out with industries)",
    "Log Monthly Income <br> (partialled out no industries)"
  ),
  
  # Estadísticos
  keep.stat = c("n"),
  
  # Wage gap
  
  add.lines = list(
    c("Wage gap (COP)", 
      format(round(dif_uncond), big.mark = ","),
      format(round(dif_con),    big.mark = ","),
      format(round(dif_sin),    big.mark = ","))
  ),
  
  # Notas
  notes        = c("Bootstrapped Standard Errors in parentheses.",
                   "Wage gap (COP) compared to male average salary.",
                   "* p<0.1, ** p<0.05, *** p<0.01"),
  notes.append = FALSE,
  
  # Formato
  title  = "Monthly Income Wage Gap",
  digits = 4,
  type   = "html",
  out    = "02_output/tables/02_Resultados_Brecha_Genero.html"
)