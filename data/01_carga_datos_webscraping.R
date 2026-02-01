################################################################################
##                                                                            ##
##                    CARGA DE DATOS - WEB SCRAPING                           ##
##                    GEIH 2018 - Bogotá                                      ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

# ==============================================================================
# 1. SETUP
# ==============================================================================

rm(list = ls())

# Instalar pacman si no está
if (!require("pacman")) install.packages("pacman")

# Cargar paquetes necesarios
pacman::p_load(
  tidyverse,  # Manipulación de datos
  rvest,      # Web scraping
  httr        # HTTP requests
)

# Crear carpeta para guardar datos
dir.create("data", showWarnings = FALSE)

# ==============================================================================
# 2. WEB SCRAPING DE LOS 10 CHUNKS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("     WEB SCRAPING - GEIH 2018 BOGOTÁ                            \n")
cat("     Fuente: ignaciomsarmiento.github.io/GEIH2018_sample/       \n")
cat("================================================================\n\n")

# URL base
base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

# Lista para almacenar chunks
chunks_list <- list()

# Loop para descargar los 10 chunks
for (i in 1:10) {
  
  # Construir URL
  url <- paste0(base_url, "page", i, ".html")
  
  cat(sprintf("[%2d/10] Descargando: %s\n", i, url))
  
  tryCatch({
    
    # Leer página HTML
    pagina <- read_html(url)
    
    # Extraer tabla (la página contiene una tabla HTML con los datos)
    tabla <- pagina %>%
      html_table(fill = TRUE)
    
    # La tabla está en el primer elemento
    if (length(tabla) > 0) {
      chunk_df <- tabla[[1]]
      
      # Agregar identificador de chunk
      chunk_df$chunk_id <- i
      
      # Guardar en la lista
      chunks_list[[i]] <- chunk_df
      
      cat(sprintf("        ✓ %s observaciones descargadas\n", 
                  format(nrow(chunk_df), big.mark = ",")))
    }
    
    # Pausa de 1 segundo para no sobrecargar el servidor
    Sys.sleep(20)
    
  }, error = function(e) {
    cat(sprintf("        ✗ Error: %s\n", e$message))
    chunks_list[[i]] <- NULL
  })
}

# ==============================================================================
# 3. COMBINAR TODOS LOS CHUNKS
# ==============================================================================

cat("\n")
cat("Combinando chunks...\n")

# Filtrar chunks nulos (por si alguno falló)
chunks_validos <- chunks_list[!sapply(chunks_list, is.null)]

# Combinar en un único dataframe
geih_raw <- bind_rows(chunks_validos)

cat(sprintf("\n✓ TOTAL: %s observaciones de %d chunks\n",
            format(nrow(geih_raw), big.mark = ","),
            length(chunks_validos)))

# ==============================================================================
# 4. VERIFICAR ESTRUCTURA DE LOS DATOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("     ESTRUCTURA DE LOS DATOS                                    \n")
cat("================================================================\n\n")

cat("Dimensiones:", nrow(geih_raw), "filas x", ncol(geih_raw), "columnas\n\n")

cat("Primeras 20 variables:\n")
print(names(geih_raw)[1:min(20, ncol(geih_raw))])

cat("\n")
cat("Vista previa de variables clave:\n")
cat("─────────────────────────────────────────\n")

# Verificar variables clave
vars_clave <- c("age", "y_total_m", "totalHoursWorked", "sex", "relab")

for (v in vars_clave) {
  if (v %in% names(geih_raw)) {
    cat(sprintf("  %s: ✓ encontrada\n", v))
  } else {
    # Buscar variantes
    variante <- grep(v, names(geih_raw), ignore.case = TRUE, value = TRUE)
    if (length(variante) > 0) {
      cat(sprintf("  %s: encontrada como '%s'\n", v, variante[1]))
    } else {
      cat(sprintf("  %s: ✗ NO encontrada\n", v))
    }
  }
}

# ==============================================================================
# 5. GUARDAR DATOS RAW
# ==============================================================================

cat("\n")
cat("Guardando datos...\n")

# Guardar como RDS (formato eficiente de R)
saveRDS(geih_raw, "data/geih_2018_bogota_raw.rds")
cat("  ✓ data/geih_2018_bogota_raw.rds\n")

# También guardar como CSV (por si acaso)
write_csv(geih_raw, "data/geih_2018_bogota_raw.csv")
cat("  ✓ data/geih_2018_bogota_raw.csv\n")

cat("\n")
cat("================================================================\n")
cat("     DESCARGA COMPLETADA                                        \n")
cat("================================================================\n")
cat("\n")
cat("Para cargar los datos en el futuro, usa:\n")
cat("  geih_raw <- readRDS('data/geih_2018_bogota_raw.rds')\n")
cat("\n")

# ==============================================================================
# 6. RESUMEN ESTADÍSTICO RÁPIDO
# ==============================================================================

cat("================================================================\n")
cat("     RESUMEN ESTADÍSTICO RÁPIDO                                 \n")
cat("================================================================\n\n")

# Convertir a numérico las variables clave
geih_raw <- geih_raw %>%
  mutate(
    age = as.numeric(age),
    y_total_m = as.numeric(y_total_m),
    totalHoursWorked = as.numeric(totalHoursWorked)
  )

cat("EDAD:\n")
cat(sprintf("  Rango: %d - %d años\n", 
            min(geih_raw$age, na.rm = TRUE),
            max(geih_raw$age, na.rm = TRUE)))
cat(sprintf("  Media: %.1f años\n", mean(geih_raw$age, na.rm = TRUE)))
cat(sprintf("  Missing: %d (%.1f%%)\n", 
            sum(is.na(geih_raw$age)),
            sum(is.na(geih_raw$age))/nrow(geih_raw)*100))

cat("\nINGRESO MENSUAL (y_total_m):\n")
cat(sprintf("  Observaciones con ingreso > 0: %s\n",
            format(sum(geih_raw$y_total_m > 0, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  Observaciones con ingreso = 0: %s\n",
            format(sum(geih_raw$y_total_m == 0, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  Missing: %s\n",
            format(sum(is.na(geih_raw$y_total_m)), big.mark = ",")))

cat("\nHORAS TRABAJADAS:\n")
cat(sprintf("  Media (entre quienes trabajan): %.1f horas/semana\n",
            mean(geih_raw$totalHoursWorked[geih_raw$totalHoursWorked > 0], na.rm = TRUE)))

cat("\n")