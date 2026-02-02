# Problem Set 1: Predicting Income

## Big Data and Machine Learning para Economía Aplicada

**MECA 4107** -- Universidad de los Andes -- 2026-10

------------------------------------------------------------------------

## Autores

| Nombre           | Código    |
|------------------|-----------|
| Jose A. Rincon S | 202013328 |
| Juan C. Riaño    | 20....    |
| Lucas Rodriguez  | 20.....   |

------------------------------------------------------------------------

## Descripción

Este repositorio contiene el análisis del **Problem Set 1** del curso de Big Data y Machine Learning para Economía Aplicada. El objetivo es estimar y predecir el ingreso laboral utilizando datos de la Gran Encuesta Integrada de Hogares (GEIH) 2018 para Bogotá.

El análisis se divide en tres secciones:

1.  **Section 1: Age-Labor Income Profile** - Estimación del perfil edad-ingreso para testear las predicciones de la teoría del capital humano
2.  **Section 2: Gender Wage Gap** - Medición y descomposición de la brecha salarial de género
3.  **Section 3: Income Prediction** - Modelos de predicción de ingreso para fines de fiscalización tributaria

------------------------------------------------------------------------

## Instrucciones de Replicación

Para reproducir todos los resultados, ejecutar:

``` r
source("01_code/00_rundirectory.R")
```

Este script maestro ejecuta secuencialmente:

1.  Configuración del entorno y carga de paquetes
2.  Web scraping de los 10 chunks de datos de la GEIH 2018
3.  Limpieza de datos y construcción de la muestra de análisis
4.  Estadísticas descriptivas para cada sección
5.  Estimación de modelos econométricos
6.  Bootstrap para intervalos de confianza
7.  Generación de todas las figuras y tablas

**Tiempo estimado de ejecución:** 10-15 minutos

------------------------------------------------------------------------

## Estructura del Repositorio

```         
BDML-PS01/
│
├── README.md                                # Este archivo
├── .gitignore                               # Archivos ignorados por Git
│
├── 01_code/                                 # Todo el código del proyecto
│   │
│   ├── 00_rundirectory.R                    # Script maestro y cnfiguración inicial
│   │
│   │   # --- Carga y preparación de datos ---
│   ├── 01_data_scraping.R                   # Web scraping de GEIH 2018
│   ├── 02_data_cleaning?workers.R           # Limpieza y construcción de muestra
│   │
│   │   # --- Section 1: Age-Labor Income Profile ---
│   ├── 03_age_analysis_descriptive_stats.R  # Estadísticas descriptivas
│   ├── 04_age_income_models.R               # Modelos incondicional y condicional y Bootstrap para IC de edad pico
│   ├── 05_age_income_models_visualization.R # Visualización de resultado de los modelos
│   ├── 06_age_model_results_sum.R           # Resumen de los resultados obtenidos
│   │
│   │   # --- Section 2: Gender Wage Gap ---
│   ├── 06_section2_descriptive_stats.R      # Estadísticas por género
│   ├── 07_section2_wage_gap_models.R        # Modelos de brecha salarial
│   │
│   │   # --- Section 3: Income Prediction ---
│   ├── 08_section3_descriptive_stats.R      # Estadísticas para predicción
│   ├── 09_section3_prediction_models.R      # Modelos de machine learning
│   │
│   │   # --- Outputs finales ---
│   └── 10_generate_outputs.R                # Genera figuras y tablas finales
│
├── 02_outputs/                              # Resultados generados automáticamente
│   ├── figures/                             # Gráficos (.png)
│   │
│   └── tables/                              # Tablas (.tex, .html, .csv)
---
## Datos

### Fuente

[GEIH 2018 - Bogotá](https://ignaciomsarmiento.github.io/GEIH2018_sample/)

Los datos provienen de la Medición de Pobreza Monetaria y Desigualdad 2018 del DANE, basada en la Gran Encuesta Integrada de Hogares (GEIH). El dataset contiene **32,177 observaciones** distribuidas en 10 chunks.

### Variables Principales

| Variable | Descripción |
|----------|-------------|
| `age` | Edad del individuo (años) |
| `y_total_m` | Ingreso laboral mensual total (pesos colombianos) |
| `totalHoursWorked` | Horas trabajadas por semana |
| `sex` | Sexo (1 = Hombre, 2 = Mujer) |
| `relab` | Tipo de relación laboral (1-7) |

### Construcción de la Muestra

| Criterio | Justificación Económica |
|----------|-------------------------|
| `age >= 18` | Mercado laboral formal de adultos |
| `y_total_m > 0` | Individuos empleados con ingreso reportado |
| `totalHoursWorked > 0` | Consistencia con condición de empleo |
| `!is.na(relab)` | Tipo de empleo identificable |

### Variables Derivadas

| Variable | Definición | Uso |
|----------|------------|-----|
| `log_income` | `log(y_total_m)` | Variable dependiente |
| `age2` | `age^2` | Término cuadrático para perfil edad-ingreso |
| `female` | `1` si `sex == 2`, `0` si no | Indicador de género |
| `age_group` | Categorías: 18-24, 25-34, 35-44, 45-54, 55-64, 65+ | Descriptivas |

---

## Metodología

### Section 1: Age-Labor Income Profile

**Objetivo:** Testear si el perfil edad-ingreso es cóncavo, como predice la teoría del capital humano.

**Especificaciones:**

1. **Modelo Incondicional:**
```

log(w) = β₁ + β₂·Age + β₃·Age² + u \`\`\`

2.  **Modelo Condicional:**

    ```         
    log(w) = β₁ + β₂·Age + β₃·Age² + β₄·Hours + β₅·Relab + u
    ```

**Edad Pico:** Se calcula como `Age* = -β₂ / (2·β₃)` a partir de la condición de primer orden.

**Inferencia:** Los intervalos de confianza para la edad pico se calculan mediante **bootstrap** (B = 1000 réplicas) usando el método percentil, dado que la edad pico es una función no lineal de los coeficientes.

### Section 2: Gender Wage Gap

**Objetivo:** Medir la brecha salarial de género y analizar cómo cambia al controlar por características observables.

### Section 3: Income Prediction

**Objetivo:** Construir modelos predictivos de ingreso para identificar individuos con posible subdeclaración tributaria.

------------------------------------------------------------------------

## Software

```         
R version 4.3.0 o superior
```

### Paquetes Requeridos

Los paquetes se instalan y cargan automáticamente mediante `pacman::p_load()`:

| Categoría     | Paquetes                                    |
|---------------|---------------------------------------------|
| Datos         | `tidyverse`, `rvest`                        |
| Modelado      | `boot`, `broom`, `caret`                    |
| Visualización | `ggplot2`, `scales`, `gridExtra`, `viridis` |
| Tablas        | `stargazer`, `knitr`, `kableExtra`          |

## Referencias

-   Sarmiento-Barbieri, I. (2026). *Big Data and Machine Learning for Applied Economics*. Universidad de los Andes.
-   DANE. *Gran Encuesta Integrada de Hogares (GEIH) 2018*.
-   Mincer, J. (1974). *Schooling, Experience, and Earnings*. NBER.

------------------------------------------------------------------------

## Contacto

Para preguntas sobre este repositorio, contactar a los autores menciondos al inicio.
