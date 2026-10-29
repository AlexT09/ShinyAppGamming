# PS Gaming Analytics 2025 · App Shiny + informe bookdown

Versión en **R** del proyecto de visualización sobre **PlayStation Store** con el dataset público [Gaming Profiles 2025](https://www.kaggle.com/datasets/artyomkruglov/gaming-profiles-2025-steam-playstation-xbox) (Kaggle, CC0):

- **App Shiny** (`shiny_app/`): dashboard interactivo con `shiny`, `bslib`, `plotly`, `dplyr` y `DT`.
- **Informe bookdown** (raíz del repo, publicado en `docs/`): introducción, ETL, EDA con `ggplot2` y conclusiones, con una interpretación debajo de cada gráfico.

La misma app existe en **Python / Dash** en [DashAppGamming](https://github.com/AlexT09/DashAppGamming). Las dos leen los mismos archivos de `data/processed/`, tienen las mismas páginas y el mismo diseño (`ps-theme.css`), y muestran las mismas cifras e interpretaciones.

## Pregunta y objetivos

> ¿Qué patrones de catálogo, precio, audiencia, popularidad y dificultad caracterizan a PlayStation Store según los datos de Gaming Profiles 2025?

| # | Objetivo | Página |
|---|---|---|
| 1 | Composición del catálogo por plataforma, género y publisher | Catálogo |
| 2 | Distribución de precios por género y plataforma | Precios |
| 3 | Comparación de precios entre USD, EUR, GBP, JPY y RUB | Precios |
| 4 | Crecimiento del catálogo por año y género | Catálogo |
| 5 | Distribución geográfica de los jugadores | Jugadores |
| 6 | Juegos más comprados y su relación con el precio | Popularidad |
| 7 | Actividad de los jugadores en el tiempo (2008–2024) | Jugadores |
| 8 | Dificultad de los juegos según sus trofeos | Logros |
| 9 | Cambios de precio entre capturas de febrero de 2025 | Precios |

## Ejecutar la app Shiny

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "dplyr", "tidyr", "readr"))
shiny::runApp("shiny_app")      # desde la carpeta del repositorio
```

En RStudio también puedes abrir `shiny_app/app.R` y pulsar **Run App**.

La app tiene 11 páginas (Resumen, Introducción, Objetivos, ETL, Catálogo, Precios, Jugadores, Popularidad, Logros, Explorador y Conclusiones) y filtros globales por plataforma, género y año. Los archivos de `shiny_app/R/` se cargan solos en orden:

| Archivo | Contenido |
|---|---|
| `01_theme.R` | Colores, estilo de los gráficos y bloques de interfaz |
| `02_data.R` | Carga de `data/processed/` y filtros |
| `03_textos.R` | Páginas de texto: Introducción, Objetivos, ETL y Conclusiones |
| `04_analisis.R` | Gráficos e interpretaciones de cada página de análisis |

> Para publicar en shinyapps.io, copia `data/processed/` dentro de `shiny_app/data/processed/`: la app también busca los datos ahí.

## Compilar el informe

```r
install.packages(c("bookdown", "ggplot2", "scales", "knitr", "dplyr", "tidyr", "readr"))
bookdown::render_book("index.Rmd")   # resultado en docs/
```

Para GitHub Pages: *Settings → Pages → Branch: main → Folder: /docs*.

## Datos y ETL

`data/processed/` ya viene en el repositorio (≈10 MB). Para regenerarlo desde los CSV originales (3,8 GB) se usa el ETL en Python de `etl/` (mismo código que en el repo de Dash):

```bash
pip install -r etl/requirements.txt
python etl/etl_playstation.py --raw "ruta/a/playstation"
```

## Estructura

```text
ShinyAppGamming/
├── shiny_app/          # app Shiny (app.R, R/, www/ps-theme.css)
├── index.Rmd, 01-introduccion.Rmd, 02-etl.Rmd, 03-eda.Rmd, 04-conclusiones.Rmd
├── _bookdown.yml, _output.yml, style.css
├── docs/               # libro compilado (GitHub Pages)
├── data/processed/     # 13 tablas limpias que leen la app y el libro
└── etl/                # ETL en Python (opcional)
```

## Autor

**Alex Terán** · Curso de Visualización de Datos.
