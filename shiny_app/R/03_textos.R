# 03_textos.R — Páginas de contexto: Introducción, Objetivos, ETL y Conclusiones.
# Mismos textos que dash_app/pages/textos.py y etl.py. Las cifras salen de summary.csv.

OBJETIVOS <- list(
  list("1", "Composición del catálogo", "Describir el catálogo por plataforma, género y publisher.", "catalogo"),
  list("2", "Distribución de precios", "Analizar los precios por género y por plataforma.", "precios"),
  list("3", "Precios entre monedas", "Comparar el precio en USD, EUR, GBP, JPY y RUB para ver en qué región sale más caro.", "precios"),
  list("4", "Crecimiento del catálogo", "Describir cómo creció el número de lanzamientos por año y género.", "catalogo"),
  list("5", "Distribución geográfica", "Mostrar desde qué países juegan los usuarios de la muestra.", "jugadores"),
  list("6", "Juegos más populares", "Identificar los juegos con más compradores y su relación con el precio.", "popularidad"),
  list("7", "Actividad en el tiempo", "Analizar la actividad de los jugadores (trofeos ganados) entre 2008 y 2024.", "jugadores"),
  list("8", "Dificultad según trofeos", "Medir la dificultad de los juegos con la cantidad y la rareza de sus trofeos.", "logros"),
  list("9", "Cambios de precio", "Revisar si los precios cambiaron entre las capturas de febrero de 2025.", "precios")
)

page_introduccion <- function() {
  raw <- etl_quality |> group_by(tabla) |> summarise(filas = first(filas_antes), .groups = "drop")
  filas <- function(t) fmt_int(raw$filas[raw$tabla == t])
  tablas <- data.frame(
    archivo = c("games", "prices", "players", "purchased_games", "achievements", "history"),
    desc = c("Ficha de cada juego: título, plataforma, géneros, publisher y fecha",
             "Precio en 5 monedas, capturado varias veces en febrero de 2025",
             "Jugadores de la muestra y su país",
             "Biblioteca de juegos comprados por cada jugador",
             "Trofeos de cada juego y su rareza (Bronze a Platinum)",
             "Cada trofeo ganado por cada jugador, con fecha y hora"))
  tabla <- data.frame(a = I(lapply(tablas$archivo, function(t) tags$code(paste0(t, ".csv")))),
                      n = sapply(tablas$archivo, filas), d = tablas$desc)
  tagList(
    section_header("Introducción", "Contexto del mercado y pregunta de investigación", "book"),
    grid(
      ps_card("info-circle", "Contexto", div(class = "text-card",
        p("PlayStation Store reúne decenas de miles de juegos digitales para PS3, PS4, PS5 y PS Vita, ",
          "desde producciones AAA de más de $60 hasta juegos indie de menos de $1. Como copiar un ",
          "juego digital casi no cuesta nada, el precio depende del género, del publisher y de la competencia."),
        p("Este trabajo usa el dataset público ", strong("Gaming Profiles 2025"),
          " (Kruglov, Kaggle, licencia CC0), que reúne datos reales de Steam, PlayStation y Xbox. ",
          "Aquí se usan las 6 tablas del esquema ", strong("PlayStation"), "."),
        p(sprintf("El catálogo tiene %s productos (%s títulos distintos) y la muestra de jugadores reúne %s perfiles de %s países.",
                  S$n_games, S$n_titles, S$n_players, S$n_countries)))),
      ps_card("question-circle", "Pregunta de investigación", div(class = "text-card",
        div(class = "question", "¿Qué patrones de catálogo, precio, audiencia, popularidad y dificultad caracterizan a ",
            "PlayStation Store según los datos de Gaming Profiles 2025?"),
        p("Subpreguntas:"),
        tags$ol(
          tags$li("¿Qué plataformas, géneros y publishers dominan el catálogo y cómo creció?"),
          tags$li("¿Cómo se distribuyen los precios y en qué moneda sale más caro un juego?"),
          tags$li("¿Desde dónde y cuándo juegan los usuarios?"),
          tags$li("¿Qué juegos son los más comprados y tiene relación con su precio?"),
          tags$li("¿Qué tan difíciles son los juegos según sus trofeos?"))))
    ),
    ps_card("database", "Tablas del dataset (esquema PlayStation)",
            ps_table(tabla, c("Archivo", "Filas crudas", "Contenido"), num_cols = 2),
            note = "Fuente: kaggle.com/datasets/artyomkruglov/gaming-profiles-2025-steam-playstation-xbox"),
    spacer(),
    ps_card("diagram-3", "Flujo del proyecto", div(class = "step-grid",
      step(1, "Extracción", "Lectura de las 6 tablas crudas (3,8 GB con las otras plataformas).", BLUE),
      step(2, "ETL", "Limpieza, precio vigente por juego, países a ISO-3 y agregados.", ACCENT),
      step(3, "EDA", "Distribuciones, comparaciones y tendencias por objetivo.", GREEN),
      step(4, "Dashboards", "La misma app en Dash (Python) y en Shiny (R).", PURPLE),
      step(5, "Conclusiones", "Hallazgos por objetivo y limitaciones de los datos.", AMBER)))
  )
}

page_objetivos <- function() {
  colores <- c(BLUE, ACCENT, GREEN, BLUE, PURPLE, AMBER, PURPLE, RED, ACCENT)
  tarjetas <- lapply(seq_along(OBJETIVOS), function(i) {
    o <- OBJETIVOS[[i]]
    tags$a(href = paste0("#/", o[[4]]), style = "text-decoration:none",
           div(class = "step", style = paste0("--step-color:", colores[i], ";height:100%"),
               div(class = "step-num", o[[1]]), div(class = "step-title", o[[2]]), p(class = "step-desc", o[[3]])))
  })
  tagList(
    section_header("Objetivos", "Qué busca responder el dashboard", "bullseye"),
    ps_card("flag", "Objetivo general", div(class = "text-card", p(
      "Construir un dashboard interactivo que permita explorar el catálogo, los precios y el comportamiento de ",
      "los jugadores de PlayStation Store, para identificar patrones del mercado y apoyar decisiones de ",
      "desarrolladores, publishers y jugadores."))),
    spacer(),
    ps_card("list-check", "Objetivos específicos (clic para ir a la página)", div(class = "step-grid", tarjetas)),
    spacer(),
    ps_card("signpost-split", "Cómo se organiza el dashboard", div(
      insight("Catálogo:", "objetivos 1 y 4."),
      insight("Precios:", "objetivos 2, 3 y 9."),
      insight("Jugadores:", "objetivos 5 y 7."),
      insight("Popularidad:", "objetivo 6."),
      insight("Logros:", "objetivo 8.")))
  )
}

# ── ETL ──────────────────────────────────────────────────────────────────────
missing_fig <- function() {
  df <- missing_raw |> filter(pct_faltante > 0) |>
    mutate(campo = paste(tabla, "·", columna)) |> arrange(pct_faltante)
  df$campo <- factor(df$campo, levels = df$campo)
  cols <- c(games = BLUE, prices = AMBER)
  p <- plotly::plot_ly()
  for (t in unique(df$tabla)) {
    d <- df[df$tabla == t, ]
    p <- p |> plotly::add_bars(data = d, x = ~pct_faltante, y = ~campo, orientation = "h", name = t,
                               marker = list(color = unname(cols[t] %||% GREEN)),
                               text = sprintf("%.1f%%", d$pct_faltante), textposition = "outside", cliponaxis = FALSE)
  }
  p |> ps_style(340) |>
    plotly::layout(xaxis = list(title = "% de valores faltantes"), yaxis = list(title = ""), barmode = "relative") |>
    pad_x(max(df$pct_faltante))
}

missing_interp <- function() {
  m <- function(t, c) missing_raw$pct_faltante[missing_raw$tabla == t & missing_raw$columna == c]
  sprintf(paste0("Los faltantes se concentran en los precios en RUB (%.1f%%) y JPY (%.1f%%) y en los idiomas (%.1f%%). ",
                 "No son errores: muchos juegos no se venden en Rusia o Japón y la tienda no siempre publica los idiomas. ",
                 "Por eso la comparación de monedas usa solo juegos que tienen los dos precios. Las tablas de jugadores, ",
                 "compras, trofeos y actividad no tienen faltantes."),
          m("prices", "rub"), m("prices", "jpy"), m("games", "supported_languages"))
}

page_etl <- function() {
  fx <- fx_rates |> filter(currency != "USD")
  fx_txt <- paste(sprintf("1 %s = %s USD", fx$currency, signif(fx$usd_per_unit, 4)), collapse = ", ")
  q <- etl_quality |> transmute(tabla, paso, antes = fmt_int(filas_antes), despues = fmt_int(filas_despues), detalle)
  tagList(
    section_header("ETL", "Extracción, limpieza y transformación de las 6 tablas", "database-gear"),
    ps_card("diagram-3", "Pasos del proceso (etl/etl_playstation.py)", div(class = "step-grid",
      step("E", "Extraer", paste("Lectura de games, prices, players, purchased_games, achievements y history.",
                                 "Los archivos grandes se leen por partes (history: 19,5 M de filas)."), BLUE),
      step("T1", "Limpiar", paste("Duplicados, fechas inválidas, precios ≤ 0 como nulos, listas de texto",
                                  "(['Action', 'Indie']) convertidas a columnas."), ACCENT),
      step("T2", "Corregir el precio", paste("prices tiene 2 o 3 capturas por juego. Se usa solo la última",
                                             "(precio vigente) para no contar el mismo juego varias veces."), GREEN),
      step("T3", "Enriquecer", paste("País a código ISO-3 para el mapa, monedas convertidas a USD,",
                                     "dueños por juego y trofeos por rareza."), PURPLE),
      step("L", "Cargar", "13 tablas pequeñas en data/processed/ (≈10 MB) que leen Dash y Shiny.", AMBER))),
    spacer(),
    ps_card("table", "Registro de calidad: filas antes y después de cada paso",
            ps_table(q, c("Tabla", "Paso", "Antes", "Después", "Detalle"), num_cols = c(3, 4))),
    spacer(),
    grid(cols = "grid-60-40",
      ps_card("clipboard-data", "Valores faltantes en los datos crudos", plotly::plotlyOutput("etl_missing", height = "340px"),
              note = "Solo columnas con algún faltante, en los CSV originales.", interp = missing_interp()),
      ps_card("lightbulb", "Decisiones del ETL", div(
        insight("Precio vigente:", paste("se toma la última captura de cada juego. El trabajo anterior unía todas",
                                         "las capturas y contaba cada juego 2 o 3 veces.")),
        insight("Sin precio:", sprintf(paste("%s juegos no tienen precio publicado (gratis, retirados o",
                                             "solo en bundles). Se excluyen solo de los gráficos de precio."), S$n_no_price)),
        insight("Atípicos:", paste("los precios altos (> Q3 + 1,5·IQR) se marcan pero no se eliminan: son",
                                   "juegos AAA reales, no errores.")),
        insight("Ediciones regionales:", sprintf(paste("%s productos y %s títulos únicos. Se",
                                                       "conservan porque son fichas distintas de la tienda."), S$n_games, S$n_titles)),
        insight("Tasas de cambio:", paste0("aprox. 24-feb-2025: ", fx_txt, ".")),
        insight("Trofeos:", paste("un trofeo repetido por el mismo jugador se cuenta una vez; solo se usan",
                                  "trofeos de juegos que están en el catálogo."))))
    )
  )
}

page_conclusiones <- function() {
  tagList(
    section_header("Conclusiones", "Hallazgos por objetivo y limitaciones", "check2-circle"),
    grid(
      ps_card("lightbulb", "Hallazgos", div(
        insight("1. Catálogo:", sprintf("%s concentra el %s del catálogo; %s es el género principal más común (%s) y %s es el publisher con más productos (%s).",
                                        S$top_platform, S$top_platform_pct, S$top_genre, S$top_genre_pct, S$top_publisher, S$top_publisher_n)),
        insight("2. Precios:", sprintf("La mediana es %s y el %s de los juegos cuesta menos de $10. %s es el género más caro (mediana %s) y %s el más barato (%s).",
                                       S$median_price, S$pct_under_10, S$priciest_genre, S$priciest_genre_median, S$cheapest_genre, S$cheapest_genre_median)),
        insight("3. Monedas:", sprintf("Convertido a dólares, un juego cuesta en mediana %s más en %s y %s en %s frente a su precio en USD.",
                                       S$priciest_currency_diff, S$priciest_currency, S$cheapest_currency_diff, S$cheapest_currency)),
        insight("4. Crecimiento:", sprintf("Los lanzamientos pasaron de %s en 2015 a %s en 2024 (%s); el pico fue %s.",
                                           S$launches_2015, S$launches_2024, S$growth_2015_2024, S$peak_year)),
        insight("5. Geografía:", sprintf("%s aporta el %s de los jugadores y los 5 primeros países suman el %s.",
                                         S$top_country, S$top_country_pct, S$top5_countries_pct)),
        insight("6. Popularidad:", sprintf("%s está en el %s de las bibliotecas. La correlación de Spearman entre precio y compradores es %s: el precio casi no explica la popularidad.",
                                           S$top_owned_game, S$top_owned_pct, S$spearman_price_owners)),
        insight("7. Actividad:", sprintf("El mes con más trofeos fue %s; la hora con más actividad es las %s (probablemente UTC) y el día, %s.",
                                         S$peak_month, S$peak_hour, S$peak_weekday)),
        insight("8. Dificultad:", sprintf("Un juego tiene en mediana %s trofeos. El Platinum lo consigue en mediana el %s de sus jugadores, frente al %s de un Bronze.",
                                          S$median_trophies, S$earn_platinum, S$earn_bronze)),
        insight("9. Cambios de precio:", sprintf("Solo %s de %s juegos (%s) cambiaron de precio entre capturas: los precios fueron estables.",
                                                 S$n_price_changed, S$n_multi_snapshot, S$pct_price_changed)))),
      ps_card("exclamation-triangle", "Limitaciones", div(
        insight("Precios de 2025:", paste("las capturas son de 4 días de febrero de 2025. No hay historial de precios,",
                                          "así que no se puede medir la evolución de precios a lo largo de los años."), warn = TRUE),
        insight("Muestra de jugadores:", sprintf("los datos de jugadores no son un censo de PlayStation. Solo %s perfiles tienen biblioteca pública.",
                                                 S$n_players_library), warn = TRUE),
        insight("Compras ≠ ventas:", paste("la popularidad cuenta cuántos jugadores de la muestra tienen el juego, no",
                                           "las unidades vendidas."), warn = TRUE),
        insight("Ediciones regionales:", sprintf("un mismo título puede aparecer varias veces (%s productos, %s títulos). Se analizan como productos distintos de la tienda.",
                                                 S$n_games, S$n_titles), warn = TRUE),
        insight("Tasas de cambio:", "la comparación de monedas usa tasas aproximadas del 24-feb-2025.", warn = TRUE),
        insight("Último mes:", sprintf("la actividad de %s está incompleta y no se usa para el pico.", S$last_month), warn = TRUE)))
    )
  )
}
