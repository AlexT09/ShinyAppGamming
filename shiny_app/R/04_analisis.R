# 04_analisis.R — Páginas de análisis (EDA): Resumen, Catálogo, Precios, Jugadores,
# Popularidad, Logros y Explorador. Cada gráfico tiene su función fig_* y su texto interp_*,
# equivalentes a los de dash_app/pages/*.py.

# ── Utilidades ───────────────────────────────────────────────────────────────
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
usd <- function(x) paste0("$", f2(x))
signed <- function(x) paste0(ifelse(x >= 0, "+", ""), f1(x), "%")
last_full_year <- function(df) { y <- df$year[df$year <= 2024]; if (length(y)) max(y) else max(df$year) }
y_lista <- function(x) if (length(x) > 1) paste(paste(head(x, -1), collapse = ", "), "y", tail(x, 1)) else x
top_genres <- function(df, n) head(names(vcount(df$genre_main[df$genre_main != "Sin género"])), n)

vline <- function(p, x, label) {
  plotly::layout(p,
    shapes = list(list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
                       line = list(dash = "dash", color = TEXT, width = 1.5))),
    annotations = list(list(x = x, y = 1, yref = "paper", text = label, showarrow = FALSE,
                            xanchor = "left", font = list(color = TEXT))))
}

# ══ RESUMEN ═════════════════════════════════════════════════════════════════
page_resumen <- function() tagList(
  section_header("Resumen", "Vista general del catálogo y de la audiencia de PlayStation Store", "speedometer2"),
  uiOutput("res_kpis", class = "kpi-grid"),
  grid(cols = "grid-60-40",
    graph_card("bar-chart-steps", "Lanzamientos por año y plataforma", "res_year", dynamic = TRUE,
               note = "Productos de la tienda según su año de lanzamiento. 2025 solo incluye enero."),
    ps_card("lightbulb", "Insights clave (todo el dataset)", div(
      insight("Catálogo:", sprintf("%s tiene el %s de los productos.", S$top_platform, S$top_platform_pct)),
      insight("Precio:", sprintf("mediana de %s; el %s cuesta menos de $10.", S$median_price, S$pct_under_10)),
      insight("Audiencia:", sprintf("%s reúne el %s de los jugadores.", S$top_country, S$top_country_pct)),
      insight("Popularidad:", sprintf("%s está en el %s de las bibliotecas.", S$top_owned_game, S$top_owned_pct)),
      insight("Dificultad:", sprintf("solo el %s (mediana) de los jugadores de un juego consigue su Platinum.", S$earn_platinum)),
      insight("Precios estables:", sprintf("solo %s juegos cambiaron de precio en feb. 2025.", S$n_price_changed), warn = TRUE)))
  )
)

res_kpis <- function(df) {
  priced <- df$price_usd[!is.na(df$price_usd)]
  tr <- df$n_trophies[df$n_trophies > 0]
  tagList(
    kpi("Productos", fmt_int(nrow(df)), paste(fmt_int(length(unique(df$title))), "títulos únicos"), BLUE),
    kpi("Precio mediano", if (length(priced)) usd(median(priced)) else "—", "USD, feb. 2025", AMBER),
    kpi("Menos de $10", if (length(priced)) paste0(f1(mean(priced < 10) * 100), "%") else "—", "de los juegos con precio", GREEN),
    kpi("Trofeos por juego", if (length(tr)) fmt_int(median(tr)) else "—", "mediana", PURPLE),
    kpi("Jugadores", S$n_players, paste(S$n_countries, "países · sin filtro"), ACCENT))
}

fig_res_year <- function(df) {
  by <- df |> count(year, platform, name = "juegos")
  plotly::plot_ly(by, x = ~year, y = ~juegos, color = ~platform, colors = PLATFORM_COLORS, type = "bar") |>
    ps_style(360) |>
    plotly::layout(barmode = "stack", bargap = 0.15,
                   xaxis = list(title = "Año de lanzamiento"), yaxis = list(title = "Productos"))
}

interp_res_year <- function(df) {
  by <- vcount(df$year); plat <- vcount(df$platform) / nrow(df) * 100
  sprintf(paste0("El año con más lanzamientos es %s (%s productos). El %s%% de los productos filtrados salió desde 2020: ",
                 "el catálogo es reciente y creció muy rápido en los últimos años. %s es la plataforma con más productos (%s%%)."),
          names(by)[1], fmt_int(by[1]), f1(mean(df$year >= 2020) * 100), names(plat)[1], f1(plat[1]))
}

# ══ CATÁLOGO ════════════════════════════════════════════════════════════════
page_catalogo <- function() tagList(
  section_header("Catálogo", "Qué se vende en PlayStation Store y cómo creció la oferta", "grid-3x3-gap",
                 c("Objetivo 1", "Objetivo 4")),
  grid(
    graph_card("diagram-2", "Plataforma → género principal", "cat_treemap", height = 400, dynamic = TRUE,
               note = "El tamaño de cada bloque es el número de productos. Clic para entrar en una plataforma."),
    graph_card("building", "Top 15 publishers por número de productos", "cat_publishers", height = 400, dynamic = TRUE)),
  grid(
    graph_card("graph-up", "Lanzamientos por año — 6 géneros más comunes", "cat_genre_year", dynamic = TRUE),
    graph_card("pie-chart", "Participación de cada plataforma por año", "cat_platform_share", dynamic = TRUE,
               note = "Porcentaje de los lanzamientos de cada año."))
)

fig_cat_treemap <- function(df) {
  tm <- df |> count(platform, genre_main, name = "v")
  pl <- tm |> group_by(platform) |> summarise(v = sum(v), .groups = "drop")
  ids <- c("Catálogo", paste0("Catálogo/", pl$platform), paste0("Catálogo/", tm$platform, "/", tm$genre_main))
  labels <- c("Catálogo", pl$platform, tm$genre_main)
  parents <- c("", rep("Catálogo", nrow(pl)), paste0("Catálogo/", tm$platform))
  values <- c(sum(tm$v), pl$v, tm$v)
  # Color: géneros por su conteo; plataformas por el promedio ponderado de sus géneros (como plotly express);
  # la raíz en 0 para que quede oscura como en Dash.
  pl_col <- sapply(pl$platform, function(p) { v <- tm$v[tm$platform == p]; sum(v^2) / sum(v) })
  plotly::plot_ly(type = "treemap", ids = ids, labels = labels, parents = parents, values = values,
                  branchvalues = "total",
                  marker = list(colors = c(0, pl_col, tm$v), colorscale = SEQ_SCALE, line = list(color = "#0A0F1E")),
                  hovertemplate = "<b>%{label}</b><br>%{value:,} productos<extra></extra>") |>
    ps_style(400, legend = FALSE)
}

interp_cat_treemap <- function(df) {
  plat <- vcount(df$platform) / nrow(df) * 100; top_p <- names(plat)[1]
  gen <- vcount(df$genre_main[df$platform == top_p])
  shares <- vcount(df$genre_main) / nrow(df) * 100; small <- sum(shares < 1)
  txt <- if (length(plat) == 1) paste("Todos los productos filtrados son de", top_p)
         else sprintf("%s concentra el %s%% de los productos filtrados", top_p, f1(plat[1]))
  if (length(shares) == 1) return(sprintf("%s y todos son del género %s (%s productos).", txt, names(gen)[1], fmt_int(gen[1])))
  txt <- sprintf("%s y dentro de ella el género más grande es %s (%s productos).", txt, names(gen)[1], fmt_int(gen[1]))
  if (small > 0) txt <- sprintf("%s %d de los %d géneros tienen menos del 1%% del catálogo: hay pocos géneros grandes y muchos de nicho.",
                                txt, small, length(shares))
  txt
}

fig_cat_publishers <- function(df) {
  pub <- head(vcount(df$publisher), 15)
  pub <- data.frame(publisher = names(pub), productos = as.integer(pub))[15:1, ] |> na.omit()
  plotly::plot_ly(pub, x = ~productos, y = ~factor(publisher, levels = publisher), type = "bar", orientation = "h",
                  marker = list(color = BLUE), text = ~productos, textposition = "outside", cliponaxis = FALSE) |>
    ps_style(400, legend = FALSE) |>
    plotly::layout(xaxis = list(title = "Productos"), yaxis = list(title = "")) |>
    pad_x(max(pub$productos))
}

interp_cat_publishers <- function(df) {
  top <- vcount(df$publisher); top15 <- head(names(top), 15)
  en <- df$publisher %in% top15
  m_top <- median(df$price_usd[en], na.rm = TRUE); m_rest <- median(df$price_usd[!en], na.rm = TRUE)
  txt <- sprintf("Los 15 publishers con más productos reúnen el %s%% del catálogo filtrado y %s lidera con %s.",
                 f1(mean(en) * 100), names(top)[1], fmt_int(top[1]))
  if (!is.na(m_top) && !is.na(m_rest))
    txt <- sprintf("%s El precio mediano de sus juegos es %s frente a %s del resto: %s", txt, usd(m_top), usd(m_rest),
                   if (m_top < m_rest) "los que más publican venden juegos más baratos." else "publicar mucho no implica precios más bajos.")
  txt
}

fig_cat_genre_year <- function(df, top) {
  gy <- df |> filter(genre_main %in% top) |> count(year, genre_main, name = "productos") |>
    mutate(genre_main = factor(genre_main, levels = top))
  plotly::plot_ly(gy, x = ~year, y = ~productos, color = ~genre_main, colors = setNames(CAT_COLORS[seq_along(top)], top),
                  type = "scatter", mode = "lines+markers") |>
    ps_style() |> plotly::layout(xaxis = list(title = "Año de lanzamiento"), yaxis = list(title = "Productos"))
}

interp_cat_genre_year <- function(df, top) {
  yl <- last_full_year(df); sub <- df[df$genre_main %in% top, ]
  last <- vcount(sub$genre_main[sub$year == yl])
  if (!length(last)) return("No hay lanzamientos en el último año del rango.")
  before <- table(sub$genre_main[sub$year == yl - 5])
  b <- sapply(names(last), function(g) if (g %in% names(before)) as.integer(before[[g]]) else 0L)
  growth <- as.integer(last) - b; g1 <- names(last)[1]; g2 <- names(last)[which.max(growth)]
  txt <- sprintf("En %d, %s fue el género con más lanzamientos (%s).", yl, g1, fmt_int(last[1]))
  cambio <- sprintf("pasó de %s lanzamientos en %d a %s", fmt_int(b[[g2]]), yl - 5, fmt_int(last[[g2]]))
  if (g2 == g1) paste0(txt, " También fue el que más creció: ", cambio, ".")
  else sprintf("%s El que más creció fue %s: %s.", txt, g2, cambio)
}

fig_cat_platform_share <- function(df) {
  ps <- df |> count(year, platform) |> group_by(year) |> mutate(pct = n / sum(n) * 100) |> ungroup()
  plotly::plot_ly(ps, x = ~year, y = ~pct, color = ~platform, colors = PLATFORM_COLORS, type = "scatter",
                  mode = "lines", stackgroup = "one") |>
    ps_style() |>
    plotly::layout(xaxis = list(title = "Año de lanzamiento"), yaxis = list(title = "% de lanzamientos", range = c(0, 100), ticksuffix = "%"))
}

interp_cat_platform_share <- function(df) {
  if (length(unique(df$platform)) == 1)
    return("Con una sola plataforma seleccionada el gráfico siempre muestra 100%; quita el filtro para comparar.")
  yl <- last_full_year(df)
  s <- vcount(df$platform[df$year == yl]); s <- s / sum(s) * 100
  txt <- sprintf("En %d los lanzamientos se repartieron así: %s.", yl, paste(sprintf("%s el %s%%", names(s), f1(s)), collapse = ", "))
  yrs <- sort(unique(df$year))
  old <- sapply(yrs, function(y) mean(df$platform[df$year == y] %in% c("PS3", "PS Vita")) * 100)
  after <- yrs[sapply(seq_along(yrs), function(i) all(old[i:length(old)] < 5))]
  if (length(after) && max(old) >= 5)
    txt <- sprintf("%s El cambio de generación es claro: desde %d PS3 y PS Vita reciben menos del 5%% de los juegos.", txt, after[1])
  txt
}

# ══ PRECIOS ═════════════════════════════════════════════════════════════════
TIER_COLORS <- c("#13294B", "#1D4E89", "#0070D1", "#00AAFF", "#7FD3F7")

page_precios <- function() tagList(
  section_header("Precios", "Cuánto cuestan los juegos, en qué moneda salen más caros y si cambian",
                 "currency-dollar", c("Objetivo 2", "Objetivo 3", "Objetivo 9")),
  grid(cols = "grid-60-40",
    graph_card("box", "Precio por género principal (12 más comunes)", "pre_box", height = 400, dynamic = TRUE,
               note = "Ordenado por mediana. La caja va del 25% al 75% de los precios; eje recortado en $80."),
    graph_card("bar-chart", "Rango de precio por plataforma", "pre_tier", height = 400, dynamic = TRUE,
               note = "Porcentaje de juegos de cada plataforma en cada rango (USD).")),
  grid(
    graph_card("currency-exchange", "¿En qué moneda sale más caro?", "pre_currency", dynamic = TRUE,
               note = paste("Diferencia mediana del precio local convertido a USD frente al precio en USD del mismo",
                            "juego. Tasas aprox. del 24-feb-2025.")),
    graph_card("graph-down", "Precio mediano según año de lanzamiento", "pre_year", dynamic = TRUE,
               note = paste("Precio de feb. 2025 de los juegos lanzados cada año (no es el precio histórico).",
                            "Solo años con 20+ juegos; 2025 se excluye porque solo tiene enero."))),
  ps_card("arrow-left-right", "Juegos que cambiaron de precio entre capturas (22 al 25 de febrero de 2025)",
          uiOutput("pre_changes"), interp_id = "pre_changes_interp")
)

price_box_data <- function(p) {
  top <- top_genres(p, 12); b <- p[p$genre_main %in% top, ]
  ord <- b |> group_by(genre_main) |> summarise(m = median(price_usd), .groups = "drop") |> arrange(desc(m))
  b$genre_main <- factor(b$genre_main, levels = ord$genre_main); b
}

fig_pre_box <- function(b) {
  plotly::plot_ly(b, x = ~genre_main, y = ~price_usd, type = "box", boxpoints = FALSE,
                  marker = list(color = BLUE), line = list(color = BLUE), fillcolor = "rgba(0,112,209,0.35)") |>
    ps_style(400, legend = FALSE) |>
    plotly::layout(xaxis = list(title = "", tickangle = -35), yaxis = list(title = "Precio (USD)", range = c(0, 80), tickprefix = "$"))
}

interp_pre_box <- function(b) {
  st <- b |> group_by(genre_main) |> summarise(q1 = quantile(price_usd, .25), m = median(price_usd),
                                               q3 = quantile(price_usd, .75), .groups = "drop") |> mutate(iqr = q3 - q1)
  if (nrow(st) == 1)
    return(sprintf("Con un solo género no hay comparación: la mitad de los juegos de %s cuesta entre %s y %s (mediana %s).",
                   st$genre_main, usd(st$q1), usd(st$q3), usd(st$m)))
  hi <- st[which.max(st$m), ]; lo <- st[which.min(st$m), ]; na <- st[which.min(st$iqr), ]
  sprintf(paste0("%s tiene el precio mediano más alto (%s) y %s el más bajo (%s). %s tiene la caja más estrecha ",
                 "(la mitad de sus juegos cuesta entre %s y %s): sus precios son muy parecidos. Los bigotes largos ",
                 "muestran que en casi todos los géneros conviven juegos baratos y juegos caros."),
          hi$genre_main, usd(hi$m), lo$genre_main, usd(lo$m), na$genre_main, usd(na$q1), usd(na$q3))
}

tier_data <- function(p) p |> count(platform, price_tier) |> group_by(platform) |> mutate(pct = n / sum(n) * 100) |> ungroup()

fig_pre_tier <- function(t) {
  t$price_tier <- factor(t$price_tier, levels = TIERS)
  plotly::plot_ly(t, x = ~platform, y = ~pct, color = ~price_tier, colors = setNames(TIER_COLORS, TIERS), type = "bar") |>
    ps_style(400) |>
    plotly::layout(barmode = "stack", legend = list(traceorder = "normal"),
                   xaxis = list(title = ""), yaxis = list(title = "% de juegos", ticksuffix = "%", range = c(0, 100)))
}

interp_pre_tier <- function(p, t) {
  cheap <- t[t$price_tier == "< $5", ]; over40 <- mean(p$price_usd >= 40) * 100; txt <- ""
  if (nrow(cheap)) {
    i <- which.max(cheap$pct)
    txt <- if (length(unique(p$platform)) == 1) sprintf("El %s%% de los juegos de %s cuesta menos de $5. ", f1(cheap$pct[i]), cheap$platform[i])
           else sprintf("%s tiene la mayor proporción de juegos de menos de $5 (%s%%). ", cheap$platform[i], f1(cheap$pct[i]))
  }
  paste0(txt, sprintf("En total solo el %s%% de los juegos cuesta $40 o más: el catálogo es sobre todo de bajo precio y los juegos AAA son una minoría.", f1(over40)))
}

currency_diffs <- function(p) {
  out <- lapply(CURRENCIES[-1], function(c) {
    col <- paste0("price_", c, "_in_usd"); both <- p[!is.na(p[[col]]), ]
    if (!nrow(both)) return(NULL)
    data.frame(moneda = toupper(c), dif = (median(both[[col]] / both$price_usd) - 1) * 100, n = nrow(both))
  })
  bind_rows(out) |> arrange(desc(dif))
}

fig_pre_currency <- function(cur) {
  cur$moneda <- factor(cur$moneda, levels = cur$moneda)
  plotly::plot_ly(cur, x = ~moneda, y = ~dif, color = ~moneda, colors = CURRENCY_COLORS, type = "bar",
                  text = ~signed(dif), textposition = "outside", cliponaxis = FALSE, customdata = ~n,
                  hovertemplate = "%{x}: %{y:+.1f}% · %{customdata:,} juegos<extra></extra>") |>
    ps_style(legend = FALSE) |>
    plotly::layout(xaxis = list(title = ""), yaxis = list(title = "Diferencia vs USD (%)", ticksuffix = "%"),
                   shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0, line = list(color = MUTED, width = 1))))
}

interp_pre_currency <- function(cur) {
  if (!nrow(cur)) return("No hay juegos con precio en otras monedas para estos filtros.")
  top <- cur[1, ]; low <- cur[nrow(cur), ]; cheaper <- cur$moneda[cur$dif < 0]
  txt <- sprintf("El mismo juego cuesta en mediana %s en %s y %s en %s frente a su precio en USD. Sony fija precios distintos por región",
                 signed(top$dif), top$moneda, signed(low$dif), low$moneda)
  if (length(cheaper)) sprintf("%s: comprar en %s sale más barato que en dólares.", txt, y_lista(as.character(cheaper))) else paste0(txt, ".")
}

year_price_data <- function(p) {
  p |> filter(platform %in% c("PS4", "PS5"), year <= 2024) |> group_by(year, platform) |>
    summarise(median = median(price_usd), size = n(), .groups = "drop") |> filter(size >= 20)
}

fig_pre_year <- function(y) {
  plotly::plot_ly(y, x = ~year, y = ~median, color = ~platform, colors = PLATFORM_COLORS, type = "scatter", mode = "lines+markers") |>
    ps_style() |> plotly::layout(xaxis = list(title = "Año de lanzamiento"), yaxis = list(title = "Precio mediano (USD)", tickprefix = "$"))
}

interp_pre_year <- function(y) {
  parts <- c(); trend <- c()
  for (pl in sort(unique(y$platform))) {
    d <- y[y$platform == pl, ] |> arrange(year)
    if (nrow(d) >= 2) {
      a <- d[1, ]; b <- d[nrow(d), ]
      parts <- c(parts, sprintf("en %s pasa de %s (juegos de %d) a %s (juegos de %d)", pl, usd(a$median), a$year, usd(b$median), b$year))
      trend <- c(trend, b$median < a$median)
    }
  }
  if (!length(parts)) return("Hay pocos juegos por año para comparar con estos filtros.")
  txt <- paste0("La mediana ", paste(parts, collapse = " y "), ".")
  if (all(trend)) txt <- paste(txt, "Los juegos más recientes son más baratos porque cada año llegan más juegos indie de bajo precio;",
                               "no significa que los precios bajen con el tiempo (todos son precios de 2025).")
  txt
}

price_changes <- function(df) {
  s <- snapshots |> filter(gameid %in% df$gameid, !is.na(usd)) |> arrange(date)
  fl <- s |> group_by(gameid) |> summarise(first = first(usd), last = last(usd), size = n(), .groups = "drop")
  multi <- sum(fl$size >= 2)
  ch <- fl |> filter(first != last) |> left_join(df |> select(gameid, title, platform), by = "gameid") |>
    mutate(pct = (last / first - 1) * 100) |> arrange(pct)
  resumen <- p(class = "card-note", style = "margin-bottom:8px",
               sprintf("%d de %s juegos con 2 o más capturas cambiaron de precio (%s%%).", nrow(ch), fmt_int(multi),
                       f2(if (multi) nrow(ch) / multi * 100 else 0)))
  txt <- if (!nrow(ch)) "Ningún juego de esta selección cambió de precio entre las capturas."
         else sprintf("En 4 días casi ningún precio cambió: %d juegos bajaron y %d subieron.", sum(ch$pct < 0), sum(ch$pct > 0))
  txt <- paste(txt, "No se ven rebajas masivas en este periodo, así que los precios del análisis son estables y representativos.",
               "Para estudiar descuentos haría falta un historial de varios meses.")
  if (!nrow(ch)) return(list(ui = resumen, interp = txt))
  tabla <- data.frame(t = ch$title, p = ch$platform, a = usd(ch$first), d = usd(ch$last),
                      c = I(lapply(ch$pct, function(v) span(style = paste0("color:", if (v < 0) GREEN else RED), signed(v)))))
  list(ui = tagList(resumen, ps_table(tabla, c("Juego", "Plataforma", "Antes", "Después", "Cambio"), num_cols = 3:5, max_height = "320px")),
       interp = txt)
}

# ══ JUGADORES (no usa filtros) ═════════════════════════════════════════════
DIAS <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")

fig_jug_map <- function() {
  plotly::plot_ly(countries, type = "choropleth", locations = ~iso3, z = ~n_players, text = ~country,
                  customdata = ~pct_players, colorscale = SEQ_SCALE, marker = list(line = list(color = "#0A0F1E")),
                  colorbar = list(title = "", thickness = 10, len = 0.7, tickfont = list(color = MUTED)),
                  hovertemplate = "<b>%{text}</b><br>%{z:,} jugadores (%{customdata:.1f}%)<extra></extra>") |>
    ps_style(400, legend = FALSE) |>
    plotly::layout(geo = list(bgcolor = "rgba(0,0,0,0)", showframe = FALSE, showcoastlines = FALSE, showland = TRUE,
                              landcolor = "#1A2236", showcountries = TRUE, countrycolor = "#0A0F1E",
                              projection = list(type = "natural earth"), lataxis = list(range = c(-58, 85))))
}

fig_jug_top <- function() {
  df <- countries[15:1, ]
  plotly::plot_ly(df, x = ~n_players, y = ~factor(country, levels = country), type = "bar", orientation = "h",
                  marker = list(color = PURPLE), text = ~paste0(f1(pct_players), "%"), textposition = "outside", cliponaxis = FALSE) |>
    ps_style(400, legend = FALSE) |> plotly::layout(xaxis = list(title = "Jugadores"), yaxis = list(title = "")) |>
    pad_x(max(df$n_players))
}

fig_jug_monthly <- function() {
  m <- head(activity_monthly, -1)  # el último mes está incompleto
  plotly::plot_ly(m) |>
    plotly::add_bars(x = ~month, y = ~trophies, name = "Trofeos ganados", marker = list(color = "#1D6FD1", line = list(width = 0))) |>
    plotly::add_lines(x = ~month, y = ~active_players, name = "Jugadores activos", yaxis = "y2", line = list(color = AMBER, width = 2)) |>
    ps_style(360) |>
    plotly::layout(bargap = 0, yaxis = list(title = "Trofeos"), margin = list(l = 10, r = 60, t = 16, b = 10),
                   yaxis2 = list(title = list(text = "Jugadores activos", standoff = 8), overlaying = "y", side = "right",
                                 showgrid = FALSE, automargin = TRUE, tickfont = list(color = MUTED),
                                 titlefont = list(color = MUTED)))
}

fig_jug_heat <- function() {
  h <- activity_heatmap |> mutate(weekday = factor(weekday, levels = DIAS)) |> arrange(weekday, hour)
  z <- matrix(h$trophies, nrow = 7, byrow = TRUE)
  plotly::plot_ly(z = z, x = sprintf("%02d", 0:23), y = DIAS, type = "heatmap", colorscale = SEQ_SCALE,
                  colorbar = list(thickness = 10, tickfont = list(color = MUTED)),
                  hovertemplate = "%{y} %{x}:00<br>%{z:,} trofeos<extra></extra>") |>
    ps_style(360, legend = FALSE) |>
    plotly::layout(xaxis = list(title = "Hora registrada en el dataset", tickangle = 0, dtick = 2),
                   yaxis = list(autorange = "reversed"))
}

.c1 <- countries[1, ]; .c2 <- countries[2, ]
INTERP_MAP <- sprintf(paste0("La audiencia está concentrada en pocos países: %s suman el %s de los jugadores y los otros ",
                             "%d países se reparten el resto. América y Europa occidental dominan la muestra."),
                      S$top5_countries, S$top5_countries_pct, nrow(countries) - 5)
INTERP_TOP <- sprintf(paste0("%s tiene %s veces más jugadores que el segundo país (%s, %s%%). Por eso, cualquier promedio ",
                             "global de la muestra refleja sobre todo el comportamiento de los jugadores de %s."),
                      .c1$country, f1(.c1$n_players / .c2$n_players), .c2$country, f1(.c2$pct_players), .c1$country)
INTERP_MONTHLY <- sprintf(paste0("Los jugadores activos al mes crecieron de %s en %s a %s en %s%s y bajaron a %s en %s. ",
                                 "Aun así, quien sigue activo gana más trofeos: %s al mes por jugador en %s frente a %s en %s. ",
                                 "Los primeros años tienen menos actividad en parte porque muchos de estos jugadores todavía no usaban PlayStation."),
                          S$active_first_avg, S$active_first_year, S$active_peak_avg, S$active_peak_year,
                          if (S$active_peak_year %in% c("2020", "2021")) " (coincide con la pandemia)" else "",
                          S$active_last_avg, S$active_last_year, S$trophies_per_active_peak, S$active_peak_year,
                          S$trophies_per_active_last, S$active_last_year)
INTERP_HEAT <- sprintf(paste0("El fin de semana concentra el %s de los trofeos (si se jugara igual todos los días sería 28.6%%). ",
                              "La actividad cae al mínimo a las %s y sube desde la tarde hasta su máximo a las %s ",
                              "(hora del dataset, probablemente UTC: es la tarde-noche en América)."),
                       S$weekend_pct, S$low_hour, S$peak_hour)

page_jugadores <- function() tagList(
  section_header("Jugadores", "Desde dónde y cuándo juegan los usuarios de la muestra", "people", c("Objetivo 5", "Objetivo 7")),
  p(class = "card-note", style = "margin-top:-8px;margin-bottom:12px",
    "Esta página no usa los filtros del menú: describe a los jugadores, no a los juegos."),
  grid(cols = "grid-60-40",
    graph_card("globe2", "Jugadores por país", "jug_map", height = 400, interp = INTERP_MAP),
    graph_card("geo-alt", "Top 15 países", "jug_top", height = 400, interp = INTERP_TOP,
               note = "Etiqueta: porcentaje del total de jugadores.")),
  grid(
    graph_card("activity", "Actividad mensual 2008–2024", "jug_monthly", interp = INTERP_MONTHLY,
               note = paste("Trofeos ganados por mes (barras) y jugadores que ganaron al menos uno (línea).",
                            "Se excluye el último mes porque está incompleto.")),
    graph_card("calendar-week", "¿Qué día y a qué hora se juega más?", "jug_heat", interp = INTERP_HEAT,
               note = "Total de trofeos ganados por día de la semana y hora."))
)

# ══ POPULARIDAD ═════════════════════════════════════════════════════════════
fig_pop_library <- function() {
  lib <- player_library$library_size[player_library$library_size <= 1000]
  med <- median(player_library$library_size)
  plotly::plot_ly(x = lib, type = "histogram", nbinsx = 50, marker = list(color = AMBER)) |>
    ps_style(340, legend = FALSE) |>
    plotly::layout(xaxis = list(title = "Juegos en la biblioteca"), yaxis = list(title = "Jugadores")) |>
    vline(med, paste("Mediana:", round(med)))
}

INTERP_LIBRARY <- sprintf(paste0("La distribución está sesgada a la derecha: la mitad de los jugadores tiene %s juegos o menos y ",
                                 "el 90%% tiene %s o menos, pero unos pocos coleccionistas llegan a %s. Por eso se usa la mediana ",
                                 "y no el promedio para describir al jugador típico."),
                          S$median_library, S$p90_library, S$max_library)

page_popularidad <- function() tagList(
  section_header("Popularidad", "Qué juegos compran realmente los jugadores de la muestra", "fire", "Objetivo 6"),
  grid(
    graph_card("trophy", "Top 15 juegos con más compradores", "pop_top", height = 420, dynamic = TRUE,
               note = sprintf("Compradores = jugadores de la muestra (%s bibliotecas públicas) que tienen el juego.", S$n_players_library)),
    graph_card("crosshair", "Precio vs. compradores", "pop_scatter", height = 420, dynamic = TRUE,
               note = "Cada punto es un juego. Eje vertical en escala logarítmica.")),
  grid(
    graph_card("tags", "Compradores medianos por género (12 más comunes)", "pop_genre", dynamic = TRUE),
    graph_card("collection", "Tamaño de las bibliotecas", "pop_library", interp = INTERP_LIBRARY,
               note = "No usa filtros. Se cortan bibliotecas de más de 1.000 juegos para ver mejor la forma."))
)

top_owned <- function(df) {
  top <- df |> arrange(desc(n_owners)) |> head(15) |> arrange(n_owners)
  top$label <- paste0(substr(top$title, 1, 38), " (", top$platform, ")")
  top
}

fig_pop_top <- function(top) {
  plotly::plot_ly(top, x = ~n_owners, y = ~factor(label, levels = unique(label)), color = ~platform, colors = PLATFORM_COLORS,
                  type = "bar", orientation = "h", text = ~fmt_int(n_owners), textposition = "outside", cliponaxis = FALSE) |>
    ps_style(420) |> plotly::layout(xaxis = list(title = "Compradores"), yaxis = list(title = "")) |>
    pad_x(max(top$n_owners))
}

interp_pop_top <- function(df, top) {
  best <- top[nrow(top), ]; np <- vcount(top$platform)
  plat_txt <- if (np[1] == nrow(top)) sprintf("Los %d son de %s.", nrow(top), names(np)[1])
              else sprintf("%d de los %d son de %s.", np[1], nrow(top), names(np)[1])
  txt <- sprintf("%s lidera con %s compradores (%s%% de las bibliotecas). %s", best$title, fmt_int(best$n_owners),
                 f1(best$n_owners / N_LIB * 100), plat_txt)
  m_top <- median(top$price_usd, na.rm = TRUE); m_all <- median(df$price_usd, na.rm = TRUE)
  if (!is.na(m_top) && !is.na(m_all))
    txt <- sprintf("%s Su precio mediano actual es %s, frente a %s de todos los juegos filtrados%s", txt, usd(m_top), usd(m_all),
                   if (m_top > m_all) ": los más comprados cuestan más que el juego típico, no son los más baratos." else ".")
  txt
}

fig_pop_scatter <- function(sc) {
  plotly::plot_ly(sc, x = ~price_usd, y = ~n_owners, color = ~platform, colors = PLATFORM_COLORS, type = "scatter",
                  mode = "markers", text = ~title, marker = list(size = 6, opacity = 0.55),
                  hovertemplate = "<b>%{text}</b><br>$%{x:.2f} · %{y:,} compradores<extra></extra>") |>
    ps_style(420) |>
    plotly::layout(xaxis = list(title = "Precio (USD)", tickprefix = "$"), yaxis = list(title = "Compradores", type = "log", dtick = 1))
}

interp_pop_scatter <- function(sc) {
  if (nrow(sc) < 10) return("Hay muy pocos juegos para medir la relación con estos filtros.")
  rho <- cor(rank(sc$price_usd), rank(sc$n_owners)); a <- abs(rho)
  fuerza <- if (a < 0.1) "prácticamente nula" else if (a < 0.3) "débil" else if (a < 0.6) "moderada" else "fuerte"
  txt <- sprintf("La correlación de Spearman entre precio y compradores es %s (%s, %s).", f2(rho), fuerza,
                 if (rho > 0) "positiva" else "negativa")
  if (a < 0.3) txt <- paste(txt, "Hay juegos de menos de $5 con miles de compradores y juegos de $60 con muy pocos:",
                           "el precio casi no explica la popularidad; pesan más la marca, el género o la publicidad.")
  txt
}

genre_owners <- function(df) {
  df |> filter(genre_main %in% top_genres(df, 12)) |> group_by(genre_main) |>
    summarise(n_owners = median(n_owners), .groups = "drop") |> arrange(n_owners)
}

fig_pop_genre <- function(gm) {
  plotly::plot_ly(gm, x = ~n_owners, y = ~factor(genre_main, levels = genre_main), type = "bar", orientation = "h",
                  marker = list(color = BLUE), text = ~fmt_int(n_owners), textposition = "outside", cliponaxis = FALSE) |>
    ps_style(legend = FALSE) |> plotly::layout(xaxis = list(title = "Compradores (mediana)"), yaxis = list(title = "")) |>
    pad_x(max(gm$n_owners))
}

interp_pop_genre <- function(df, gm) {
  hi <- gm[nrow(gm), ]; lo <- gm[1, ]
  if (nrow(gm) == 1) return(sprintf("Con un solo género no hay comparación: cada juego de %s tiene en mediana %s compradores.",
                                    hi$genre_main, fmt_int(hi$n_owners)))
  txt <- sprintf("%s tiene la mediana más alta (%s compradores por juego) y %s la más baja (%s).",
                 hi$genre_main, fmt_int(hi$n_owners), lo$genre_main, fmt_int(lo$n_owners))
  biggest <- top_genres(df, 1)
  if (biggest %in% gm$genre_main)
    txt <- sprintf("%s Que un género tenga muchos juegos no significa que cada uno se compre más: %s es el género con más juegos y su mediana es %s.",
                   txt, biggest, fmt_int(gm$n_owners[gm$genre_main == biggest]))
  txt
}

# ══ LOGROS ══════════════════════════════════════════════════════════════════
fig_log_earn <- function() {
  df <- rarity_summary[match(RARITY_ORDER, rarity_summary$rarity), ]
  df$rarity <- factor(df$rarity, levels = RARITY_ORDER)
  plotly::plot_ly(df, x = ~rarity, y = ~median_earn_rate, color = ~rarity, colors = RARITY_COLORS, type = "bar",
                  text = ~paste0(f1(median_earn_rate), "%"), textposition = "outside", cliponaxis = FALSE, customdata = ~n_trophies,
                  hovertemplate = "%{x}: %{y:.1f}% · %{customdata:,} trofeos<extra></extra>") |>
    ps_style(340, legend = FALSE) |>
    plotly::layout(xaxis = list(title = ""), yaxis = list(title = "% de jugadores que lo consigue (mediana)", ticksuffix = "%"))
}

rare_table <- function() {
  df <- head(rarest_trophies, 15)
  tabla <- data.frame(t = df$trophy, g = paste0(df$game, " (", df$platform, ")"), r = df$rarity,
                      e = paste(fmt_int(df$earners), "/", fmt_int(df$game_players)), p = paste0(f2(df$earn_rate), "%"))
  ps_table(tabla, c("Trofeo", "Juego", "Rareza", "Lo tienen", "%"), num_cols = 4:5, max_height = "340px")
}

.plat <- as.numeric(sub("%", "", S$earn_platinum))
INTERP_EARN <- sprintf(paste0("Cuanto más alta la rareza, menos jugadores la consiguen: Bronze %s, Silver %s, Gold %s y Platinum %s. ",
                              "La clasificación de Sony refleja bien la dificultad real. En el juego típico, 1 de cada %d jugadores llega al Platinum."),
                       S$earn_bronze, S$earn_silver, S$earn_gold, S$earn_platinum, round(100 / .plat))
.t15 <- head(rarest_trophies, 15); .e0 <- min(.t15$earners); .e1 <- max(.t15$earners)
.quien <- if (.e0 == .e1 && .e0 == 1) paste(.e0, "jugador") else if (.e0 == .e1) paste(.e0, "jugadores") else sprintf("entre %d y %d jugadores", .e0, .e1)
INTERP_RARE <- sprintf(paste0("Ninguno de estos trofeos supera el %s%%: de cientos de jugadores con algún trofeo del juego, solo %s ",
                              "lo consiguió. El más raro es %s. No todos son Platinum: algunos Bronze o Gold son más difíciles que el Platinum de otros juegos."),
                       f2(max(.t15$earn_rate)), .quien, S$rarest_trophy)

page_logros <- function() tagList(
  section_header("Logros", "Qué tan difíciles son los juegos según sus trofeos", "award", "Objetivo 8"),
  grid(
    graph_card("bar-chart", "Trofeos por juego", "log_hist", dynamic = TRUE,
               note = "Juegos con al menos un trofeo; eje cortado en 100 trofeos."),
    graph_card("stack", "Composición de trofeos por género (12 más comunes)", "log_mix", dynamic = TRUE,
               note = "Porcentaje de los trofeos de cada género según su rareza.")),
  grid(
    graph_card("percent", "¿Cuántos jugadores consiguen cada tipo de trofeo?", "log_earn", interp = INTERP_EARN,
               note = paste("Rareza real: jugadores que ganaron el trofeo entre los que ganaron algún trofeo del",
                            "juego (juegos con 200+ jugadores). No usa filtros.")),
    ps_card("gem", "Los 15 trofeos más raros", rare_table(), interp = INTERP_RARE, note = "No usa filtros."))
)

fig_log_hist <- function(df) {
  med <- median(df$n_trophies)
  plotly::plot_ly(x = df$n_trophies[df$n_trophies <= 100], type = "histogram", nbinsx = 50, marker = list(color = BLUE)) |>
    ps_style(legend = FALSE) |>
    plotly::layout(xaxis = list(title = "Trofeos del juego"), yaxis = list(title = "Juegos")) |>
    vline(med, paste("Mediana:", round(med)))
}

interp_log_hist <- function(df) {
  tb <- table(df$n_trophies); mode <- as.numeric(names(tb)[which.max(tb)]); k <- max(tb)
  sprintf("Un juego tiene en mediana %s trofeos. El valor más repetido es %s (%s juegos): muchos juegos pequeños usan listas cortas y casi iguales. El %s%% de los juegos tiene Platinum.",
          round(median(df$n_trophies)), mode, fmt_int(k), f1(mean(df$has_platinum) * 100))
}

rarity_mix <- function(df) {
  df |> filter(genre_main %in% top_genres(df, 12)) |> group_by(genre_main) |>
    summarise(Bronze = sum(n_bronze), Silver = sum(n_silver), Gold = sum(n_gold), Platinum = sum(n_platinum), .groups = "drop") |>
    mutate(tot = Bronze + Silver + Gold + Platinum, across(Bronze:Platinum, ~ .x / tot * 100)) |> select(-tot)
}

fig_log_mix <- function(mix) {
  ord <- mix$genre_main[order(mix$Bronze)]
  long <- mix |> pivot_longer(-genre_main, names_to = "rarity", values_to = "pct") |>
    mutate(rarity = factor(rarity, levels = RARITY_ORDER), genre_main = factor(genre_main, levels = ord))
  plotly::plot_ly(long, x = ~pct, y = ~genre_main, color = ~rarity, colors = RARITY_COLORS, type = "bar", orientation = "h") |>
    ps_style() |> plotly::layout(barmode = "stack", legend = list(traceorder = "normal"),
                                 xaxis = list(title = "% de los trofeos", ticksuffix = "%", range = c(0, 100)),
                                 yaxis = list(title = ""))
}

interp_log_mix <- function(df, mix) {
  hard <- setNames(mix$Gold + mix$Platinum, mix$genre_main); hard <- sort(hard)
  if (length(hard) == 1) return(sprintf("Con un solo género no hay comparación: el %s%% de sus trofeos son Gold o Platinum.", f1(hard)))
  g_hi <- names(hard)[length(hard)]; g_lo <- names(hard)[1]
  med <- df |> group_by(genre_main) |> summarise(m = median(n_trophies), .groups = "drop")
  m_hi <- med$m[med$genre_main == g_hi]; m_lo <- med$m[med$genre_main == g_lo]
  txt <- sprintf("%s tiene la mayor proporción de trofeos Gold + Platinum (%s%%) y %s la menor (%s%%).",
                 g_hi, f1(hard[length(hard)]), g_lo, f1(hard[1]))
  if (m_hi < m_lo) txt <- sprintf("%s Esto no significa que %s sea más difícil: sus juegos tienen listas más cortas (%s trofeos en mediana frente a %s), así que cada trofeo vale más.",
                                  txt, g_hi, round(m_hi), round(m_lo))
  txt
}

# ══ EXPLORADOR ══════════════════════════════════════════════════════════════
page_explorador <- function() tagList(
  section_header("Explorador", "Busca y ordena cualquier juego del catálogo limpio", "search"),
  uiOutput("exp_count", class = "card-note", style = "margin-bottom:8px"),
  ps_card("table", "Catálogo (usa los filtros del menú; escribe en la fila de arriba de cada columna para buscar)",
          DT::DTOutput("exp_table"))
)
