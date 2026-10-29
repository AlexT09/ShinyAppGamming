# theme.R — Colores, estilo de gráficos y bloques de interfaz.
# Es la versión en R de dash_app/components.py: mismos colores y mismas clases CSS
# (www/ps-theme.css), para que Shiny y Dash se vean iguales.

BLUE <- "#0070D1"; ACCENT <- "#00AAFF"; GREEN <- "#22C55E"; AMBER <- "#F59E0B"
RED <- "#EF4444"; PURPLE <- "#A78BFA"
TEXT <- "#F0F4FF"; MUTED <- "#8B9BB4"; BORDER <- "#1E2D4A"; CARD <- "#111827"

PLATFORM_COLORS <- c("PS3" = "#8B9BB4", "PS4" = BLUE, "PS5" = "#7FD3F7", "PS Vita" = PURPLE)
RARITY_COLORS <- c("Bronze" = "#CD7F32", "Silver" = "#C0C7D1", "Gold" = "#F5B700", "Platinum" = "#7FD3F7")
CURRENCY_COLORS <- c("USD" = BLUE, "EUR" = GREEN, "GBP" = PURPLE, "JPY" = AMBER, "RUB" = RED)
SEQ_SCALE <- list(c(0, "#13294B"), c(0.5, BLUE), c(1, ACCENT))
CAT_COLORS <- c("#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A", "#19D3F3")  # plotly por defecto

`%||%` <- function(a, b) if (is.null(a)) b else a

# Estilo oscuro común a todos los gráficos
ps_style <- function(p, height = 340, legend = TRUE) {
  axis <- list(gridcolor = BORDER, linecolor = BORDER, zerolinecolor = BORDER,
               titlefont = list(color = MUTED), tickfont = list(color = MUTED))
  p |>
    plotly::layout(
      font = list(family = "Inter, Segoe UI, Arial, sans-serif", size = 12, color = MUTED),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 10, r = 16, t = 16, b = 10),
      showlegend = legend,
      legend = list(orientation = "h", yanchor = "bottom", y = 1.0, xanchor = "left", x = 0,
                    bgcolor = "rgba(0,0,0,0)", font = list(color = MUTED), title = list(text = "")),
      hoverlabel = list(bgcolor = CARD, bordercolor = BORDER, font = list(color = TEXT)),
      xaxis = axis, yaxis = axis
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

ps_empty <- function(msg = "Sin datos para los filtros elegidos", height = 340) {
  plotly::plot_ly(type = "scatter", mode = "markers") |>
    plotly::layout(annotations = list(text = msg, showarrow = FALSE, font = list(color = MUTED, size = 13)),
                   xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)) |>
    ps_style(height, legend = FALSE)
}

# ── Bloques de interfaz (misma estructura HTML que Dash) ─────────────────────
icon_bi <- function(name) tags$i(class = paste0("bi bi-", name))

section_header <- function(title, subtitle, icon, objectives = NULL) {
  div(class = "section-header",
      icon_bi(icon),
      div(h4(title), p(subtitle)),
      div(class = "objective-tags", lapply(objectives, function(t) span(class = "tag", t))))
}

interp_children <- function(text) tagList(strong("Interpretación: "), text)

ps_card <- function(icon, title, body, note = NULL, interp = NULL, interp_id = NULL) {
  bloque <- if (!is.null(interp_id)) uiOutput(interp_id, class = "interp")
            else if (!is.null(interp)) div(class = "interp", interp_children(interp))
  div(class = "ps-card",
      div(class = "ps-card-header", icon_bi(icon), span(title)),
      div(class = "ps-card-body", body, if (!is.null(note)) p(class = "card-note", note), bloque))
}

# dynamic = TRUE crea el bloque "<id>_interp" que llena el servidor (igual que en Dash).
graph_card <- function(icon, title, id, note = NULL, height = 340, interp = NULL, dynamic = FALSE) {
  ps_card(icon, title, plotly::plotlyOutput(id, height = paste0(height, "px")), note, interp,
          if (dynamic) paste0(id, "_interp") else NULL)
}

# Deja espacio a la derecha para que no se corten las etiquetas de barras horizontales.
pad_x <- function(p, max_value, factor = 1.18) plotly::layout(p, xaxis = list(range = c(0, max_value * factor)))

kpi <- function(label, value, sub = "", color = BLUE) {
  div(class = "kpi-box", style = paste0("--kpi-color:", color),
      div(class = "kpi-label", label), div(class = "kpi-value", value), div(class = "kpi-sub", sub))
}

grid <- function(..., cols = "grid-2") div(class = paste("grid", cols), ...)

insight <- function(title, text, warn = FALSE) {
  div(class = if (warn) "insight-box warn" else "insight-box", strong(paste0(title, " ")), text)
}

step <- function(num, title, desc, color = BLUE) {
  div(class = "step", style = paste0("--step-color:", color),
      div(class = "step-num", num), div(class = "step-title", title), p(class = "step-desc", desc))
}

spacer <- function() div(style = "height:14px")

fmt_int <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_usd <- function(x) paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
fmt_pct <- function(x, d = 1) paste0(formatC(x, format = "f", digits = d), "%")

# Tabla HTML con la clase .ps-table. num_cols = índices de columnas numéricas (alineadas a la derecha).
ps_table <- function(df, headers, num_cols = integer(0), max_height = NULL) {
  th <- lapply(seq_along(headers), function(i) tags$th(class = if (i %in% num_cols) "num", headers[i]))
  rows <- lapply(seq_len(nrow(df)), function(r) {
    tags$tr(lapply(seq_along(df), function(i) {
      v <- df[[i]][[r]]
      if (inherits(v, "shiny.tag")) tags$td(class = if (i %in% num_cols) "num", v)
      else tags$td(class = if (i %in% num_cols) "num", as.character(v))
    }))
  })
  tbl <- tags$table(class = "ps-table", tags$thead(tags$tr(th)), tags$tbody(rows))
  div(style = paste0("overflow-x:auto;", if (!is.null(max_height)) paste0("max-height:", max_height, ";overflow-y:auto;")), tbl)
}
