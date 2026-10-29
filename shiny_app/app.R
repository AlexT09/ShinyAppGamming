# app.R — PS Gaming Analytics 2025 · App Shiny (R + plotly).
#
# Versión en R del dashboard Dash (repo DashAppGamming): mismas páginas,
# mismos gráficos, mismos textos y el mismo CSS (www/ps-theme.css).
# Los archivos de R/ se cargan solos en orden (01_theme, 02_data, 03_textos, 04_analisis).
#
# Ejecutar desde la carpeta del repo:  shiny::runApp("shiny_app")

library(shiny)
library(plotly)
library(DT)

# (ruta, etiqueta, icono, función que arma la página)
MENU <- list(
  "Contexto" = list(
    c("inicio", "Resumen", "speedometer2", "page_resumen"),
    c("introduccion", "Introducción", "book", "page_introduccion"),
    c("objetivos", "Objetivos", "bullseye", "page_objetivos"),
    c("etl", "ETL", "database-gear", "page_etl")),
  "Análisis (EDA)" = list(
    c("catalogo", "Catálogo", "grid-3x3-gap", "page_catalogo"),
    c("precios", "Precios", "currency-dollar", "page_precios"),
    c("jugadores", "Jugadores", "people", "page_jugadores"),
    c("popularidad", "Popularidad", "fire", "page_popularidad"),
    c("logros", "Logros", "award", "page_logros")),
  "Cierre" = list(
    c("explorador", "Explorador", "search", "page_explorador"),
    c("conclusiones", "Conclusiones", "check2-circle", "page_conclusiones"))
)
ROUTES <- setNames(unlist(lapply(MENU, function(g) sapply(g, `[`, 4))), unlist(lapply(MENU, function(g) sapply(g, `[`, 1))))

# Marca como activo el enlace de la página actual (igual que el menú de Dash)
NAV_JS <- "
function psSetActive(){var h=location.hash; if(!h || h==='#/') h='#/inicio';document.querySelectorAll('.nav-item').forEach(function(a){
  a.classList.toggle('active', a.getAttribute('href')===h);});}
window.addEventListener('hashchange', psSetActive); document.addEventListener('DOMContentLoaded', psSetActive);"

sidebar <- function() {
  nav <- lapply(names(MENU), function(group) tagList(
    div(class = "nav-group-title", group),
    lapply(MENU[[group]], function(it) tags$a(class = "nav-item", href = paste0("#/", it[1]), icon_bi(it[3]), it[2]))))
  div(class = "sidebar",
      div(class = "brand",
          div(class = "brand-logo", icon_bi("playstation")),
          div(div(class = "brand-title", "PS Gaming Analytics"), div(class = "brand-sub", "2025 · Shiny"))),
      nav,
      div(class = "nav-group-title", "Filtros globales"),
      div(class = "filters",
          tags$label(class = "filter-label", "Plataforma"),
          selectInput("f_plat", NULL, PLATFORMS, "Todas", width = "100%"),
          tags$label(class = "filter-label", "Género principal"),
          selectInput("f_genre", NULL, GENRES, "Todos", width = "100%"),
          tags$label(class = "filter-label", "Año de lanzamiento"),
          sliderInput("f_year", NULL, YEAR_MIN, YEAR_MAX, c(YEAR_MIN, YEAR_MAX), step = 1, sep = "", width = "100%"),
          p(class = "filter-note", "Aplican a Resumen, Catálogo, Precios, Popularidad, Logros y Explorador.")),
      div(class = "sidebar-footer", "Gaming Profiles 2025 · Kaggle CC0", tags$br(), "Curso de Visualización de Datos"))
}

ui <- bslib::page(
  theme = bslib::bs_theme(version = 5, primary = BLUE),
  title = "PS Gaming Analytics 2025",
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"),
    tags$link(rel = "stylesheet", href = "ps-theme.css"),
    tags$script(HTML(NAV_JS))),
  div(class = "app-shell", sidebar(), tags$main(class = "content", uiOutput("page")))
)

server <- function(input, output, session) {
  path <- reactive({
    h <- sub("^#/?", "", session$clientData$url_hash %||% "")
    if (h %in% names(ROUTES)) h else "inicio"
  })
  output$page <- renderUI(get(ROUTES[[path()]])())

  df <- reactive(filter_games(input$f_plat, input$f_year, input$f_genre))
  priced <- reactive(df()[!is.na(df()$price_usd), ])
  owned <- reactive(df()[df()$n_owners > 0, ])
  with_trophies <- reactive(df()[df()$n_trophies > 0, ])
  safe <- function(d, f, h = 340) if (nrow(d)) f else ps_empty(height = h)

  # Resumen
  output$res_kpis <- renderUI(res_kpis(df()))
  output$res_year <- renderPlotly(safe(df(), fig_res_year(df()), 360))
  output$res_year_interp <- renderUI(if (nrow(df())) interp_children(interp_res_year(df())))

  # Catálogo
  top6 <- reactive(top_genres(df(), 6))
  output$cat_treemap <- renderPlotly(safe(df(), fig_cat_treemap(df()), 400))
  output$cat_publishers <- renderPlotly(safe(df(), fig_cat_publishers(df()), 400))
  output$cat_genre_year <- renderPlotly(safe(df(), fig_cat_genre_year(df(), top6())))
  output$cat_platform_share <- renderPlotly(safe(df(), fig_cat_platform_share(df())))
  output$cat_treemap_interp <- renderUI(if (nrow(df())) interp_children(interp_cat_treemap(df())))
  output$cat_publishers_interp <- renderUI(if (nrow(df())) interp_children(interp_cat_publishers(df())))
  output$cat_genre_year_interp <- renderUI(if (nrow(df())) interp_children(interp_cat_genre_year(df(), top6())))
  output$cat_platform_share_interp <- renderUI(if (nrow(df())) interp_children(interp_cat_platform_share(df())))

  # Precios
  box_d <- reactive(price_box_data(priced()))
  tier_d <- reactive(tier_data(priced()))
  cur_d <- reactive(currency_diffs(priced()))
  year_d <- reactive(year_price_data(priced()))
  changes <- reactive(price_changes(df()))
  output$pre_box <- renderPlotly(safe(box_d(), fig_pre_box(box_d()), 400))
  output$pre_tier <- renderPlotly(safe(priced(), fig_pre_tier(tier_d()), 400))
  output$pre_currency <- renderPlotly(safe(cur_d(), fig_pre_currency(cur_d())))
  output$pre_year <- renderPlotly(safe(year_d(), fig_pre_year(year_d())))
  output$pre_box_interp <- renderUI(if (nrow(box_d())) interp_children(interp_pre_box(box_d())))
  output$pre_tier_interp <- renderUI(if (nrow(priced())) interp_children(interp_pre_tier(priced(), tier_d())))
  output$pre_currency_interp <- renderUI(if (nrow(priced())) interp_children(interp_pre_currency(cur_d())))
  output$pre_year_interp <- renderUI(if (nrow(priced())) interp_children(interp_pre_year(year_d())))
  output$pre_changes <- renderUI(changes()$ui)
  output$pre_changes_interp <- renderUI(interp_children(changes()$interp))

  # Jugadores (fijos)
  output$jug_map <- renderPlotly(fig_jug_map())
  output$jug_top <- renderPlotly(fig_jug_top())
  output$jug_monthly <- renderPlotly(fig_jug_monthly())
  output$jug_heat <- renderPlotly(fig_jug_heat())

  # Popularidad
  top15 <- reactive(top_owned(owned()))
  scat <- reactive(owned()[!is.na(owned()$price_usd), ])
  gen_own <- reactive(genre_owners(owned()))
  output$pop_top <- renderPlotly(safe(owned(), fig_pop_top(top15()), 420))
  output$pop_scatter <- renderPlotly(safe(scat(), fig_pop_scatter(scat()), 420))
  output$pop_genre <- renderPlotly(safe(gen_own(), fig_pop_genre(gen_own())))
  output$pop_library <- renderPlotly(fig_pop_library())
  output$pop_top_interp <- renderUI(if (nrow(owned())) interp_children(interp_pop_top(owned(), top15())))
  output$pop_scatter_interp <- renderUI(if (nrow(owned())) interp_children(interp_pop_scatter(scat())))
  output$pop_genre_interp <- renderUI(if (nrow(gen_own())) interp_children(interp_pop_genre(owned(), gen_own())))

  # Logros
  mix <- reactive(rarity_mix(with_trophies()))
  output$log_hist <- renderPlotly(safe(with_trophies(), fig_log_hist(with_trophies())))
  output$log_mix <- renderPlotly(safe(mix(), fig_log_mix(mix())))
  output$log_earn <- renderPlotly(fig_log_earn())
  output$log_hist_interp <- renderUI(if (nrow(with_trophies())) interp_children(interp_log_hist(with_trophies())))
  output$log_mix_interp <- renderUI(if (nrow(mix())) interp_children(interp_log_mix(with_trophies(), mix())))

  # ETL
  output$etl_missing <- renderPlotly(missing_fig())

  # Explorador
  output$exp_count <- renderUI(sprintf("%s productos con los filtros actuales (ordenados por compradores).", fmt_int(nrow(df()))))
  output$exp_table <- DT::renderDT({
    d <- df()[order(-df()$n_owners), c("title", "platform", "genre_main", "publisher", "year", "price_usd", "n_owners", "n_trophies")]
    DT::datatable(d, rownames = FALSE, filter = "top", class = "compact",
                  colnames = c("Título", "Plataforma", "Género", "Publisher", "Año", "Precio USD", "Compradores", "Trofeos"),
                  options = list(pageLength = 15, scrollX = TRUE, dom = "tip",
                                 language = list(info = "Mostrando _START_ a _END_ de _TOTAL_ productos",
                                                 infoEmpty = "Sin resultados", infoFiltered = "(de _MAX_ en total)",
                                                 zeroRecords = "No hay juegos con esos filtros",
                                                 paginate = list(previous = "Anterior", `next` = "Siguiente")))) |>
      DT::formatCurrency("price_usd", "$") |> DT::formatRound("n_owners", 0)
  })
}

shinyApp(ui, server)
