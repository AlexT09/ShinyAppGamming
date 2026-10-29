# data.R — Carga de las tablas procesadas por el ETL (data/processed/).
# Son los mismos archivos que usa el dashboard Dash, así que los números coinciden.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

.candidates <- c(Sys.getenv("PS_DATA_DIR"), "data/processed", "../data/processed", "../../data/processed")
DATA_DIR <- .candidates[nzchar(.candidates) & file.exists(file.path(.candidates, "games_clean.csv"))][1]
if (is.na(DATA_DIR)) stop("No encuentro data/processed/. Copia data/processed/ del repo DashAppGamming (o corre su ETL).")

rd <- function(name) readr::read_csv(file.path(DATA_DIR, name), show_col_types = FALSE, progress = FALSE)

games <- rd("games_clean.csv")
game_genres <- rd("game_genres.csv")
snapshots <- rd("prices_snapshots.csv")
countries <- rd("countries.csv")
player_library <- rd("player_library.csv")
activity_monthly <- rd("activity_monthly.csv") |> mutate(month = as.Date(paste0(month, "-01")))
activity_heatmap <- rd("activity_heatmap.csv")
rarity_summary <- rd("rarity_summary.csv")
rarest_trophies <- rd("rarest_trophies.csv")
etl_quality <- rd("etl_quality.csv")
missing_raw <- rd("missing_raw.csv")
fx_rates <- rd("fx_rates.csv")
.summary <- rd("summary.csv")
S <- as.list(setNames(.summary$display, .summary$key))  # cifras clave ya formateadas (mismas que en Dash)

PLATFORMS <- c("Todas", sort(unique(games$platform)))
GENRES <- c("Todos", names(sort(table(games$genre_main), decreasing = TRUE)))
YEAR_MIN <- min(games$year); YEAR_MAX <- max(games$year)
RARITY_ORDER <- c("Bronze", "Silver", "Gold", "Platinum")
CURRENCIES <- c("usd", "eur", "gbp", "jpy", "rub")
TIERS <- c("< $5", "$5–10", "$10–20", "$20–40", "> $40")
N_LIB <- nrow(player_library)

# Filtros globales del menú lateral
filter_games <- function(platform = "Todas", years = NULL, genre = "Todos") {
  df <- games
  if (!is.null(platform) && platform != "Todas") df <- df[df$platform == platform, ]
  if (!is.null(years)) df <- df[df$year >= years[1] & df$year <= years[2], ]
  if (!is.null(genre) && genre != "Todos") df <- df[df$genre_main == genre, ]
  df
}

# Conteo ordenado de mayor a menor (equivalente a value_counts de pandas)
vcount <- function(x) sort(table(x), decreasing = TRUE)
