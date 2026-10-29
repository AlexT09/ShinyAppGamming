"""
etl_playstation.py — ETL del dataset Gaming Profiles 2025 (esquema PlayStation).

Lee las 6 tablas crudas de Kaggle (games, prices, players, purchased_games,
achievements, history), las limpia y genera tablas pequeñas listas para
visualizar en data/processed/. El dashboard Dash y la app Shiny leen estos
mismos archivos, así que los dos muestran exactamente los mismos números.

Uso:
    python etl/etl_playstation.py                    # busca los CSV crudos solo
    python etl/etl_playstation.py --raw "ruta/playstation"

Los archivos grandes (history.csv ~800 MB, purchased_games.csv ~75 MB) se leen
por partes para no superar ~1 GB de RAM.
"""
from __future__ import annotations

import argparse
import ast
import os
import re
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

from build_summary import build_summary

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "data" / "processed"

RAW_CANDIDATES = [
    ROOT / "data" / "raw" / "playstation",
    ROOT / "archive (19)" / "playstation",
    ROOT / "archive" / "playstation",
]

# Tasas de cambio de referencia (aprox. 24-feb-2025, fecha de las capturas de precio).
# 1 unidad de la moneda local = X USD. Se usan solo para comparar monedas entre sí.
FX_TO_USD = {"usd": 1.0, "eur": 1.05, "gbp": 1.26, "jpy": 1 / 149.5, "rub": 1 / 88.5}

HISTORY_CHUNK = 2_000_000
MIN_PLAYERS_RARITY = 200      # juegos con al menos N jugadores para medir rareza real
DATE_MIN, DATE_MAX = pd.Timestamp("2006-11-01"), pd.Timestamp("2025-03-31")  # lanzamiento PS3 → fin del dataset

QUALITY: list[dict] = []
MISSING: list[pd.DataFrame] = []


# ──────────────────────────────────────────────────────────────────────────────
# Utilidades
# ──────────────────────────────────────────────────────────────────────────────
def log(tabla: str, paso: str, antes: int, despues: int, detalle: str) -> None:
    QUALITY.append({"tabla": tabla, "paso": paso, "filas_antes": antes,
                    "filas_despues": despues, "detalle": detalle})
    print(f"  [{tabla}] {paso}: {antes:,} → {despues:,}  ({detalle})")


def missing(tabla: str, df: pd.DataFrame | None = None, na_counts: pd.Series | None = None,
            n_rows: int | None = None) -> None:
    if df is not None:
        pct = df.isna().mean() * 100
    else:
        pct = na_counts / n_rows * 100
    MISSING.append(pd.DataFrame({"tabla": tabla, "columna": pct.index, "pct_faltante": pct.round(2).values}))


def parse_list(value) -> list[str]:
    """Convierte "['Action', 'Indie']" en ['Action', 'Indie']."""
    if not isinstance(value, str) or not value.strip():
        return []
    try:
        items = ast.literal_eval(value)
        if isinstance(items, (list, tuple)):
            return [str(i).strip() for i in items if str(i).strip()]
    except (ValueError, SyntaxError):
        pass
    return [s.strip(" '\"") for s in value.strip("[]").split(",") if s.strip(" '\"")]


# Sinónimos de género que el dataset escribe de varias formas.
GENRE_SYNONYMS = {
    "rpg": "Role Playing", "role-playing games (rpg)": "Role Playing",
    "fps": "First Person Shooter",
    "music/rhythm": "Music & Rhythm", "music+": "Music & Rhythm",
    "party!": "Party",
    "massively multiplayer": "MMO",
    "multi-player online battle arena": "MOBA",
    "educational": "Educational & Trivia", "education": "Educational & Trivia",
    "❓ quiz ❓": "Educational & Trivia",
}


def build_genre_map(genre_lists: pd.Series) -> dict[str, str]:
    """Une variantes de mayúsculas (Strategy/strategy) y sinónimos (RPG/Role Playing).
    La forma canónica es la escritura más frecuente que tenga mayúscula inicial."""
    counts = pd.Series([x for lst in genre_lists for x in lst]).value_counts()
    canon: dict[str, str] = {}
    for name in counts.index:  # de más a menos frecuente
        key = name.lower()
        if key not in canon or (not canon[key][:1].isupper() and name[:1].isupper()):
            canon[key] = name
    out = {}
    for name in counts.index:
        key = name.lower()
        target = GENRE_SYNONYMS.get(key, canon[key])
        out[name] = target if target[:1].isupper() else target.title()
    return out


def find_raw(arg: str | None) -> Path:
    candidates = [Path(arg)] if arg else []
    if os.environ.get("PS_RAW_DIR"):
        candidates.append(Path(os.environ["PS_RAW_DIR"]))
    candidates += RAW_CANDIDATES
    for c in candidates:
        if (c / "games.csv").exists():
            return c
    raise FileNotFoundError(
        "No encontré los CSV crudos de PlayStation. Descarga el dataset de Kaggle y "
        "pasa la ruta con --raw o colócalo en data/raw/playstation/.")


# ──────────────────────────────────────────────────────────────────────────────
# 1. games
# ──────────────────────────────────────────────────────────────────────────────
def etl_games(raw: Path) -> tuple[pd.DataFrame, pd.DataFrame]:
    print("1) games.csv")
    g = pd.read_csv(raw / "games.csv", encoding="utf-8")
    missing("games", g)
    n0 = len(g)

    g = g.drop_duplicates(subset="gameid")
    log("games", "Duplicados por gameid", n0, len(g), "se conserva una fila por juego")

    g["title"] = g["title"].astype(str).str.strip()
    g["release_date"] = pd.to_datetime(g["release_date"], errors="coerce")
    n1 = len(g)
    g = g[g["release_date"].between(DATE_MIN, DATE_MAX)]
    log("games", "Fecha de lanzamiento válida", n1, len(g), "fechas nulas o fuera de 2006–2025")
    g["year"] = g["release_date"].dt.year.astype(int)

    genres = g["genres"].map(parse_list)
    gmap = build_genre_map(genres)
    genres = genres.map(lambda l: list(dict.fromkeys(gmap[x] for x in l)))  # normaliza y quita repetidos
    n_raw, n_norm = len(gmap), len(set(gmap.values()))
    g["n_genres"] = genres.str.len()
    g["genre_main"] = genres.map(lambda l: l[0] if l else "Sin género")
    g["genres"] = genres.map(lambda l: "|".join(l) if l else "Sin género")
    g["publisher"] = g["publishers"].map(parse_list).map(lambda l: l[0] if l else "Desconocido")
    g["developer"] = g["developers"].map(parse_list).map(lambda l: l[0] if l else "Desconocido")
    langs = g["supported_languages"].map(parse_list)
    g["n_languages"] = langs.str.len().where(g["supported_languages"].notna())
    log("games", "Listas de texto a columnas", len(g), len(g),
        f"géneros, publisher, developer e idiomas; {int((g['genre_main'] == 'Sin género').sum())} juegos sin género")
    log("games", "Normalización de géneros", n_raw, n_norm,
        "filas = etiquetas de género distintas; se unen mayúsculas (Strategy/strategy) y sinónimos (RPG/Role Playing)")

    g["dup_title_platform"] = g.duplicated(subset=["title", "platform"], keep=False)
    log("games", "Ediciones regionales (mismo título y plataforma)", len(g), len(g),
        f"{int(g['dup_title_platform'].sum()):,} filas marcadas, no se eliminan: son productos distintos de la tienda")

    gg = g[["gameid", "genres"]].assign(genre=g["genres"].str.split("|")).explode("genre")[["gameid", "genre"]]
    log("games", "Expansión por género", len(g), len(gg), "una fila por juego y género (tabla game_genres)")

    cols = ["gameid", "title", "platform", "genre_main", "genres", "n_genres", "publisher",
            "developer", "release_date", "year", "n_languages", "dup_title_platform"]
    return g[cols].reset_index(drop=True), gg.reset_index(drop=True)


# ──────────────────────────────────────────────────────────────────────────────
# 2. prices
# ──────────────────────────────────────────────────────────────────────────────
def etl_prices(raw: Path, games: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    print("2) prices.csv")
    p = pd.read_csv(raw / "prices.csv")
    missing("prices", p)
    cur = list(FX_TO_USD)
    n0 = len(p)

    p = p.drop_duplicates()
    log("prices", "Filas duplicadas", n0, len(p), "capturas idénticas")
    for c in cur:
        p.loc[p[c] <= 0, c] = np.nan
    n1 = len(p)
    p = p.dropna(subset=cur, how="all")
    log("prices", "Precios inválidos", n1, len(p), "0 o negativos pasan a nulo; se quitan filas sin ningún precio")
    n2 = len(p)
    p = p[p["gameid"].isin(games["gameid"])]
    log("prices", "Solo juegos del catálogo", n2, len(p), "prices incluye juegos que no están en games.csv")
    p["date_acquired"] = pd.to_datetime(p["date_acquired"])
    p = p.sort_values(["gameid", "date_acquired"])

    # Precio vigente = última captura de cada juego. Evita contar un juego 2–3 veces.
    last = p.groupby("gameid").tail(1).set_index("gameid")[cur].add_prefix("price_")
    first_usd = p.dropna(subset=["usd"]).groupby("gameid")["usd"].first()
    last_usd = p.dropna(subset=["usd"]).groupby("gameid")["usd"].last()
    n_snap = p.groupby("gameid").size()
    change = pd.DataFrame({"usd_first": first_usd, "usd_last": last_usd})
    change["price_change_pct"] = ((change["usd_last"] / change["usd_first"] - 1) * 100).round(2)
    last = last.join(change[["price_change_pct"]]).join(n_snap.rename("n_snapshots"))
    log("prices", "Precio vigente por juego", len(p), len(last),
        f"última de {p['date_acquired'].nunique()} capturas ({p['date_acquired'].min():%d-%m-%Y} a {p['date_acquired'].max():%d-%m-%Y})")

    for c in cur:
        last[f"price_{c}_in_usd"] = (last[f"price_{c}"] * FX_TO_USD[c]).round(2)

    usd = last["price_usd"].dropna()
    q1, q3 = usd.quantile([.25, .75])
    upper = q3 + 1.5 * (q3 - q1)
    last["price_outlier"] = last["price_usd"] > upper
    log("prices", "Atípicos por IQR (solo marca)", len(last), len(last),
        f"{int(last['price_outlier'].sum()):,} juegos por encima de ${upper:.2f}; se conservan porque son precios reales (AAA)")

    snaps = p.rename(columns={"date_acquired": "date"})[["gameid", "date"] + cur]
    return last.reset_index(), snaps.reset_index(drop=True)


# ──────────────────────────────────────────────────────────────────────────────
# 3. achievements
# ──────────────────────────────────────────────────────────────────────────────
def etl_achievements(raw: Path, games: pd.DataFrame) -> pd.DataFrame:
    print("3) achievements.csv")
    a = pd.read_csv(raw / "achievements.csv", usecols=["achievementid", "gameid", "title", "rarity"])
    missing("achievements", a)
    n0 = len(a)
    a = a.drop_duplicates(subset="achievementid").dropna(subset=["gameid", "rarity"])
    log("achievements", "Duplicados y nulos", n0, len(a), "un registro por trofeo")
    n1 = len(a)
    a = a[a["gameid"].isin(games["gameid"])]
    log("achievements", "Solo juegos del catálogo", n1, len(a), "trofeos de juegos sin ficha en games.csv")
    return a.reset_index(drop=True)


# ──────────────────────────────────────────────────────────────────────────────
# 4. purchased_games (por partes)
# ──────────────────────────────────────────────────────────────────────────────
def etl_purchases(raw: Path, max_gameid: int) -> tuple[np.ndarray, pd.DataFrame]:
    print("4) purchased_games.csv (por partes)")
    owners = np.zeros(max_gameid + 1, dtype=np.int64)
    libs, n_rows, n_na = [], 0, 0
    digits = re.compile(r"\d+")
    for chunk in pd.read_csv(raw / "purchased_games.csv", chunksize=5_000):
        n_rows += len(chunk)
        n_na += int(chunk["library"].isna().sum())
        chunk = chunk.dropna()
        for pid, lib in zip(chunk["playerid"], chunk["library"]):
            ids = np.unique(np.fromiter(map(int, digits.findall(lib)), dtype=np.int64))
            libs.append((pid, len(ids)))
            ids = ids[ids <= max_gameid]
            owners[ids] += 1
    missing("purchased_games", na_counts=pd.Series({"playerid": 0, "library": n_na}), n_rows=n_rows)
    lib = pd.DataFrame(libs, columns=["playerid", "library_size"]).drop_duplicates("playerid")
    log("purchased_games", "Bibliotecas a conteos", n_rows, len(lib),
        "cada lista de juegos se convierte en tamaño de biblioteca y dueños por juego")
    return owners, lib


# ──────────────────────────────────────────────────────────────────────────────
# 5. history (por partes, ~800 MB)
# ──────────────────────────────────────────────────────────────────────────────
def etl_history(raw: Path, valid_achievements: set[str]) -> dict:
    print("5) history.csv (por partes, puede tardar varios minutos)")
    month_trophies = pd.Series(dtype="int64")
    heat = np.zeros((7, 24), dtype=np.int64)
    ach_counts = pd.Series(dtype="int64")
    player_trophies = pd.Series(dtype="int64")
    player_month, player_game = [], []
    n_rows = n_valid = 0
    na = pd.Series(0, index=["playerid", "achievementid", "date_acquired"])
    t0 = time.time()

    for i, ch in enumerate(pd.read_csv(raw / "history.csv", chunksize=HISTORY_CHUNK,
                                       dtype={"playerid": "int64", "achievementid": "string"})):
        n_rows += len(ch)
        na += ch.isna().sum()
        ch = ch.dropna().drop_duplicates(subset=["playerid", "achievementid"])
        ch["date"] = pd.to_datetime(ch["date_acquired"], format="%Y-%m-%d %H:%M:%S", errors="coerce")
        ch = ch[ch["date"].between(DATE_MIN, DATE_MAX)]
        ch = ch[ch["achievementid"].isin(valid_achievements)]
        n_valid += len(ch)

        gid = ch["achievementid"].str.split("_", n=1).str[0].astype("int64").to_numpy()
        pid = ch["playerid"].to_numpy()
        month = (ch["date"].dt.year * 100 + ch["date"].dt.month).to_numpy()

        month_trophies = month_trophies.add(pd.Series(month).value_counts(), fill_value=0)
        np.add.at(heat, (ch["date"].dt.dayofweek.to_numpy(), ch["date"].dt.hour.to_numpy()), 1)
        ach_counts = ach_counts.add(ch["achievementid"].value_counts(), fill_value=0)
        player_trophies = player_trophies.add(pd.Series(pid).value_counts(), fill_value=0)
        player_month.append(np.unique(pid * 1_000_000 + month))
        player_game.append(np.unique(pid * 1_000_000 + gid))
        print(f"    parte {i + 1}: {n_rows:,} filas leídas ({time.time() - t0:.0f}s)")

    missing("history", na_counts=na, n_rows=n_rows)
    log("history", "Limpieza de actividad", n_rows, n_valid,
        "sin nulos, sin trofeos repetidos por jugador, fechas válidas y trofeos de juegos del catálogo")

    pm = np.unique(np.concatenate(player_month))
    pg = np.unique(np.concatenate(player_game))
    active = pd.Series(pm % 1_000_000).value_counts()
    players_per_game = pd.Series(pg % 1_000_000).value_counts()
    log("history", "Agregación por mes", n_valid, len(month_trophies),
        "filas finales = meses con actividad; también mapa día×hora y rareza real por trofeo")
    return {"month_trophies": month_trophies, "active": active, "heat": heat,
            "ach_counts": ach_counts, "players_per_game": players_per_game,
            "player_trophies": player_trophies}


# ──────────────────────────────────────────────────────────────────────────────
# 6. players
# ──────────────────────────────────────────────────────────────────────────────
def iso3(name: str) -> str | None:
    import pycountry
    manual = {"Türkiye": "TUR", "Turkey": "TUR", "Korea, Republic of": "KOR",
              "Taiwan, Province of China": "TWN", "Russian Federation": "RUS",
              "Bolivia, Plurinational State of": "BOL", "Hong Kong": "HKG"}
    if name in manual:
        return manual[name]
    try:
        return pycountry.countries.lookup(name).alpha_3
    except LookupError:
        return None


def etl_players(raw: Path) -> pd.DataFrame:
    print("6) players.csv")
    pl = pd.read_csv(raw / "players.csv", encoding="utf-8")
    missing("players", pl)
    n0 = len(pl)
    pl = pl.drop_duplicates(subset="playerid")
    log("players", "Duplicados por playerid", n0, len(pl), "un registro por jugador")
    pl["country"] = pl["country"].fillna("Desconocido").str.strip()
    codes = {c: iso3(c) for c in pl["country"].unique()}
    pl["iso3"] = pl["country"].map(codes)
    sin = sorted(c for c, v in codes.items() if v is None)
    log("players", "País a código ISO-3", len(pl), len(pl),
        f"{len(codes)} países; sin código: {', '.join(sin) if sin else 'ninguno'}")
    return pl[["playerid", "country", "iso3"]]


# ──────────────────────────────────────────────────────────────────────────────
# Main
# ──────────────────────────────────────────────────────────────────────────────
def main() -> None:
    sys.stdout.reconfigure(encoding="utf-8")  # la consola de Windows no imprime "→" sin esto
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--raw", help="carpeta con los CSV crudos de PlayStation")
    raw = find_raw(ap.parse_args().raw)
    OUT.mkdir(parents=True, exist_ok=True)
    print(f"Datos crudos: {raw}\nSalida: {OUT}\n")
    t0 = time.time()

    games, game_genres = etl_games(raw)
    price, snaps = etl_prices(raw, games)
    ach = etl_achievements(raw, games)
    owners, lib = etl_purchases(raw, int(games["gameid"].max()))
    hist = etl_history(raw, set(ach["achievementid"]))
    players = etl_players(raw)

    # ── Tabla maestra de juegos ───────────────────────────────────────────────
    rar = ach.pivot_table(index="gameid", columns="rarity", values="achievementid",
                          aggfunc="count", fill_value=0)
    rar.columns = [f"n_{c.lower()}" for c in rar.columns]
    rar["n_trophies"] = rar.sum(axis=1)
    g = (games.merge(price, on="gameid", how="left")
              .merge(rar.reset_index(), on="gameid", how="left"))
    g["n_owners"] = owners[g["gameid"].to_numpy()]
    g["n_players_history"] = g["gameid"].map(hist["players_per_game"]).fillna(0).astype(int)
    for c in ["n_bronze", "n_silver", "n_gold", "n_platinum", "n_trophies"]:
        g[c] = g[c].fillna(0).astype(int)
    g["has_platinum"] = g["n_platinum"] > 0
    g["price_tier"] = pd.cut(g["price_usd"], [0, 5, 10, 20, 40, np.inf],
                             labels=["< $5", "$5–10", "$10–20", "$20–40", "> $40"], right=False)
    g.to_csv(OUT / "games_clean.csv", index=False)
    game_genres.to_csv(OUT / "game_genres.csv", index=False)
    snaps.to_csv(OUT / "prices_snapshots.csv", index=False)

    # ── Jugadores y países ────────────────────────────────────────────────────
    pl = players.merge(lib, on="playerid", how="left")
    pl["n_trophies"] = pl["playerid"].map(hist["player_trophies"]).fillna(0).astype(int)
    pl[pl["library_size"].notna()].assign(library_size=lambda d: d["library_size"].astype(int)) \
        .to_csv(OUT / "player_library.csv", index=False)
    countries = (pl.groupby(["country", "iso3"], dropna=False)
                   .agg(n_players=("playerid", "size"),
                        n_with_library=("library_size", "count"),
                        median_library=("library_size", "median"),
                        n_trophies=("n_trophies", "sum"))
                   .reset_index().sort_values("n_players", ascending=False))
    countries["pct_players"] = (countries["n_players"] / countries["n_players"].sum() * 100).round(2)
    countries.to_csv(OUT / "countries.csv", index=False)

    # ── Actividad ─────────────────────────────────────────────────────────────
    m = pd.DataFrame({"trophies": hist["month_trophies"].astype(int),
                      "active_players": hist["active"]}).fillna(0).astype(int).sort_index()
    m.index = pd.to_datetime(m.index.astype(str), format="%Y%m").strftime("%Y-%m")
    m.rename_axis("month").reset_index().to_csv(OUT / "activity_monthly.csv", index=False)
    dias = ["Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"]
    heat = pd.DataFrame(hist["heat"], index=dias).rename_axis("weekday").reset_index() \
        .melt(id_vars="weekday", var_name="hour", value_name="trophies")
    heat.to_csv(OUT / "activity_heatmap.csv", index=False)

    # ── Rareza real de trofeos ────────────────────────────────────────────────
    a = ach.copy()
    a["earners"] = a["achievementid"].map(hist["ach_counts"]).fillna(0).astype(int)
    a["game_players"] = a["gameid"].map(hist["players_per_game"]).fillna(0).astype(int)
    a = a[a["game_players"] >= MIN_PLAYERS_RARITY]
    a["earn_rate"] = (a["earners"] / a["game_players"] * 100).round(2)
    a = a.merge(games[["gameid", "title", "platform"]].rename(columns={"title": "game"}), on="gameid")
    a.groupby("rarity").agg(n_trophies=("achievementid", "size"),
                            median_earn_rate=("earn_rate", "median"),
                            mean_earn_rate=("earn_rate", "mean")).round(2) \
        .reset_index().to_csv(OUT / "rarity_summary.csv", index=False)
    # Trofeos con 0 ganadores suelen ser retirados o inalcanzables: no cuentan como "raros".
    a[a["earners"] > 0].nsmallest(300, "earn_rate")[["game", "platform", "title", "rarity", "earners",
                                                      "game_players", "earn_rate"]] \
        .rename(columns={"title": "trophy"}).to_csv(OUT / "rarest_trophies.csv", index=False)

    # ── Calidad y tasas de cambio ─────────────────────────────────────────────
    pd.DataFrame(QUALITY).to_csv(OUT / "etl_quality.csv", index=False)
    pd.concat(MISSING, ignore_index=True).to_csv(OUT / "missing_raw.csv", index=False)
    pd.DataFrame({"currency": [c.upper() for c in FX_TO_USD], "usd_per_unit": list(FX_TO_USD.values())}) \
        .to_csv(OUT / "fx_rates.csv", index=False)

    build_summary(OUT)

    print(f"\nListo en {time.time() - t0:.0f}s. Archivos:")
    for f in sorted(OUT.glob("*.csv")):
        print(f"  {f.name:<24} {f.stat().st_size / 1e6:6.2f} MB")


if __name__ == "__main__":
    main()
