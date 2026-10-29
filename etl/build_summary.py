"""
build_summary.py — Cifras clave del análisis (data/processed/summary.csv).

Lee las tablas procesadas y calcula los números que aparecen en los insights y
las conclusiones. Dash y Shiny leen este mismo archivo, así que los textos de
los dos dashboards muestran exactamente las mismas cifras.

Uso independiente:  python etl/build_summary.py
"""
from __future__ import annotations

from pathlib import Path

import pandas as pd

MIN_GENRE_N = 50  # géneros con al menos 50 juegos con precio


def build_summary(out: Path) -> pd.DataFrame:
    g = pd.read_csv(out / "games_clean.csv")
    countries = pd.read_csv(out / "countries.csv")
    lib = pd.read_csv(out / "player_library.csv")
    act = pd.read_csv(out / "activity_monthly.csv")
    heat = pd.read_csv(out / "activity_heatmap.csv")
    rar = pd.read_csv(out / "rarity_summary.csv").set_index("rarity")
    rare = pd.read_csv(out / "rarest_trophies.csv")
    q = pd.read_csv(out / "etl_quality.csv")

    rows: list[tuple[str, object, str]] = []

    def add(key, value, display=None):
        rows.append((key, value, display if display is not None else str(value)))

    def n(x):
        return f"{int(x):,}"

    def usd(x):
        return f"${x:,.2f}"

    def pct(x, d=1):
        return f"{x:.{d}f}%"

    # Catálogo
    add("n_games", len(g), n(len(g)))
    add("n_titles", g["title"].nunique(), n(g["title"].nunique()))
    add("n_publishers", g["publisher"].nunique(), n(g["publisher"].nunique()))
    plat = g["platform"].value_counts(normalize=True) * 100
    add("top_platform", plat.index[0])
    add("top_platform_pct", round(plat.iloc[0], 1), pct(plat.iloc[0]))
    add("ps5_pct", round(plat.get("PS5", 0), 1), pct(plat.get("PS5", 0)))
    by_year = g["year"].value_counts().sort_index()
    add("peak_year", int(by_year.idxmax()))
    add("peak_year_n", int(by_year.max()), n(by_year.max()))
    y0, y1 = by_year.get(2015, 0), by_year.get(2024, 0)
    add("launches_2015", int(y0), n(y0))
    add("launches_2024", int(y1), n(y1))
    add("growth_2015_2024", round((y1 / y0 - 1) * 100, 1), pct((y1 / y0 - 1) * 100, 0))
    gen = g.loc[g["genre_main"] != "Sin género", "genre_main"].value_counts()
    add("top_genre", gen.index[0])
    add("top_genre_pct", round(gen.iloc[0] / len(g) * 100, 1), pct(gen.iloc[0] / len(g) * 100))
    top_pub = g["publisher"].value_counts()
    add("top_publisher", top_pub.index[0])
    add("top_publisher_n", int(top_pub.iloc[0]), n(top_pub.iloc[0]))

    # Precios
    priced = g.dropna(subset=["price_usd"])
    add("n_priced", len(priced), n(len(priced)))
    add("n_no_price", len(g) - len(priced), n(len(g) - len(priced)))
    add("median_price", round(priced["price_usd"].median(), 2), usd(priced["price_usd"].median()))
    add("pct_under_10", round((priced["price_usd"] < 10).mean() * 100, 1), pct((priced["price_usd"] < 10).mean() * 100))
    add("pct_over_40", round((priced["price_usd"] >= 40).mean() * 100, 1), pct((priced["price_usd"] >= 40).mean() * 100))
    gm = priced.groupby("genre_main")["price_usd"].agg(["median", "size"])
    gm = gm[(gm["size"] >= MIN_GENRE_N) & (gm.index != "Sin género")].sort_values("median")
    add("cheapest_genre", gm.index[0])
    add("cheapest_genre_median", gm["median"].iloc[0], usd(gm["median"].iloc[0]))
    add("priciest_genre", gm.index[-1])
    add("priciest_genre_median", gm["median"].iloc[-1], usd(gm["median"].iloc[-1]))
    pm = priced.groupby("platform")["price_usd"].median()
    for p in ["PS4", "PS5"]:
        add(f"median_{p.lower()}", round(pm.get(p, float("nan")), 2), usd(pm.get(p, float("nan"))))
    ratios = {}
    for c in ["eur", "gbp", "jpy", "rub"]:
        both = priced.dropna(subset=[f"price_{c}_in_usd"])
        ratios[c.upper()] = (both[f"price_{c}_in_usd"] / both["price_usd"]).median()
    exp = max(ratios, key=ratios.get)
    chp = min(ratios, key=ratios.get)
    add("priciest_currency", exp)
    add("priciest_currency_diff", round((ratios[exp] - 1) * 100, 1), pct((ratios[exp] - 1) * 100))
    add("cheapest_currency", chp)
    add("cheapest_currency_diff", round((ratios[chp] - 1) * 100, 1), pct((ratios[chp] - 1) * 100))
    changed = int((priced["price_change_pct"].fillna(0) != 0).sum())
    multi = int((priced["n_snapshots"] >= 2).sum())
    add("n_price_changed", changed, n(changed))
    add("n_multi_snapshot", multi, n(multi))
    add("pct_price_changed", round(changed / multi * 100, 2), pct(changed / multi * 100, 2))

    # Jugadores
    add("n_players", int(countries["n_players"].sum()), n(countries["n_players"].sum()))
    add("n_countries", len(countries), n(len(countries)))
    top = countries.iloc[0]
    add("top_country", top["country"])
    add("top_country_pct", top["pct_players"], pct(top["pct_players"]))
    add("top5_countries_pct", round(countries["pct_players"].head(5).sum(), 1), pct(countries["pct_players"].head(5).sum()))
    add("n_players_library", len(lib), n(len(lib)))
    add("median_library", int(lib["library_size"].median()), n(lib["library_size"].median()))
    events = int(q.loc[(q["tabla"] == "history") & (q["paso"] == "Limpieza de actividad"), "filas_despues"].iloc[0])
    add("n_trophy_events", events, n(events))
    act_full = act.iloc[:-1]  # el último mes del dataset está incompleto
    peak = act_full.loc[act_full["trophies"].idxmax()]
    add("peak_month", peak["month"])
    add("peak_month_trophies", int(peak["trophies"]), n(peak["trophies"]))
    hh = heat.groupby("hour")["trophies"].sum()
    dd = heat.groupby("weekday")["trophies"].sum()
    add("peak_hour", int(hh.idxmax()), f"{int(hh.idxmax()):02d}:00")
    add("low_hour", int(hh.idxmin()), f"{int(hh.idxmin()):02d}:00")
    add("peak_weekday", dd.idxmax())
    weekend = dd[["Sábado", "Domingo"]].sum() / dd.sum() * 100
    add("weekend_pct", round(weekend, 1), pct(weekend))
    add("last_month", act["month"].iloc[-1])
    yearly = act_full.assign(y=act_full["month"].str[:4].astype(int)).groupby("y")[["trophies", "active_players"]].mean()
    yp = int(yearly["active_players"].idxmax())
    ylast = int(yearly.index.max())
    add("active_peak_year", yp)
    add("active_peak_avg", int(yearly.loc[yp, "active_players"]), n(yearly.loc[yp, "active_players"]))
    add("active_last_year", ylast)
    add("active_last_avg", int(yearly.loc[ylast, "active_players"]), n(yearly.loc[ylast, "active_players"]))
    add("active_first_avg", int(yearly["active_players"].iloc[0]), n(yearly["active_players"].iloc[0]))
    add("active_first_year", int(yearly.index.min()))
    tpt_first = yearly.loc[yp, "trophies"] / yearly.loc[yp, "active_players"]
    tpt_last = yearly.loc[ylast, "trophies"] / yearly.loc[ylast, "active_players"]
    add("trophies_per_active_peak", round(tpt_first, 1), f"{tpt_first:.0f}")
    add("trophies_per_active_last", round(tpt_last, 1), f"{tpt_last:.0f}")
    add("top5_countries", ", ".join(countries["country"].head(5)))
    add("p90_library", int(lib["library_size"].quantile(.9)), n(lib["library_size"].quantile(.9)))
    add("max_library", int(lib["library_size"].max()), n(lib["library_size"].max()))

    # Popularidad
    best = g.loc[g["n_owners"].idxmax()]
    add("top_owned_game", best["title"])
    add("top_owned_pct", round(best["n_owners"] / len(lib) * 100, 1), pct(best["n_owners"] / len(lib) * 100))
    pop = priced[priced["n_owners"] > 0]
    rho = pop["price_usd"].rank().corr(pop["n_owners"].rank())  # Spearman = Pearson sobre rangos
    add("spearman_price_owners", round(rho, 2), f"{rho:.2f}")
    om = g[g["genre_main"] != "Sin género"].groupby("genre_main")["n_owners"].agg(["median", "size"])
    om = om[om["size"] >= MIN_GENRE_N].sort_values("median")
    add("most_owned_genre", om.index[-1])
    add("most_owned_genre_median", int(om["median"].iloc[-1]), n(om["median"].iloc[-1]))

    # Logros
    with_t = g[g["n_trophies"] > 0]
    add("median_trophies", int(with_t["n_trophies"].median()), n(with_t["n_trophies"].median()))
    add("pct_platinum", round(g["has_platinum"].mean() * 100, 1), pct(g["has_platinum"].mean() * 100))
    for r in ["Bronze", "Silver", "Gold", "Platinum"]:
        v = rar.loc[r, "median_earn_rate"]
        add(f"earn_{r.lower()}", v, pct(v))
    r0 = rare.iloc[0]
    add("rarest_trophy", f"{r0['trophy']} ({r0['game']})")
    add("rarest_trophy_rate", r0["earn_rate"], pct(r0["earn_rate"], 2))

    df = pd.DataFrame(rows, columns=["key", "value", "display"])
    df.to_csv(out / "summary.csv", index=False)
    return df


if __name__ == "__main__":
    import sys
    sys.stdout.reconfigure(encoding="utf-8")
    out = Path(__file__).resolve().parents[1] / "data" / "processed"
    print(build_summary(out).to_string(index=False))
