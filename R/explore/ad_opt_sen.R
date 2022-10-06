suppressMessages({
    library(tidyverse)
    library(collapse)
    library(wacolors)
    library(scales)
    library(rvest)
    library(jsonlite)
    library(here)
})

d_races = read_csv(here("docs/senate_races.csv"), show_col_types=FALSE) |>
    filter(unopp == 0)

d_markets = read_rds(here("data-raw/produced/state_markets.rds"))
d_rates = read_csv(here("data-raw/produced/ad_rates.csv"), show_col_types=FALSE)
d_markets = d_markets |>
    select(-stations) |>
    unnest(markets) |>
    left_join(select(d_rates, market, rate_imp), by=c("markets"="market")) |>
    group_by(state, households, area) |>
    summarize(cost = sum(rate_imp), .groups="drop")

d = d_races |>
    select(-starts_with("dem_q")) |>
    left_join(d_markets, by="state") |>
    mutate(wt = pr_dem * (1 - pr_dem),
           impact = wt / cost,
           impact = impact / mean(impact)) |>
    arrange(desc(impact))

if (interactive()) {
    ggplot(d, aes(pr_dem, impact, color=cost, size=wt, label=state)) +
        geom_text(fontface="bold") +
        scale_size_area(max_size=8, guide="none") +
        scale_color_wa_c("puget", name="Ad cost", labels=dollar) +
        scale_x_continuous("Win probability", labels=percent) +
        labs(y="Donation impact") +
        theme_bw() +
        theme(legend.position=c(0.125, 0.875),
              legend.background=element_blank(),
              panel.grid.minor.y=element_blank())
}

opt_sen = head(d, 5) |>
    select(state, dem_cand, inc_seat, pr_dem, cost, wt, impact)

parse_name = function(x) {
    x = str_to_upper(str_replace_all(x, " ([A-Z]\\.)+ ", " "))
    str_c(str_sub(x, 1, 1), ". ", word(x, 2, -1))
}

page = read_html("https://secure.actblue.com/donate/senate-impact-22")
script_texts = html_text(html_elements(page, "script"))
obj = script_texts[str_detect(script_texts, "window\\.preloadedState")] |>
    str_squish() |>
    str_remove("window\\.preloadedState\\s*=\\s*") |>
    str_replace_all("undefined", "null") |>
    parse_json(simplifyVector=TRUE)

curr_sen = tibble(distr=obj$entities$display_pretty_location,
                    cand=parse_name(obj$entities$display_name)) |>
    separate(distr, c("state", NA), sep="-")

cat("Remove:\n")
anti_join(curr_sen, opt_sen, by=c("state", "cand"="dem_cand")) |>
    print()

cat("\nAdd:\n")
anti_join(opt_sen, curr_sen, by=c("state", "dem_cand"="cand")) |>
    select(state, cand=dem_cand) |>
    print()
