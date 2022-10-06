suppressMessages({
    library(tidyverse)
    library(collapse)
    library(wacolors)
    library(scales)
    library(rvest)
    library(jsonlite)
    library(here)
})

d_distr = read_csv(here("docs/house_districts.csv"), show_col_types=FALSE) |>
    filter(unopp == 0)

d_markets = read_rds(here("data-raw/produced/distr_markets.rds"))
d_rates = read_csv(here("data-raw/produced/ad_rates.csv"), show_col_types=FALSE)
d_markets = d_markets |>
    select(-stations) |>
    unnest(markets) |>
    left_join(select(d_rates, market, rate_imp), by=c("markets"="market")) |>
    group_by(state, district, households, area) |>
    summarize(cost = sum(rate_imp), .groups="drop") |>
    mutate(district = as.numeric(district))

d = d_distr |>
    select(-starts_with("dem_q")) |>
    left_join(d_markets, by=c("state", "district")) |>
    mutate(wt = pr_dem * (1 - pr_dem),
           impact = wt / cost,
           impact = impact / mean(impact)) |>
    arrange(desc(impact))

if (interactive()) {
    ggplot(d, aes(pr_dem, impact, color=cost, size=wt, label=str_c(state, "-", district))) +
        geom_text(fontface="bold") +
        scale_size_area(max_size=5, guide="none") +
        scale_color_wa_c("puget", name="Ad cost", labels=dollar) +
        scale_x_continuous("Win probability", labels=percent) +
        labs(y="Donation impact") +
        theme_bw() +
        theme(legend.position=c(0.125, 0.875),
              legend.background=element_blank(),
              panel.grid.minor.y=element_blank())
}

opt_house = head(d, 20) |>
    select(state, district, dem_cand, inc_seat, pr_dem, cost, wt, impact)


parse_name = function(x) {
    x = str_to_upper(str_replace_all(x, " ([A-Z]\\.)+ ", " "))
    str_c(str_sub(x, 1, 1), ". ", word(x, 2, -1))
}

page = read_html("https://secure.actblue.com/donate/house-impact-22")
script_texts = html_text(html_elements(page, "script"))
obj = script_texts[str_detect(script_texts, "window\\.preloadedState")] |>
    str_squish() |>
    str_remove("window\\.preloadedState\\s*=\\s*") |>
    str_replace_all("undefined", "null") |>
    parse_json(simplifyVector=TRUE)

curr_house = tibble(distr=obj$entities$display_pretty_location,
                    cand=obj$entities$display_name) |>
    separate(distr, c("state", "district"), sep="-") |>
    mutate(district = as.numeric(district))

cat("Remove:\n")
anti_join(curr_house, opt_house, by=c("state", "district")) |>
    print()

cat("\nAdd:\n")
anti_join(opt_house, curr_house, by=c("state", "district")) |>
    select(state, district, cand=dem_cand) |>
    print()
