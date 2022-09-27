library(tidyverse)
library(collapse)
library(wacolors)
library(scales)
library(here)

d_distr = read_csv(here("docs/districts.csv"), show_col_types=FALSE) |>
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

head(d, 20) |>
    select(state, district, dem_cand, inc_seat, pr_dem, cost, wt, impact)
