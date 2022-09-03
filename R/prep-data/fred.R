library(fredr)
library(tidyverse)
library(lubridate)

# FRED_API_KEY must be set

build_q_series <- function(vars, units=rep_along(vars, "lin")) {
    map2_dfr(vars, units, function(x, u) {
        fredr(x, frequency="q", units=u) |>
            select(date, series_id, value)
    }) |>
        pivot_wider(names_from=series_id) |>
        drop_na()
}

d = build_q_series(c("GDPC1", "CPIAUCSL", "UNRATE"),
                   c("pc1", "pc1", "log")) |>
    rename(gdp_chg=GDPC1, cpi_chg=CPIAUCSL, lunemp=UNRATE)

d |>
    mutate(year = as.integer(year(date)),
           month = as.integer(month(date)),
           .before=date) |>
    filter(year %% 2 == 0) |>
    group_by(year) |>
    filter(month == max(month)) |>
    ungroup() |>
    select(-date, -month) |>
    write_csv(here::here("data-raw/fred_ind.csv"))
