library(tidyverse)
library(lubridate)
library(here)

d_econ <- read_csv(here("data-raw/produced/fred_ind.csv"), show_col_types=FALSE)
d_appr <- read_csv(here("data-raw/produced/oct_pres_approval.csv"), show_col_types=FALSE)
d_hist <- read_csv(here("data-raw/dfp/001_house_national_voteshare_input.csv"), show_col_types=FALSE) |>
    rename_with(~ str_replace(., "in_power", "inc"), starts_with("in_power")) |>
    rename(year=cycle) |>
    mutate(linc_vote_contest = qlogis(inc_contested_two_way),
           linc_vote = qlogis(inc_two_way),
           lg_retire = qlogis(inc_retire_pct))
d_control <- read_csv(here("data-raw/manual/party_control.csv"), show_col_types=FALSE) |>
    mutate(inc_house = if_else(dem_pres == 1, dem_house, 1 - dem_house),
           inc_senate = if_else(dem_pres == 1, dem_senate, 1 - dem_senate)) |>
    select(year=elect_year, inc_house:inc_senate) |>
    bind_rows(tibble(year=2022, inc_house=1, inc_senate=1))

d <- d_hist |>
    select(year, linc_vote_contest, linc_vote, lg_retire, inc_special_performance, dem_pres, midterm) |>
    left_join(d_control, by="year") |>
    left_join(d_econ, by="year") |>
    left_join(d_appr, by="year") |>
    mutate(midterm = 1 - midterm)

d |>
    select(year, linc_vote_contest, linc_vote, lg_retire, midterm,
           inc_house, dem_pres, gdp_chg, lunemp, cpi_chg, lg_approval) |>
    filter(year == 2022 | !is.na(linc_vote)) |>
    mutate(across(where(is.numeric), round, 5)) |>
    write_csv(here("data-raw/produced/fundamentals_basic.csv"))
