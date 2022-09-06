library(tidyverse)
library(lubridate)
library(here)
library(brms)

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
    left_join(d_appr, by="year")

# save to public data
d |>
    select(year, linc_vote_contest, linc_vote, lg_retire, midterm,
           inc_house, dem_pres, gdp_chg, lunemp, lg_approval) |>
    filter(year == 2022 | !is.na(linc_vote)) |>
    mutate(across(where(is.numeric), round, 5)) |>
    write_csv(here("data/fundamentals.csv"))

form <- linc_vote ~ lg_retire + midterm*inc_house + dem_pres + gdp_chg + lunemp + lg_approval
bprior <- prior(R2D2(0.6), class=b)

m <- brm(form, data=drop_na(d, linc_vote_contest), prior=bprior,
         backend="cmdstan", cores=4, iter=1200, warmup=1000,
         file=here("stan/fund_m"), file_refit="on_change",
         control=list(adapt_delta=0.9995, refresh=1000))

pred_fund_m <- function(pred_year = 2022) {
    d_fit <- drop_na(d, linc_vote_contest) |>
        filter(year < pred_year)

    m_fit <- update(m, newdata=d_fit, cores=4, iter=1200, warmup=1000,
                    control=list(adapt_delta=0.9995, refresh=0))

    d_pred = filter(d, year == pred_year)
    pred <- posterior_predict(m_fit, newdata=d_pred)[, 1]
}

walk(seq(2010, 2022, by=2), function(yr) {
    pred_fund_m(yr) |>
        write_rds(here(str_glue("data/fund_pred/fundamentals_pred_{year}.rds")), compress="gz")
})

write_rds(m, here("data-raw/produced/fundamentals_model_2022.rds"), compress="xz")

