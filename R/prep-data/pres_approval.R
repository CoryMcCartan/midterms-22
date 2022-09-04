library(tidyverse)
library(lubridate)
library(here)

d <- read_csv(here("data-raw/clean_potus_approval_1941_2022.csv"), show_col_types=FALSE) |>
    drop_na()

# get October data or latest available
d_elec <- d |>
    mutate(year = year(date),
           month = month(date),
           party = str_sub(str_to_lower(party), 1, 3)) |>
    filter(year %% 2 == 0, month <= 10) |>
    group_by(year) |>
    filter(month == max(month)) |>
    ungroup() |>
    group_by(year) |>
    summarize(lg_approval = qlogis(mean(approval)),
              .groups="drop")

write_csv(d_elec, here("data-raw/oct_pres_approval.csv"))
