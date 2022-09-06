suppressMessages({
    library(dplyr)
    library(stringr)
    library(readr)
    library(lubridate)
    library(here)
    source(here("R/utils.R"))
})

load_polls <- function(year=2022, limit_per_firm=100) {
    download_polls(year) |>
        transmute(year = as.integer(cycle),
                  race = "house",
                  firm = pollster,
                  firm_id = as.integer(pollster_rating_id),
                  type = coalesce(str_to_lower(methodology), "unknown"),
                  type = case_when(type == "live phone" ~ "phone",
                                   str_detect(type, "ivr") ~ "ivr",
                                   type == "live phone/online" ~ "mixed",
                                   str_detect(type, "online") ~ "online",
                                   TRUE ~ type),
                  not_lv = as.integer(coalesce(population != "lv", TRUE)),
                  n = as.integer(coalesce(sample_size, 600)),
                  tte = as.integer(mdy(election_date) -
                                       date_midpt(mdy(start_date), mdy(end_date))),
                  est = log(dem) - log(rep)) |>
        group_by(firm_id) |>
        slice_sample(prop=1, replace=FALSE) |>
        slice_head(n=limit_per_firm) |>
        ungroup()
}

download_polls <- function(year=2022) {
    current_year <- ceiling(year(Sys.Date()) / 2) * 2
    if (year == current_year) {
        url <- "https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls.csv"
    } else {
        url <- "https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls_historical.csv"
    }

    read_csv(url, show_col_types=FALSE) |>
        filter(cycle == year)
}
