suppressMessages({
    library(dplyr)
    library(stringr)
    library(readr)
    library(lubridate)
    library(here)
    source(here("R/utils.R"))
})

load_polls <- function(year=2022, limit_per_firm=100, from_date=Sys.Date(), force=FALSE) {
    if (year >= 2018) {
        out <- download_polls(year, from_date, force=force) |>
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
                      est = log(dem) - log(rep))
    } else {
        out <- read_csv(here("data-raw/produced/hist_polls_house_pres.csv"),
                 show_col_types=FALSE) |>
            filter(.data$year == .env$year, race == "house") |>
            mutate(type = case_when(year <= 2008 ~ "phone",
                                    year <= 2004 & is.na(type) ~ "phone",
                                    TRUE ~ type),
                   type = coalesce(type, "unknown"),
                   firm_id = as.integer(firm_id),
                   not_lv = as.integer(coalesce(pop != "lv", TRUE))) |>
            select(-act, -err)
    }

    out |>
        group_by(firm_id) |>
        slice_sample(prop=1, replace=FALSE) |>
        slice_head(n=limit_per_firm) |>
        ungroup()
}

download_polls <- function(year=2022, from_date=Sys.Date(), force=FALSE) {
    path <- here("data-raw/raw_gcb_polls_cache.csv")

    if (file.exists(path) && file.mtime(path) >= from_date && isFALSE(force)) {
        read_csv(path, show_col_types=FALSE)
    } else {
        current_year <- ceiling(year(Sys.Date()) / 2) * 2
        if (year == current_year) {
            url <- "https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls.csv"
        } else {
            url <- "https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls_historical.csv"
        }

        out = read_csv(url, show_col_types=FALSE) |>
            filter(cycle == year)
        write_csv(out, path)
        out
    }
}

get_latest_approval <- function(from_date=Sys.Date(), limit_per_firm=10, force=FALSE) {
    path <- here("data-raw/raw_appr_polls_cache.csv")

    if (file.exists(path) && file.mtime(path) >= from_date && isFALSE(force)) {
        raw <- read_csv(path, show_col_types=FALSE)
    } else {
        url <- "https://projects.fivethirtyeight.com/biden-approval-data/approval_polllist.csv"
        raw <- read_csv(url, show_col_types=FALSE) |>
            filter(subgroup == "All polls")
        write_csv(raw, path)
    }

    raw |>
        mutate(startdate = mdy(startdate),
               enddate = mdy(enddate),
               approve = approve / 100,
               date = startdate + 0.5*(enddate-startdate)) |>
        filter(date >= from_date - 30) |>
        group_by(date, pollster) |>
        slice_sample(n=1) |>
        group_by(pollster) |>
        slice_head(n=limit_per_firm) |>
        pull(approve) |>
        mean()
}
