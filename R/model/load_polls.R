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

get_senate_poll_avg <- function(from_date=Sys.Date(), force=FALSE) {
    path <- here("data-raw/raw_sen_polls_cache.csv")

    if (file.exists(path) && file.mtime(path) >= from_date && isFALSE(force)) {
        raw <- read_csv(path, show_col_types=FALSE)
    } else {
        url <- "https://projects.fivethirtyeight.com/polls/data/senate_polls.csv"
        raw <- read_csv(url, show_col_types=FALSE)
        write_csv(raw, path)
    }

    parse_name = function(x) {
        x = str_to_upper(str_replace_all(x, " ([A-Z]\\.)+ ", " "))
        str_c(str_sub(x, 1, 1), ". ", word(x, 2, -1)) |>
            str_replace("JUNIOR WALKER", "WALKER") |>
            str_replace("BUSCH VALENTINE", "VALENTINE") |>
            str_replace("PAUL LAXALT", "LAXALT") |>
            str_replace("RAE PERKINS", "PERKINS")
    }

    d_pres = read_csv(here("data-raw/produced/hist_pres_state.csv"), show_col_types=FALSE) |>
        filter(year == 2022)

    raw |>
        mutate(start_date = mdy(start_date),
               end_date = mdy(end_date),
               date = date_midpt(start_date, end_date)) |>
        filter(date >= from_date - 30) |>
        transmute(id = poll_id,
                  state = state.abb[match(state, state.name)],
                  party = str_to_lower(party),
                  cand = parse_name(candidate_name),
                  pct = pct/100) |>
        filter(party %in% c("dem", "rep")) |>
        group_by(id, party) |>
        arrange(desc(pct)) |>
        slice_head(n=1) |>
        pivot_wider(names_from=party, values_from=c(cand, pct)) |>
        left_join(d_pres, by="state") |>
        mutate(est = log(pct_dem) - log(pct_rep),
               cand_dem = if_else(state != "AK", cand_dem, NA_character_), # manual
               ldem_pres_adj = ldem_pres - ldem_pres_natl) |>
        group_by(state, cand_dem, cand_rep) |>
        summarize(poll_avg = conj_mean(est, ldem_pres_adj[1], 2.0))
}
