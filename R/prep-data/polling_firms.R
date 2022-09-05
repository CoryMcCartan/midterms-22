library(tidyverse)
library(lubridate)
library(here)
source(here("R/utils.R"))

# Load and combine data --------

raw_hist_all = read_csv("https://github.com/fivethirtyeight/data/raw/master/pollster-ratings/raw-polls.csv",
                        show_col_types=FALSE)
raw_generic = read_csv("https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls_historical.csv",
                        show_col_types=FALSE)
raw_pres20 = read_csv("https://projects.fivethirtyeight.com/polls/data/president_polls_historical.csv",
                      show_col_types=FALSE)
raw_pres16 = read_csv(here("data-raw/538/president_general_polls_2016.csv"),
                      show_col_types=FALSE) |>
    suppressWarnings()
raw_dfp = read_csv(here("data-raw/dfp/clean_us_house_electoral_generic_ballot_polling.csv"),
                   show_col_types=FALSE) |>
    suppressWarnings()


d_hist_all = raw_hist_all |>
    filter(location == "US") |>
    transmute(year = as.integer(str_sub(race, 1, 4)),
              race = c(H="house", P="pres")[str_sub(race, 6, 6)],
              firm = pollster, firm_id = pollster_rating_id,
              type = str_to_lower(methodology),
              n = samplesize,
              tte = as.integer(mdy(electiondate) - mdy(polldate)),
              est = qlogis(cand1_pct / (cand1_pct + cand2_pct)),
              act = qlogis(cand1_actual / (cand1_actual + cand2_actual)))

d_generic = raw_generic |>
    transmute(year = as.integer(cycle),
              race = "house",
              firm = pollster, firm_id = pollster_rating_id,
              type = str_to_lower(methodology),
              pop = factor(population, levels=c("lv", "v", "rv", "a")),
              n = coalesce(sample_size, 600),
              tte = as.integer(mdy(election_date) -
                                   date_midpt(mdy(start_date), mdy(end_date))),
              est = qlogis(dem / (dem + rep)))

d_pres20 = raw_pres20 |>
    filter(is.na(state), party %in% c("DEM", "REP")) |>
    transmute(poll_id = poll_id,
              year = as.integer(cycle),
              race = "pres",
              firm = pollster, firm_id = pollster_rating_id,
              type = str_to_lower(methodology),
              pop = factor(population, levels=c("lv", "v", "rv", "a")),
              n = coalesce(sample_size, 600),
              tte = as.integer(mdy(election_date) -
                                   date_midpt(mdy(start_date), mdy(end_date))),
              party = party,
              pct = pct) |>
    group_by(poll_id) |>
    arrange(pop, .by_group=TRUE) |>
    slice_head(n=2) |>
    ungroup() |>
    pivot_wider(names_from=party, values_from=pct) |>
    mutate(est = qlogis(DEM / (DEM + REP))) |>
    select(-poll_id, -pop, -DEM, -REP)

## 2016 pres: need to match firm names --------
d_pres16 = raw_pres16 |>
    filter(state == "U.S.") |>
    transmute(year = 2016L,
              race = "pres",
              firm = pollster,
              pop = factor(population, levels=c("lv", "v", "rv", "a")),
              n = coalesce(samplesize, 100),
              tte = as.integer(make_date(2016, 11, 8) -
                                   date_midpt(mdy(startdate), mdy(enddate))),
              est = qlogis(rawpoll_clinton / (rawpoll_clinton + rawpoll_trump))) |>
    distinct()

guess_types = bind_rows(d_hist_all, d_generic, d_pres20) |>
    count(firm, firm_id, type) |>
    group_by(firm, firm_id) |>
    arrange(desc(n)) |>
    slice_head(n=1) |>
    ungroup() |>
    select(-n)

# match 2016 firms to IDs and methodologies
if (interactive()) {
    match_16 <- match_manual(d_pres16$firm, guess_types$firm)
    d_pres16$firm = match_16[d_pres16$firm]
} else {
    m_dist = adist(d_pres16$firm, guess_types$firm)
    max_dist = 9
    m_dist[m_dist >= max_dist] = Inf
    idx_match = apply(m_dist, 1, function(x) {
        out = which.min(x)
        if (is.infinite(x[out])) out = NA_integer_
        out
    })
    d_pres16$firm = guess_types$firm[idx_match]
}

d_pres16 = left_join(d_pres16, select(guess_types, firm, firm_id, type), by="firm")


## DFP: need to match firm names ------------
# missing: actual result (easy to fix)
# firm names don't match 538 (harder to fix)
d_dfp = raw_dfp |>
    transmute(year = as.integer(cycle_year),
              race = "house",
              firm = pollster,
              type = str_to_lower(na_if(methodology, "U")),
              pop = factor(case_when(
                  poll_population == "U" ~ NA_character_,
                  poll_population == "A" ~ "a",
                  TRUE ~ "lv"
              ), levels=c("lv", "v", "rv", "a")),
              n = coalesce(sample_size, 600),
              tte = as.integer(election_date - poll_date),
              est = qlogis(dem_pct / (dem_pct + gop_pct)))

if (interactive()) {
    match_dfp <- match_manual(d_dfp$firm, guess_types$firm)
    match_dfp["Public Opinion Strategies (POS)"] = "POS"
    match_dfp["Rasmussen Reports"] = "Rasmussen (Pulse Opinion Research)"
    d_dfp$firm = match_dfp[d_dfp$firm]
} else {
    m_dist = adist(d_dfp$firm, guess_types$firm)
    max_dist = 9
    m_dist[m_dist >= max_dist] = Inf
    idx_match = apply(m_dist, 1, function(x) {
        out = which.min(x)
        if (is.infinite(x[out])) out = NA_integer_
        out
    })
    d_dfp$firm = guess_types$firm[idx_match]
}

d_dfp = left_join(d_dfp, select(guess_types, firm, firm_id, type), by="firm",
                  suffix=c("", "_guess")) |>
    mutate(type = coalesce(type, type_guess)) |>
    select(-type_guess)


# Join -----

filler_firm_ids = bind_rows(d_generic, d_pres20, d_pres16, d_dfp, d_hist_all) |>
    distinct(firm, firm_id) |>
    mutate(firm_id = coalesce(firm_id, 1:n()))


act_results = distinct(d_hist_all, year, race, act)

d = bind_rows(d_generic, d_pres20, d_pres16, d_dfp) |>
    inner_join(act_results, by=c("year", "race")) |>
    bind_rows(d_hist_all) |>
    filter(tte >= 0) |>
    select(-firm_id) |>
    left_join(filler_firm_ids, by=c("firm")) |>
    mutate(firm_id = as.integer(firm_id),
           n = as.integer(n),
           type = case_when(type == "live phone" ~ "phone",
                            str_detect(type, "ivr") ~ "ivr",
                            type == "live phone/online" ~ "mixed",
                            str_detect(type, "online") ~ "online"),
           is_lv = as.integer(coalesce(pop == "lv", FALSE)),
           err = est - act) |>
    drop_na(firm) |>
    group_by(firm_id) |>
    filter(n() >= 3) |>
    ungroup() |>
    slice_sample(prop=1, replace=FALSE) |> # shuffle
    mutate(firm_id = factor(firm_id),
           year = factor(year),
           type = factor(type),
           poll_id = 1:n())

# de-dupe
idx_dedupe = distinct(d, year, race, firm_id, n, round(tte/10), est,
                      .keep_all=TRUE) |>
    pull(poll_id)

d = filter(d, poll_id %in% idx_dedupe) |>
    select(-poll_id) |>
    relocate(firm_id, .after=firm) |>
    relocate(is_lv, .after=pop)

d |>
    arrange(year, race, tte, firm, pop) |>
write_csv(here("data-raw/produced/hist_polls_house_pres.csv"))

if (FALSE) {
    # how are polling errors distributed by methodology?
    d |>
        filter(!str_detect(firm, "Harris")) |>
    ggplot(aes(sample=err, group=type)) +
        geom_qq(size=0.3) +
        stat_qq_line(color='red')

    # how are polling errors distributed by LV/not?
    d |>
        filter(!str_detect(firm, "Harris")) |>
    ggplot(aes(sample=err, group=is_lv, color=is_lv)) +
        geom_qq(size=0.3) +
        stat_qq_line() +
        wacolors::scale_color_wa_c("sea_star")

    # how are polling errors distributed by year?
    d |>
        filter(!str_detect(firm, "Harris")) |>
    ggplot(aes(sample=err, group=year, color=year)) +
        geom_qq(size=0.3) +
        stat_qq_line() +
        wacolors::scale_color_wa_d("sound_sunset")

    # Look at 2 big online pollsters specifically
    filter(d, type=="online") |>
        filter(firm == "Ipsos" | firm == "HarrisX") |>
        mutate(x = qnorm(cume_dist(err))) |>
    ggplot(aes(x, err, shape=fct_lump(firm, 5), color=str_c(race, year))) +
        geom_point(size=1, position=position_jitter(0, 0.05))

    # how are polling errors distributed by methodology?
    # final 3 months, de-meaned
    d |>
        group_by(firm, race) |>
        mutate(err = err - mean(err)) |>
        filter(tte <= 120) |>
    ggplot(aes(sample=err, group=type)) +
        geom_qq(size=0.3) +
        stat_qq_line(color='red')

    # error by sample size
    ggplot(d, aes(log10(n), err^2)) +
        geom_point(size=0.2) +
        scale_y_log10() +
        geom_smooth()

}
