library(tidyverse)
library(here)

d_state <- read_csv(here("data-raw/produced/hist_house_races.csv.gz"), show_col_types=FALSE) |>
    select(year, state, ldem_gen, polar, pop:pov, region, division) |>
    distinct()

parse_name = function(x) {
    matched = str_match(x, "^([A-Z'\\- ]+), ([A-Z])")
    str_c(matched[, 3], ". ", matched[, 2])
}

d_cand_raw <- read_tsv(here("data-raw/dv/candidates_2006-2020.tab"), show_col_types=FALSE)
d_cand = d_cand_raw |>
    rename(name = name_snyder) |>
    filter(office == "S", type == "G") |>
    mutate(dist = as.integer(dist),
           party = case_when(str_detect(name, "SANDERS, BERNARD") ~ "dem",
                             str_detect(name, "KING, ANGUS") ~ "dem",
                             str_detect(name, "MURKOWSKI, LISA") ~ "rep",
                             party == "D" ~ "dem",
                             party == "R" ~ "rep",
                             TRUE ~ "oth")) |>
    select(year, state, class=dist, party, cand=name, inc,
           votes=candidatevotes, total=totalvotes) |>
    group_by(year, state, class) |>
    filter(votes / total > 0.15) |>
    group_by(year, state, class, party) |>
    arrange(desc(votes)) |>
    mutate(votes = sum(votes)) |>
    group_by(year, state, class) |>
    mutate(unopp = 1L*(n() < 2 | max(table(party)) == n()),
           multi = 1L*(n() > 2),
           inc = coalesce(ifelse(any(inc == 1), party[inc == 1], "open"), "open"),
           cand = parse_name(cand)) |>
    ungroup() |>
    select(-total) |>
    pivot_wider(names_from=party, values_from=c(cand, votes),
                values_fn = ~ .[1]) |>
    arrange(year, state) %>%
    mutate(ldem_seat = log(votes_dem) - log(votes_rep)) |>
    select(-cand_oth)
# TODO check if S. BROWN is the same in MA 2012 and NH 2014

d_pres <- read_csv(here("data-raw/medsl/1976-2020-president.csv"), show_col_types=FALSE) |>
    transmute(year = year + 2,
              state = state_po,
              party = str_to_lower(str_sub(party_simplified, 1, 3)),
              votes = candidatevotes) |>
    filter(party %in% c("dem", "rep")) |>
    group_by(year, state, party) |>
    arrange(desc(votes)) |>
    slice_head(n=1) |>
    ungroup() |>
    pivot_wider(names_from=party, values_from=votes) |>
    group_by(year) |>
    mutate(ldem_pres_natl = log(sum(dem)) - log(sum(rep))) |>
    group_by(year, state) |>
    summarize(ldem_pres = log(sum(dem)) - log(sum(rep)),
              ldem_pres_natl = ldem_pres_natl[1],
              .groups="drop") |>
    filter(year >= 2006) |>
    complete(year = unique(d_cand$year), state) |>
    group_by(state) |>
    arrange(year) |>
    fill(ldem_pres, ldem_pres_natl) |>
    ungroup()


d = d_cand |>
    left_join(d_pres, by=c("year", "state")) |>
    left_join(d_state, by=c("year", "state"))


write_csv(d, here("data-raw/produced/hist_sen_races.csv.gz"))
