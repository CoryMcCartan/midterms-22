library(tidyverse)
library(here)

d_state <- read_csv(here("data-raw/produced/hist_house_races.csv.gz"), show_col_types=FALSE) |>
    select(year, state, ldem_gen, polar, inc_pres, pop:pov, region, division) |>
    distinct()

parse_name = function(x) {
    matched = str_match(x, "^([A-Z'\\- ]+), ([A-Z])")
    str_c(matched[, 3], ". ", matched[, 2])
}

d_cand_raw <- read_tsv(here("data-raw/dv/candidates_2006-2020.tab"), show_col_types=FALSE)
d_cand = d_cand_raw |>
    rename(name = name_snyder) |>
    filter(office == "S", type == "G" | (type == "S" & year %% 2 == 0)) |>
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

parse_name_22 = function(x) {
    str_c(str_sub(x, 1, 1), ". ", word(x, 2, -1))
}

d_22 <- read_csv(here("data-raw/dfp/senate_candidates_2022.csv"), show_col_types=FALSE) |>
    filter(year == 2022, candidate_number <= 2) |>
    transmute(year = year,
              class = 3,
              state = state_code,
              inc = 1*(incumbency_status == "incumbent"),
              party = str_to_lower(str_sub(party_affiliation, 1, 3)),
              cand = parse_name_22(candidate_name),
              votes = NA_real_) |>
    group_by(year, state) |>
    mutate(unopp = 1L*(n() < 2 | max(table(party)) == n()),
           multi = 1L*(n() > 2),
           inc = coalesce(ifelse(any(inc == 1), party[inc == 1], "open"), "open")) |>
    bind_rows(tibble(year=2022, class=2, state="OK", inc="open",
                     party=c("dem", "rep"), cand=c("K. HORN", "M. MULLIN"),
                     unopp=0, multi=0)) |>
    bind_rows(tibble(year=2022, class=3, state="LA", inc="rep",
                     party=c("rep", "dem"), cand=c("J. KENNEDY", "NA"),
                     unopp=0, multi=0)) |>
    ungroup() |>
    pivot_wider(names_from=party, values_from=c(cand, votes),
                values_fn = ~ .[1]) |>
    arrange(state) %>%
    mutate(ldem_seat = log(votes_dem) - log(votes_rep)) |>
    select(-any_of("cand_oth"))

d_cand <- bind_rows(d_cand, d_22)


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
