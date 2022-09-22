library(tidyverse)
library(sf)
library(janitor)
library(googlesheets4)
library(here)

# Historical candidates and results -------

raw <- read_csv(here("data-raw/dfp/clean_us_house_district_results.csv.gz"), show_col_types=FALSE)

d_hist <- raw |>
    transmute(year = as.integer(year),
              state = state_code,
              district = as.integer(district_number),
              inc_pres = str_to_lower(wh_party),
              inc_seat = factor(coalesce(str_to_lower(house_incumbent_party), "open"),
                                levels=c("dem", "open", "gop")),
              unopp = house_unopposed_binary,
              dem_exp = house_dem_expenditure,
              rep_exp = house_gop_expenditure,
              ldem_seat = coalesce(qlogis(house_dem_pct_two_way), Inf), # one unopposed Dem shows up as NA
              ldem_pres = qlogis(potus_dem_pct_two_way)
    ) |>
    arrange(year) |>
    filter(year >= 1970)

## 2022 -----

d_22 <- d_cand_raw |>
    filter(year == 2022) |>
    transmute(year = 2022L,
              state = state_code,
              district = as.integer(str_sub(district, 4)),
              inc_seat = factor(if_else(dem_inc == 1, "dem",
                                        if_else(gop_inc == 1, "gop", "open")),
                                levels=c("dem", "open", "gop")),
              unopp = is.na(dem_candidate) | is.na(gop_candidate),
              ldem_seat = NA_real_)

d_pres_20 = map_dfr(state.abb, function(abbr) {
    cat(abbr, "\n")
    if (abbr == "SD") {
        return(tibble(year=2022, state="SD", district=1L,
                      inc_pres = "dem",
                      ldem_pres = log(150471) - log(261043)))
    }
    st_map = alarmdata::alarm_50state_map(abbr)
    if (!"pre_20_dem_bid" %in% colnames(st_map)) {
        st_map = select(st_map, GEOID, cd_2020, geometry) |>
            left_join(alarmdata::alarm_census_vest(abbr, geometry=FALSE),
                      by=c("GEOID"="GEOID20"))
    }
    st_drop_geometry(st_map) |>
        group_by(state, district=cd_2020) |>
        summarize(ldem_pres = log(sum(pre_20_dem_bid)) - log(sum(pre_20_rep_tru))) |>
        mutate(year = 2022, inc_pres = "dem")
})

d_22 <- full_join(d_22, d_pres_20, by=c("year", "state", "district"))

# FEC data
d_dk_raw <- read_sheet("1WveGDjicdkFlTcuW1MYwXzvrzf78igsi7Gz9d_X8qa8", sheet="House")
d_cand_names <- read_csv(here("data-raw/dfp/house_candidates_2010_2022.csv"), show_col_types=FALSE) |>
    filter(year == 2022) |>
    select(district, dem_candidate, gop_candidate) |>
    pivot_longer(dem_candidate:gop_candidate, names_to="party",
                 names_pattern="([a-z]+)_candidate", values_to="name") |>
    drop_na() |>
    mutate(initials = str_replace_all(str_c(" ", str_squish(name)), "[ -.](\\w)(\\w*)", "\\1"))
d_fec22 <- d_dk_raw |>
    clean_names() |>
    transmute(district = str_replace(cd, "-AL", "-01"),
              name = str_to_upper(actual_name),
              initials = str_replace_all(str_c(" ", str_squish(name)), "[ -.](\\w)(\\w*)", "\\1"),
              party = if_else(str_detect(party, "\\(D"), "dem", "gop"),
              exp = spent_ctd) |>
    inner_join(d_cand_names, c("district", "initials", "party"), suffix=c("_fec", "_dfp")) |>
    group_by(district, party) |>
    summarize(exp = sum(exp),
              .groups="drop") |>
    transmute(state = str_sub(district, 1, 2),
              district = as.integer(str_sub(district, 4)),
              party = if_else(party == "dem", "dem", "rep"),
              exp = exp) |>
    pivot_wider(names_from=party, values_from=exp, names_glue="{party}_exp")


d_22 <- left_join(d_22, d_fec22, by=c("state", "district"))


d_hist <- bind_rows(d_hist, d_22)

# Link to presidential vote shares and generic ballot
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
    summarize(ldem_pres_natl = log(sum(dem)) - log(sum(rep))) |>
    complete(year = unique(d_hist$year)) |>
    fill(ldem_pres_natl) |>
    drop_na() |>
    rows_insert(tibble(year=c(1974, 1976),
                       ldem_pres_natl=log(29173222) - log(47168710)),
                by="year")

d_fund <- read_csv(here("data-raw/produced/fundamentals_basic.csv"), show_col_types=FALSE) |>
    transmute(year = year,
              ldem_gen = if_else(dem_pres == 1, linc_vote, -linc_vote))

d_hist <- d_hist |>
    left_join(d_pres, by="year") |>
    left_join(d_fund, by="year")

d_polar = d_hist |>
    filter(unopp == 0, !is.infinite(ldem_seat)) |>
    drop_na(ldem_seat, ldem_pres) |>
    group_by(year) |>
    summarize(polar = cor(ldem_seat, ldem_pres)) |>
    mutate(year = year + 2) # lag

d_hist <- d_hist |>
    left_join(d_polar, by="year")

# Candidate names ------

proc_name = function(x) {
    x = x |>
        str_replace_all("-", " ") |>
        str_replace_all("(^|\\s+)[A-Z]($|\\s+)", " ") |>
        str_remove_all(" (SR\\.?|JR\\.?|II|III|IV)") |>
        str_squish()
    fname = word(x, 1, 1)
    lname = word(x, -1)
    if_else(lname %in% c("SANCHEZ", "HERNANDEZ", "DAVIS", "ANDERSON"),
        str_c(fname, " ", lname),
        str_c(str_sub(fname, 1, 1), ". ", lname)
    )
}

d_cand_raw <- read_csv(here("data-raw/dfp/house_candidates_2010_2022.csv"), show_col_types=FALSE)
d_medsl_raw <- read_csv(here("data-raw/medsl/1976-2020-house.csv"), show_col_types=FALSE)

d_medsl_names <- d_medsl_raw |>
    group_by(year, state, district, party) |>
    arrange(desc(candidatevotes)) |>
    slice_head(n=1) |>
    ungroup() |>
    filter(str_starts(party, "(DEMOCRAT|REPUBLICAN)")) |>
    transmute(year = year,
              state = state_po,
              district = as.integer(district),
              district = if_else(district == 0, 1L, district),
              party = str_to_lower(str_sub(party, 1, 3)),
              candidate = proc_name(candidate)) |>
    pivot_wider(names_from=party, values_from=candidate, names_glue="{party}_{.value}")

d_cand_names <- d_cand_raw |>
    separate(district, c(NA, "district"), sep="-") |>
    transmute(year = year,
              state = state_code,
              district = as.integer(district),
              dem_candidate = proc_name(dem_candidate),
              rep_candidate = proc_name(gop_candidate))

d_names = full_join(d_cand_names, d_medsl_names,
                    by=c("state", "year", "district"), suffix=c("_dfp", "_medsl")) |>
    mutate(dem_dist = coalesce(mapply(adist, dem_candidate_dfp, dem_candidate_medsl), 0),
           rep_dist = coalesce(mapply(adist, rep_candidate_dfp, rep_candidate_medsl), 0),
           dem_cand = case_when(dem_dist == 0 ~ coalesce(dem_candidate_dfp, dem_candidate_medsl),
                                rep_dist == 0 & dem_dist < 5 ~ coalesce(dem_candidate_dfp, dem_candidate_medsl),
                                TRUE ~ NA_character_),
           rep_cand = case_when(rep_dist == 0 ~ coalesce(rep_candidate_dfp, rep_candidate_medsl),
                                dem_dist == 0 & rep_dist < 5 ~ coalesce(rep_candidate_dfp, rep_candidate_medsl),
                                TRUE ~ NA_character_)) |>
    arrange(year) |>
    select(year, state, district, dem_cand, rep_cand) |>
    filter(!(is.na(dem_cand) & is.na(rep_cand)))


# Historical demographics ---------
raw <- read_csv(here("data-raw/ipums/nhgis0001_ts_nominal_state.csv"),
                show_col_types=FALSE, comment="GIS Join Match Code")

d_demg <- raw |>
    transmute(year_start = as.integer(str_split(raw$YEAR, "-", n=2, simplify=T)[, 1]),
              year_end = coalesce(as.integer(str_split(raw$YEAR, "-", n=2, simplify=T)[, 2]), year_start + 9L),
              state = map_chr(STATE, censable::match_abb),
              pop = AV0AA,
              urban = A57AB / AV0AA,
              suburban = A57AC / AV0AA,
              rural = A57AD / AV0AA,
              age_u35 = (B57AE + B57AF + B57AG + B57AH + B57AI + B57AJ) / AV0AA,
              age_3565 = (B57AK + B57AL + B57AM + B57AN + B57AO) / AV0AA,
              age_o65 = (B57AP + B57AQ + B57AR) / AV0AA,
              white = B18AA / AV0AA,
              black = B18AB / AV0AA,
              hisp_other = 1 - (B18AA + B18AB)/AV0AA,
              edu_u9 = B69AA / (B69AA + B69AB + B69AC),
              edu_915 = B69AB / (B69AA + B69AB + B69AC),
              edu_o15 = B69AC / (B69AA + B69AB + B69AC),
              inc_u10k = BS7AA / (BS7AA + BS7AB + BS7AC + BS7AD),
              inc_1015k = BS7AB / (BS7AA + BS7AB + BS7AC + BS7AD),
              inc_1525k = BS7AC / (BS7AA + BS7AB + BS7AC + BS7AD),
              inc_o25k = BS7AD / (BS7AA + BS7AB + BS7AC + BS7AD),
              pov = CL6AA / AV0AA) |>
    arrange(year_start, year_end) |>
    group_by(state) |>
    fill(everything(), .direction="down") |>
    ungroup()

yr_lookup = distinct(d_demg, year_start, year_end)
yr_lookup = map_dfr(as.double(unique(d_hist$year)), function(year) {
    idx = which.min(with(yr_lookup, case_when(
        year < year_start ~ year_start - year,
        year <= year_end ~ abs(year - year_start) / (year_end - year_start) - 1,
        TRUE ~ year - year_end
    )))
    bind_cols(tibble(year=as.integer(year)), yr_lookup[idx, ])
})

d_demg <- left_join(d_demg, yr_lookup, by=c("year_start", "year_end")) |>
    select(-year_start, -year_end) |>
    relocate(year, .before=everything()) |>
    arrange(year)

d <- d_hist |>
    inner_join(d_demg, by=c("year", "state")) |>
    filter(year >= 1976, state != "DC") |>
    left_join(d_names, by=c("year", "state", "district")) |>
    left_join(select(censable::stata, state=abb, region, division), by="state")


write_csv(d, here("data-raw/produced/hist_house_races.csv.gz"))

