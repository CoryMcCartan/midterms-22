library(tidyverse)
library(rvest)
library(sf)
library(geomander)
library(here)

state_regex = str_c(c(state.abb, "DC"), collapse="|")
parse_station = function(x) str_extract(x, "^[WK][A-Z]{2,3}(-[A-Z]{1,2})?")
tidy_market = function(x) {
    x |>
        # str_replace_all(str_glue(",? ({state_regex})"), " ") |>
        str_replace_all("[-–().,&]", " ") |>
        str_replace_all("Ft ", "Fort ") |>
        str_squish()
}

d_dma = read_sf(here("data-raw/dma/NatDMA1.shp")) |>
    filter(is.na(GEOID)) |>
    transmute(market = tidy_market(NAME),
              geometry = geometry) |>
    st_transform(5070) |>
    st_make_valid()

d_stations = read_html("https://en.wikipedia.org/wiki/List_of_United_States_television_markets") |>
    html_elements("table.wikitable") |>
    pluck(1) |> # first table on the page
    html_table(na.strings="N/A") |>
    repair_names() |>
    rename(rank=1, hh=4) |>
    transmute(market = tidy_market(str_remove_all(Market, "\\[[a-z]+\\]$")),
              state = coalesce(state.abb[match(State, state.name)], "DC"),
              households = parse_number(hh),
              station_nbc = parse_station(NBC),
              station_cbs = parse_station(CBS),
              station_abc = parse_station(ABC),
              station_fox = parse_station(Fox)) |>
    drop_na(market, station_nbc)

m_dist = adist(d_dma$market, d_stations$market, cost=c(1, 2, 10))
idx_match = RcppHungarian::HungarianSolver(m_dist)$pairs[, 2]
d_dma$market = d_stations$market[idx_match]

d_dma = inner_join(d_stations, d_dma, by=c("market")) |>
    st_as_sf()


d_cd = read_sf(here("data-raw/dk/shp_22/2022 U.S. House of Representatives Districts with Water Clipped to Shoreline.shp")) |>
    transmute(state = str_sub(Code, 1, 2),
              district = District,
              geometry = geometry) |>
    st_transform(5070) |>
    rmapshaper::ms_simplify(0.05) |>
    st_make_valid()

d_states = tigris::states(cb=TRUE, resolution="20m") |>
    select(state = STUSPS, geometry) |>
    st_transform(5070) |>
    st_make_valid()

d = suppressWarnings(st_intersection(d_cd, select(d_dma, -state))) |>
    mutate(area = as.numeric(st_area(geometry))) |>
    st_drop_geometry() |>
    group_by(state, district) |>
    filter(area / sum(area) > 0.02) |>
    summarize(households = sum(households),
              area = sum(area) / 1609.34^2,
              markets = list(unique(market)),
              stations = list(unique(c(station_nbc, station_cbs, station_abc, station_fox))),
              .groups = "drop")

write_rds(d, here("data-raw/produced/distr_markets.rds"), compress="gz")


d = suppressWarnings(st_intersection(d_states, select(d_dma, -state))) |>
    mutate(area = as.numeric(st_area(geometry))) |>
    st_drop_geometry() |>
    group_by(state) |>
    filter(area / sum(area) > 0.01) |>
    summarize(households = sum(households),
              area = sum(area) / 1609.34^2,
              markets = list(unique(market)),
              stations = list(unique(c(station_nbc, station_cbs, station_abc, station_fox))),
              .groups = "drop")

write_rds(d, here("data-raw/produced/state_markets.rds"), compress="gz")


st_drop_geometry(d_dma) |>
    pivot_longer(-market:-households, names_prefix="station_",
                 names_to="affiliate", values_to="station") |>
write_rds(here("data-raw/produced/media_markets.rds"), compress="gz")
