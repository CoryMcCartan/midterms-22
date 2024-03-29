library(tidyverse)
library(rvest)
library(sf)
library(geomander)
library(here)

state_regex = str_c(c(state.abb, "DC"), collapse="|")
parse_station = function(x) str_extract(x, "^[WK][A-Z]{2,3}(-[A-Z]{1,2})?")
tidy_market = function(x) {
    x |>
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
    filter(state != "PR") |>
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

if (FALSE) {
    library(wacolors)
    library(scales)

    d_rates = read_csv("data-raw/produced/ad_rates.csv", show_col_types=FALSE)
    d_dma = left_join(d_dma, d_rates)

    p = ggplot(tigris::shift_geometry(d_dma), aes(fill=rate_imp)) +
        geom_sf(size=0.1, color="black") +
        geom_sf(data=tigris::shift_geometry(d_states),
                size=0.15, color="#dadada", fill=NA, inherit.aes=FALSE) +
        scale_fill_wa_c(name="Ad rate", labels=dollar) +
        theme_void() +
        theme(legend.position=c(0.92, 0.3))
    ggsave("data-raw/figures/ad_rates.pdf", plot=p, width=11, height=8.5)

    d_markets = d |>
        select(-stations) |>
        unnest(markets) |>
        left_join(select(d_rates, market, rate_imp), by=c("markets"="market")) |>
        group_by(state, district, households, area) |>
        summarize(cost = sum(rate_imp), .groups="drop") |>
        left_join(d_cd, by=c("state", "district")) |>
        st_as_sf()

    p = ggplot(tigris::shift_geometry(d_markets), aes(fill=cost)) +
        geom_sf(size=0.1, color="black") +
        geom_sf(data=tigris::shift_geometry(d_states),
                size=0.15, color="#dadada", fill=NA, inherit.aes=FALSE) +
        scale_fill_wa_c(name="Ad cost", labels=dollar) +
        theme_void() +
        theme(legend.position=c(0.92, 0.3))
    ggsave("data-raw/figures/distr_ad_costs.pdf", plot=p, width=11, height=8.5)
}
