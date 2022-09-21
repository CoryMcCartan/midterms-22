library(tidyverse)
library(sf)
library(jsonlite)
library(here)

d_hex = read_rds(here("data-raw/manual/districts_2020_hex.rds")) |>
    st_sf() |>
    rename(district=cd_2020) |>
    mutate(geometry = rmapshaper::ms_simplify(geometry, keep=0.25)) |>
    select(-geom_label)

json = geojsonio::topojson_json(d_hex)
write_file(json, here("out/hex.json"))
