library(tidyverse)
library(sf)
library(jsonlite)
library(here)

d_hex = read_rds(here("data-raw/manual/districts_2020_hex.rds")) |>
    st_sf() |>
    rename(district=cd_2020) |>
    mutate(geometry = rmapshaper::ms_simplify(geometry, keep=0.25)) |>
    select(-geom_label) |>
    st_transform(4326)

# fix permutation
d_hex$district[d_hex$state == "GA"] = c(5L, 11L, 4L, 7L, 6L, 9L, 10L, 2L, 3L, 1L, 8L, 12L, 14L, 13L)
d_hex$district[d_hex$state == "IN"] = c(1L, 2L, 3L, 5L, 4L, 7L, 6L, 9L, 8L)
d_hex = arrange(d_hex, state, district)

json = geojsonio::topojson_json(d_hex, quantization=1e5)
write_file(json, here("docs/hex.json"))
