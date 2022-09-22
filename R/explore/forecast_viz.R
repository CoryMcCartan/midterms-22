library(tidyverse)
library(sf)
library(ggredist)
library(scales)
library(here)

d_seats = read_csv(here("out/districts.csv"), show_col_types=FALSE)
d_hex = read_rds(here("data-raw/manual/districts_2020_hex.rds")) |>
    st_sf() |>
    rename(district = cd_2020) |>
    mutate(geometry = rmapshaper::ms_simplify(geometry, keep=0.5))

d_hex |>
    left_join(d_seats, by=c("state", "district")) |>
ggplot(aes(fill=pr_dem)) +
    geom_sf(size=0.2, color="black") +
    geom_sf_text(aes(geometry=geom_label, label=district), size=2.2) +
    scale_fill_party_c() +
    guides(fill="none") +
    theme_void()

ggsave("~/Desktop/seat_map_09-22.pdf", width=11, height=8)
