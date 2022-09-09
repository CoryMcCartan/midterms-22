library(tidyverse)
library(brms)
library(here)

raw = read_csv(here("data-raw/rochester/allCongressDataPublishV2.csv"), show_col_types=FALSE)

raw2 = read_csv(here("data-raw/dfp/clean_us_house_district_results.csv"), show_col_types=FALSE)

raw3 = read_csv(here("data-raw/dfp/house_candidates_2010_2022.csv"), show_col_types=FALSE)
