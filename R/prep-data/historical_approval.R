library(tidyverse)
library(googlesheets4)
library(janitor)
library(here)

sheet_id = "1iEl565M1mICTubTtoxXMdxzaHzAcPTnb3kpRndsrfyY"
ss = gs4_get(sheet_id)

d = map_dfr(ss$sheets$name, function(name) {
    cat("Getting data for", name, "\n")
    read_sheet(sheet_id, sheet=name) |>
        clean_names() |>
        transmute(president = str_to_lower(word(name, -1)),
                  start_date = as.Date(start_date),
                  end_date = as.Date(end_date),
                  approve = approving / 100,
                  disapprove = disapproving / 100)
})

write_csv(d, here("data/past_pres_approval.csv"))
