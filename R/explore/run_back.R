library(here)
library(glue)

dates = as.Date("2022-06-03") + seq(4, 125, 3) - 1

for (date in as.character(dates)) {
    system(glue('{here("R/run.R")} --date="{date}" --n_mix=10'))
}
