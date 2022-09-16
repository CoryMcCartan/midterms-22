library(tidyverse)
library(here)
library(dbarts)
library(matrixStats)

# Load data --------

d <- read_csv(here("data-raw/produced/hist_house_races.csv.gz"), show_col_types=FALSE)

d_fit <- d |>
    drop_na(ldem_gen) |>
    mutate(ldem_pres_adj = ldem_pres - ldem_pres_natl,
           ldem_pred = ldem_pres_adj + ldem_gen,
           midterm = 1*(year %% 4 == 2),
           div_yr = str_c(division, "_", year)) |>
    filter(unopp == 0, !is.infinite(ldem_seat), midterm == 1)


form = ldem_seat ~ inc_seat + inc_pres + ldem_pres_adj + polar + ldem_pred + ldem_gen +
    age_u35 + white + black + edu_o15 + pov + suburban + rural + division + year
pred_vars = attr(terms(form), "term.labels")

d_imp <- d |>
    filter(year >= 2008, year < 2022, unopp == 1) |>
    mutate(ldem_pres_adj = ldem_pres - ldem_pres_natl,
           ldem_pred = ldem_pres_adj + ldem_gen,
           midterm = 1*(year %% 4 == 2),
           div_yr = str_c(division, "_", year)) |>
    filter(if_any(all_of(pred_vars), ~ !is.na(.x))) |>
    mutate(i = seq_len(n()), .before=everything())


m_bart = bart2(form, data=d_fit, test=d_imp,
               n.thin=10L, power=1.8, n.trees=300L, n.samples=2000L, keepTrees=TRUE)

res_bart = resid(m_bart)
fit_bart = fitted(m_bart)
yardstick::rsq_vec(fit_bart, d_fit$ldem_seat)

draws_pred = plogis(m_bart$yhat.test)
dim(draws_pred) = c(prod(dim(draws_pred)[1:2]), nrow(d_imp))

d_pred = tibble(i = seq_len(nrow(d_imp)),
       dem_seat.pred = colMeans(draws_pred),
       dem_seat.lower = colQuantiles(draws_pred, probs=0.05),
       dem_seat.upper = colQuantiles(draws_pred, probs=0.95),
       dem_seat.width = 0.9)

N_mi = 20
d_mi = tibble(i = rep(seq_len(nrow(d_imp)), each=N_mi),
              .imputation = rep(seq_len(N_mi), nrow(d_imp)),
              dem_seat = as.numeric(draws_pred[1:N_mi, ]))

d_uncont_skinny = d_imp |>
    mutate(winner = if_else(ldem_seat > 0, "dem", "gop")) |>
    select(i, year, state, district, inc_seat, winner)

left_join(d_uncont_skinny, d_pred, by="i") |>
    select(-i) |>
write_csv(here("data-raw/produced/hist_house_uncontested_impute_sum.csv"))

left_join(d_uncont_skinny, d_mi, by="i") |>
    select(-i) |>
    arrange(.imputation) |>
write_csv(here("data-raw/produced/hist_house_uncontested_impute_multiple.csv.gz"))

