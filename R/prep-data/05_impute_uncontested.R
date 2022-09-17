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
    filter(year < 2022, unopp == 1) |>
    mutate(ldem_pres_adj = ldem_pres - ldem_pres_natl,
           ldem_pred = ldem_pres_adj + ldem_gen,
           midterm = 1*(year %% 4 == 2),
           div_yr = str_c(division, "_", year)) |>
    filter(if_any(all_of(pred_vars), ~ !is.na(.x))) |>
    mutate(i = seq_len(n()), .before=everything())


m_bart = bart2(form, data=d_fit, test=d_imp,
               n.thin=10L, n.trees=400L, n.samples=2000L, keepTrees=TRUE)

cor(fitted(m_bart), d_fit$ldem_seat)^2 # R^2

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


# rebuild generic ballot estimates and export

d_fund <- read_csv(here("data-raw/produced/fundamentals_basic.csv"), show_col_types=FALSE)

d_wgt_cont = d |>
    group_by(year) |>
    summarize(wgt_contest = (n() - sum(unopp)) / n())

# uncontested seats by winning party and year
d |>
    filter(unopp == 1) |>
    mutate(winner = if_else(ldem_seat > 0, "dem", "gop")) |>
    group_by(year, winner) |>
    summarize(value = n(), .groups="drop") |>
    pivot_wider(names_from=winner) |>
    select(year:gop) |>
    print(n=25)

d_uncont = left_join(d_uncont_skinny, d_pred, by="i") |>
    group_by(year) |>
    summarize(dem_uncont = mean(plogis(dem_seat.pred)))


d_fund2 = d_fund |>
    left_join(d_wgt_cont, by="year") |>
    left_join(d_uncont, by="year") |>
    left_join(distinct(d, year, polar), by="year") |>
    mutate(dem_vote_contest = plogis((2*dem_pres - 1)*linc_vote_contest),
           dem_vote_imp = wgt_contest*dem_vote_contest + (1-wgt_contest)*dem_uncont,
           linc_vote_imp = (2*dem_pres - 1) * qlogis(dem_vote_imp))

ggplot(d_fund2, aes(linc_vote, linc_vote_imp, label=year)) +
    geom_abline(slope=1, color="red") +
    geom_text(size=3)

d_fund2 |>
    select(year, linc_vote_imp, linc_vote, nomidterm=midterm,
           inc_house, dem_pres, polar, gdp_chg:lg_approval, lg_retire) |>
    mutate(across(where(is.numeric), round, 5)) |>
    write_csv(here("data/fundamentals.csv"))

d_ldem_gen = transmute(d_fund2, year=year, ldem_gen=qlogis(d_fund2$dem_vote_imp))

d |>
    select(-ldem_gen) |>
    left_join(d_ldem_gen, by="year") |>
    relocate(ldem_gen, .before=polar) |>
    write_csv(here("data-raw/produced/hist_house_races.csv.gz"))

# export for DFP

left_join(d_uncont_skinny, d_pred, by="i") |>
    filter(year >= 2008) |>
    select(-i) |>
write_csv(here("data-raw/produced/hist_house_uncontested_impute_sum.csv"))

left_join(d_uncont_skinny, d_mi, by="i") |>
    filter(year >= 2008) |>
    select(-i) |>
    arrange(.imputation) |>
write_csv(here("data-raw/produced/hist_house_uncontested_impute_multiple.csv.gz"))

