#!/usr/bin/env Rscript

#####################################
#     U.S. HOUSE MODEL 2022         #
#     CORY McCARTAN                 #
#     (c) 2022                      #
#####################################

start_date = as.Date("2022-03-01")
elec_date = as.Date("2022-11-08")

# Parse options -----
library(optparse)

option_list = list(
    make_option("--dry", action="store_true", default=F,
                help="Dry run, results not saved."),
    make_option("--date", type="character",
                default=as.character(min(max(Sys.Date(), start_date), elec_date)),
                help=paste0("The date to estimate from, between
                    Between ", start_date, " and ", elec_date, " (election day).")),
    make_option("--iter", type="integer", default=400,
                help="Number of MCMC iterations for voter intent estimation per chain,
                    not including warmup iterations."),
    make_option("--chains", type="integer", default=4,
                help="Number of MCMC chains for voter intent estimation."),
    make_option("--recompile", action="store_true", default=F,
                help="Force recompile of Stan model."),
    make_option("--refresh_polls", action="store_true", default=F,
                help="Force redownloading of polls")
)
opt = parse_args(OptionParser(option_list=option_list,
                              description="Forecast the 2022 U.S. House election."))

suppressMessages({
    library(cli)
    library(tidyverse)
    library(lubridate)
    library(cmdstanr)
    library(posterior)
    library(brms)
    library(jsonlite)
    library(here)
    source(here("R/utils.R"))
    source(here("R/model/intent.R"))
    source(here("R/model/load_polls.R"))
})

from_date = as_date(opt$date)

# Load data -----
cli_h1("Loading data")

# approval
d_fund_22 <- read_csv(here("data/fundamentals.csv"), show_col_types=FALSE) |>
    filter(year == 2022) |>
    mutate(econ = 0.678*gdp_chg - 0.038*lunemp - 0.734*cpi_chg) # 1948-2006 PCA weights (see `fundamentals.R`)
d_fund_22$lg_approval = qlogis(get_latest_approval(from_date, force=opt$refresh_polls))
cli_alert_success("Fundamentals data loaded.")

# GCB polls
d_polls <- load_polls(2022, from_date=from_date, force=opt$refresh_polls)
cli_alert_success("Generic Congressional ballot polls loaded.")


# Estimate nat'l intent -----
cli_h1("Estimating national intent")

## Fundamentals -----
m_fund = read_rds(here("data-raw/produced/fundamentals_model_2022.rds"))
pred_fund = posterior_predict(m_fund, newdata=d_fund_22)[, 1]
rm(m_fund) # save RAM

## Intent -----

sm <- build_stan_model()
stan_d = make_stan_data_intent(d_polls, 2022, start_date, from_date, elec_date,
                               pred_fund=pred_fund)

m_gcb = sm$sample(stan_d, init=1, seed=5118,
                  chains=opt$chains, parallel_chains=4,
                  iter_warmup=800, iter_sampling=opt$iter,
                  adapt_delta=0.97, max_treedepth=11, refresh=0) |>
    suppressMessages()
cli_alert_success("Intent model fit.")

early_vote_len = 14
vote_period_wt = rep(0, stan_d$N_days)
vote_period_wt[1] = 0.65 # reasonable estimate of E-day vote from previous years
vote_period_wt[1 + seq_len(early_vote_len)] = (1 - vote_period_wt[1]) *
    rep(1/early_vote_len, early_vote_len)

draws_gcb = as_draws_rvars(m_gcb$draws("mu"))
pred_natl = rvar_sum(draws_gcb$mu * vote_period_wt)
cli_alert_success("Draws extracted.")


# Predict outcomes -----
cli_h1("Forecasting seat outcomes")

m_outcomes = read_rds(here("stan/outcomes_m.rds"))
d_house_22 = read_csv(here("data-raw/produced/hist_house_races.csv.gz"), show_col_types=FALSE) |>
    filter(year == 2022) |>
    mutate(ldem_pres_adj = ldem_pres - ldem_pres_natl,
           inc_seat = coalesce(inc_seat, "open"),
           unopp = coalesce(unopp, 0)) |>
    rows_update(tibble(state="AK", district=1, inc_seat="dem"), by=c("state", "district"))
d_house_pred = d_house_22 |>
    mutate(midterm = 1*(year %% 4 == 2),
           ldem_exp = log(dem_exp) - log(rep_exp),
           exp_mis = 1*(is.na(ldem_exp) | is.infinite(ldem_exp)),
           ldem_exp = if_else(exp_mis == 1, 0, ldem_exp),
           div_yr = str_c(division, "_", year)) |>
    filter(unopp == 0)
d_house_unopp = d_house_22 |>
    filter(unopp == 1) |>
    mutate(pr_dem = if_else(inc_seat == "dem", 1, 1*(ldem_pres > ldem_pres_natl)))

# mix the national intent and outcome draws
N_mix_natl = 40 # how many draws from the national intent to use
pr_mix_natl = seq(0.5/N_mix_natl, 1, 1/N_mix_natl)
mix_natl = quantile(draws_of(pred_natl)[, 1], probs=pr_mix_natl, names=FALSE) # evenly space natl draws (reduce var)

N_outcomes = sum(m_outcomes$fit@sim$n_save)
N_per_chunk = N_outcomes %/% N_mix_natl
iter_grp = lapply(seq_len(N_mix_natl), \(i) (i-1)*N_per_chunk + seq_len(N_per_chunk))

pb = cli_progress_along(1:N_mix_natl, name="Combining draws")
m_pred = do.call(rbind, map(pb, function(i) {
    d_tmp = d_house_pred |>
        mutate(ldem_gen = mix_natl[i],
               ldem_pred = ldem_pres_adj + ldem_gen)
    posterior_predict(m_outcomes, newdata=d_tmp, draw_ids=iter_grp[[i]],
                      sample_new_levels="uncertainty", cores=1,
                      allow_new_levels=TRUE)
}))
cli_progress_done()
cli_alert_success("Forecast complete.")

pred_seats = sum(d_house_unopp$pr_dem) + rowSums(m_pred > 0)

d_pred_22 = bind_rows(
    d_house_unopp,
    mutate(d_house_pred,
           part_base = plogis(ldem_pres_adj),
           pr_dem = colMeans(m_pred > 0),
           dem_mean = colMeans(plogis(m_pred)),
           dem_q10 = plogis(apply(m_pred, 2, quantile, probs=0.1)),
           dem_q25 = plogis(apply(m_pred, 2, quantile, probs=0.25)),
           dem_q75 = plogis(apply(m_pred, 2, quantile, probs=0.75)),
           dem_q90 = plogis(apply(m_pred, 2, quantile, probs=0.9)))
) |>
    select(state, district, inc_seat, unopp, part_base, pr_dem, dem_mean:dem_q90) |>
    arrange(state, district)


# Output ------

out = list(
    s_prob = mean(pred_seats >= 218),
    s_med = median(pred_seats),
    s_q10 = quantile(pred_seats, 0.1),
    s_q25 = quantile(pred_seats, 0.25),
    s_q75 = quantile(pred_seats, 0.75),
    s_q90 = quantile(pred_seats, 0.9),
    i_prob = E(pred_natl > 0),
    i_med = plogis(median(pred_natl)),
    i_q10 = plogis(quantile(pred_natl, 0.1)),
    i_q25 = plogis(quantile(pred_natl, 0.25)),
    i_q75 = plogis(quantile(pred_natl, 0.75)),
    i_q90 = plogis(quantile(pred_natl, 0.9))
)

tt_elec = round(elec_date - from_date)
with(out, cat(str_glue("
 ===========================================
  2022 U.S. House Forecast
  {as.character(Sys.Date(), format='%B %d, %Y')}
 -------------------------------------------
  Forecast from: {as.character(from_date, format='%B %d, %Y')}
  {tt_elec} day{if (tt_elec > 1) 's'} until the election.
  {nrow(d_polls)} polls.

  Dem. share of two-party vote:  {sprintf('%.1f%%', 100*i_med)}%
  Median seat estimate:          {round(s_med)}
  80% CI for seats:              {round(s_q10)} - {round(s_q90)}
  Prob. of keeping control:      {round(100*s_prob)}%
  Prob. of winning pop. vote:    {round(100*i_prob)}%
 ===========================================
\n\n")))
system("osascript -e 'display notification \"Model run complete.\" with title \"House Model\"'")

if (isTRUE(opts$dry)) quit(save="no", status=0)

write_json(out, here("out/summary.json"), auto_unbox=TRUE, digits=5)

d_pred_22 |>
    mutate(across(where(is.numeric), round, digits=4)) |>
write_csv(here("out/districts.csv"))

d_intent = summarize_natl(m_gcb$draws("natl_dem"), elec_date, c(0.5, 0.8)) |>
    pivot_wider(names_from=.width, values_from=c(.lower, .upper)) |>
    rename(q10 = .lower_0.8,
           q25 = .lower_0.5,
           q75 = .upper_0.5,
           q90 = .upper_0.8) |>
    mutate(across(where(is.numeric), round, digits=4))
write_csv(d_intent, here("out/natl_intent.csv"))

d_hist = tibble(dem_seats = 1:400,
                pr = tabulate(pred_seats, nbins=400) / length(pred_seats)) |>
    mutate(cdf = cumsum(pr))
write_csv(d_hist, here("out/seats_hist.csv"))

cli_alert_success("Forecast saved.")
