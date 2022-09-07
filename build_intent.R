library(tidyverse)
library(cmdstanr)
library(posterior)
library(tidybayes)
library(here)
library(geomtextpath)
source(here("R/utils.R"))
source(here("R/model/intent.R"))
source(here("R/model/load_polls.R"))

e_days = list(
    `2010` = as.Date("2010-11-02"),
    `2012` = as.Date("2012-11-06"),
    `2014` = as.Date("2014-11-04"),
    `2016` = as.Date("2016-11-08"),
    `2018` = as.Date("2018-11-06"),
    `2020` = as.Date("2020-11-03"),
    `2022` = as.Date("2022-11-08")
)

year = 2020
for (year in seq(2010, 2022, by=2)) {
    min_date = as.Date(str_glue("{year}-03-01"))
    max_date = e_days[[as.character(year)]]
    election_date = e_days[[as.character(year)]]

    # Prep data and fit ------

    d_polls <- load_polls(year, limit_per_firm=100)
    sm <- build_stan_model()

    stan_d = make_stan_data_intent(d_polls, year, min_date, max_date, election_date)

    fit = sm$sample(stan_d, init=0.1, seed=5118,
                    chains=4, parallel_chains=4,
                    iter_warmup=800, iter_sampling=1200,
                    adapt_delta=0.97, max_treedepth=11, refresh=400) |>
        suppressMessages()

    draws <- as_draws_rvars(fit$draws())

    d_natl <- summarize_natl(draws, election_date)
    plot_time(d_natl, natl_dem, election_date,
              ylab="Democratic two-party national vote share",
              thin=if (year >= 2018) 4 else 7)

    ggsave(here(str_glue("doc/intent_backtest_{year}_herd.svg")), width=8, height=3)
}

# Diagnose ------

if (FALSE) {
    fit$cmdstan_diagnose()

    bayesplot::mcmc_trace(fit$draws("mu[1]"))
    bayesplot::mcmc_trace(fit$draws("r_year"))
    bayesplot::mcmc_hist(fit$draws("r_year"), binwidth=0.002)
    as_draws_rvars(fit$draws("r_year"))$r_year
    bayesplot::mcmc_trace(fit$draws("sd_delta"))
    # bayesplot::mcmc_trace(fit$draws("m_herding[10]"))
    bayesplot::mcmc_pairs(fit$draws(c("sd_delta", "delta[1]")))
    bayesplot::mcmc_pairs(fit$draws(c("delta[1]", "delta[2]")))
    bayesplot::mcmc_pairs(fit$draws(c("delta[1]", "r_year")))

    # plot(E(as_draws_rvars(fit$draws("m_herding"))$m_herding), sd(as_draws_rvars(fit$draws("m_herding"))$m_herding))
    bayesplot::mcmc_hist(fit$draws("natl_dem[1]"), binwidth=0.002)
    hist(2*as_draws_matrix(fit$draws("natl_dem[1]")) - 1)
    2*as_draws_rvars(fit$draws("natl_dem[1]"))$natl_dem - 1
}

# Exploration ------


## poll impact ----
m_ll = -draws_of(draws$log_lik, with_chains=TRUE)
r_eff = loo::relative_eff(exp(m_ll))
psis = loo::psis(m_ll, r_eff=r_eff, cores=4)
est = 100*draws_of(draws$natl_dem[1])
poll_eff = apply(psis$log_weights, 2, function(lw) {
    w = exp(lw)
    cov(w / mean(w), est)
})
qplot(stan_d$Y, poll_eff, color=stan_d$firms)


## weight over the voting period ----
wt = rep(0, stan_d$N_days)
wt[1] = 0.65 # reasonable estimate of E-day vote from previous years
wt[2:15] = (1 - wt[1]) * rep(1/14, 14)

pred_natl = rvar_sum(draws$mu * wt)
suppressWarnings(MASS::fitdistr(draws_of(pred_natl), "t"))
suppressWarnings(MASS::fitdistr(plogis(draws_of(pred_natl)), "t"))
