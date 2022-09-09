suppressMessages({
    library(dplyr)
    library(stringr)
    library(readr)
    library(lubridate)
    library(tidybayes)
    library(here)
    source(here("R/utils.R"))
})

build_stan_model <- function(recompile=FALSE, quiet=TRUE, optim=TRUE) {
    stanc_opt = if (isTRUE(optim)) list("O1") else list("O0")
    sm = cmdstan_model(here("stan/intent.stan"),
                       stanc_options=stanc_opt,
                       quiet=quiet)
    if (isTRUE(recompile)) sm$compile(force_recompile=TRUE)
    sm
}

summarize_natl <- function(draws, election_date) {
    spread_draws(draws, natl_dem[day]) |>
        suppressWarnings() |>
        point_interval(.width=c(0.6666, 0.9, 0.99)) |>
        select(-.point, -.interval) |>
        mutate(date = election_date + 1 - day)
}

make_stan_data_intent <- function(d_polls, year, min_date, max_date, election_date) {
    m_firms <- read_rds(here(str_glue("data/firms_fit/firms_fit_{year}.rds")))
    pred_fund <- read_rds(here(str_glue("data/fund_pred/fundamentals_pred_{year}.rds")))

    min_tte = as.integer(election_date - max_date)
    max_tte = as.integer(election_date - min_date)

    types = names(m_firms$loc$r_types)

    d_fit = filter(d_polls, tte >= min_tte, tte <= max_tte) |>
        mutate(day = as.integer(tte + 1),
               type = if_else(type == "text", "mixed", type),
               type = if_else(type %in% types, type, types[1]),
               type = factor(type, levels=types),
               firm_id = as.factor(firm_id)) |>
        arrange(tte)

    obs_firm_ids = as.integer(levels(d_fit$firm_id))
    known_firm_ids = as.integer(head(names(m_firms$firms), -1)) # 'other' is last
    firm_lookup = match(obs_firm_ids, known_firm_ids)
    firm_lookup[is.na(firm_lookup)] = length(known_firm_ids) + 1 # match to 'other'

    pred_fit = suppressWarnings(MASS::fitdistr(pred_fund, "t"))
    inc_pres = list(`2018`="rep", `2020`="rep", `2022`="dem")[as.character(year)]
    flip_pred = if (inc_pres == "dem") 1 else -1

    list(
        N = nrow(d_fit),
        Y = d_fit$est,

        N_days = max_tte + 1L,
        day = d_fit$day,

        nu_delta = 5.0,
        prior_sd_delta_shape = 4.0,
        prior_sd_delta_loc = 1.0,
        chol_years = chol(cor(cbind(m_firms$loc$r_years, m_firms$loc$r_years_shared)))[,2],

        prior_eday_loc = pred_fit$estimate["m"] * flip_pred,
        prior_eday_scale = pred_fit$estimate["s"],
        prior_eday_df = pred_fit$estimate["df"],

        N_firms = length(obs_firm_ids),
        N_types = length(m_firms$loc$r_types),
        firms = as.integer(d_fit$firm_id),
        firm_ids = obs_firm_ids,
        types = as.integer(d_fit$type),
        not_lv = as.double(d_fit$not_lv),

        K_sigma = 3,
        X_sigma = with(d_fit, cbind(log(n), sqrt(tte), not_lv)),

        prior_z_firms_loc = m_firms$loc$r_firms[firm_lookup],
        prior_z_sigma_firms_loc = m_firms$loc$r_sigma_firms[firm_lookup],
        prior_z_herding_loc = m_firms$loc$m_herding[firm_lookup],
        prior_z_types_loc = m_firms$loc$r_types,

        prior_z_firms_scale = m_firms$scale$r_firms[firm_lookup],
        prior_z_sigma_firms_scale = m_firms$scale$r_sigma_firms[firm_lookup],
        prior_z_herding_scale = m_firms$scale$m_herding[firm_lookup] * 1.1, # be slightly less confident
        prior_z_types_scale = m_firms$scale$r_types,

        prior_bias_loc = m_firms$loc$bias,
        prior_b_intercept_sigma_loc = m_firms$loc$b_sigma_intercept,
        prior_b_sigma_loc = m_firms$loc$b_sigma,
        prior_bias_scale = m_firms$scale$bias,
        prior_b_intercept_sigma_scale = m_firms$scale$b_sigma_intercept,
        prior_b_sigma_scale = m_firms$scale$b_sigma,

        sd_firms = m_firms$loc$sd_firms,
        sd_sigma_firms = m_firms$loc$sd_sigma_firms,
        sd_herding = m_firms$loc$sd_herding,
        sd_types = m_firms$loc$sd_types,
        sd_years = m_firms$loc$sd_years,
        sd_years_shared = m_firms$loc$sd_years_shared,
        sd_lv = m_firms$loc$sd_lv
    )
}
