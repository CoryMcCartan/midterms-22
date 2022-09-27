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

run_forecast <- function(elec_date, start_date, from_date=Sys.Date(),
                         refresh_polls=FALSE, chains=4, iter=800, N_mix_natl=40) {
    # Load data -----
    cli_h1("Loading data")

    # approval
    d_fund_22 <- read_csv(here("data/fundamentals.csv"), show_col_types=FALSE) |>
        filter(year == 2022) |>
        mutate(econ = 0.678*gdp_chg - 0.038*lunemp - 0.734*cpi_chg) # 1948-2006 PCA weights (see `fundamentals.R`)
    d_fund_22$lg_approval = qlogis(get_latest_approval(from_date, force=refresh_polls))
    cli_alert_success("Fundamentals data loaded.")

    # GCB polls
    d_polls <- load_polls(2022, from_date=from_date, force=refresh_polls)
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
                      chains=chains, parallel_chains=4,
                      iter_warmup=800, iter_sampling=iter,
                      adapt_delta=0.97, max_treedepth=11, refresh=0) |>
        suppressMessages()
    cli_alert_success("Intent model fit.")

    # average intent over early voting period + E-day
    early_vote_len = 14
    vote_period_wt = rep(0, stan_d$N_days)
    vote_period_wt[1] = 0.65 # reasonable estimate of E-day vote from previous years
    vote_period_wt[1 + seq_len(early_vote_len)] = (1 - vote_period_wt[1]) *
        rep(1/early_vote_len, early_vote_len)

    draws_gcb = as_draws_rvars(m_gcb$draws(c("mu", "bias", "r_firms", "log_lik")))
    pred_natl = rvar_sum(draws_gcb$mu * vote_period_wt)
    cli_alert_success("Draws extracted.")

    # LOO-based poll impact score
    ll = as_draws_array(draws_gcb$log_lik)
    reff = loo::relative_eff(exp(-ll))
    psis_obj = suppressWarnings(loo::psis(ll, r_eff=reff))
    loo_wt = weights(psis_obj, log=FALSE)
    impact = colSums(draws_of(pred_natl, with_chains=FALSE)[, 1] * loo_wt) - E(pred_natl)

    d_firms = tibble(firm_id = stan_d$firm_ids,
                     bias = E(draws_gcb$bias) + E(draws_gcb$r_firms))
    d_polls = filter(d_polls,
                     tte >= as.integer(elec_date - from_date),
                     tte <= as.integer(elec_date - start_date)) |>
        mutate(impact = impact) |>
        left_join(d_firms, by=c("firm_id")) |>
        arrange(tte) |>
        transmute(date = elec_date - tte,
                  firm = firm,
                  type = type,
                  lv = 1 - not_lv,
                  firm_bias = bias/4,
                  est = plogis(est),
                  impact = 100*impact)


    # Predict outcomes -----
    cli_h1("Forecasting seat outcomes")

    d_house_22 = read_csv(here("data-raw/produced/hist_house_races.csv.gz"), show_col_types=FALSE) |>
        filter(year == 2022) |>
        rows_update(tibble(state="AK", district=1, inc_seat="dem"), by=c("state", "district")) |>
        mutate(dem_cand_full = dem_cand,
               rep_cand_full = rep_cand,
               dem_cand = coalesce(str_c(state, ": ", dem_cand), "<other>"),
               rep_cand = coalesce(str_c(state, ": ", rep_cand), "<other>"),
               ldem_pres_adj = ldem_pres - ldem_pres_natl,
               inc_seat = coalesce(inc_seat, "open"),
               inc_seat = c(dem=1, open=0, gop=-1)[inc_seat],
               unopp = coalesce(unopp, 0))
    d_house_pred = d_house_22 |>
        mutate(inc_midterm = c(dem=-1, gop=1)[inc_pres]*(year %% 4 == 2),
               ldem_exp = log(dem_exp) - log(rep_exp),
               exp_mis = 1*(is.na(ldem_exp) | is.infinite(ldem_exp)),
               ldem_exp = if_else(exp_mis == 1, 0, ldem_exp),
               div_yr = str_c(division, "_", year)) |>
        filter(unopp == 0)
    d_house_unopp = d_house_22 |>
        filter(unopp == 1) |>
        # mutate(pr_dem = if_else(inc_seat == "dem", 1, 1*(ldem_pres > ldem_pres_natl)),
        mutate(pr_dem = if_else(inc_seat == 1, 1, 1*(ldem_pres > ldem_pres_natl)),
               part_base = plogis(ldem_pres_adj))

    # mix the national intent and outcome draws
    pr_mix_natl = seq(0.5/N_mix_natl, 1, 1/N_mix_natl)
    mix_natl = quantile(draws_of(pred_natl)[, 1], probs=pr_mix_natl, names=FALSE) # evenly space natl draws (reduce var)

    m_outcomes = read_rds(here("stan/outcomes_m.rds"))
    N_outcomes = sum(m_outcomes$fit@sim$n_save)
    N_per_chunk = N_outcomes %/% N_mix_natl
    iter_grp = lapply(seq_len(N_mix_natl), \(i) (i-1)*N_per_chunk + seq_len(N_per_chunk))

    pb = cli_progress_along(1:N_mix_natl, name="Combining draws")
    set.seed(5118)
    m_pred = do.call(rbind, map(pb, function(i) {
        d_tmp = d_house_pred |>
            mutate(ldem_gen = mix_natl[i],
                   ldem_pred = ldem_pres_adj + ldem_gen)
        posterior_predict(m_outcomes, newdata=d_tmp, draw_ids=iter_grp[[i]],
                          sample_new_levels="uncertainty", cores=1,
                          allow_new_levels=TRUE)
    }))
    colnames(m_pred) = with(d_house_pred, str_c(state, "-", district))
    cli_progress_done()

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
        mutate(inc_seat = c("gop", "open", "dem")[inc_seat + 2]) |>
        select(state, district, dem_cand=dem_cand_full, rep_cand=rep_cand_full,
               inc_seat, unopp, part_base, pr_dem, dem_mean:dem_q90) |>
        arrange(state, district)

    out = list(
        s_prob = mean(pred_seats >= 218),
        s_med = round(median(pred_seats)),
        s_q10 = round(quantile(pred_seats, 0.1)),
        s_q25 = round(quantile(pred_seats, 0.25)),
        s_q75 = round(quantile(pred_seats, 0.75)),
        s_q90 = round(quantile(pred_seats, 0.9)),
        i_prob = E(pred_natl > 0),
        i_med = plogis(median(pred_natl)),
        i_q10 = plogis(quantile(pred_natl, 0.1)),
        i_q25 = plogis(quantile(pred_natl, 0.25)),
        i_q75 = plogis(quantile(pred_natl, 0.75)),
        i_q90 = plogis(quantile(pred_natl, 0.9))
    )

    list(pred_seats = pred_seats,
         pred_natl = pred_natl,
         m_pred = m_pred,
         gcb = m_gcb$draws("natl_dem"),
         d_pred_22 = d_pred_22,
         n_polls = stan_d$N,
         d_polls = d_polls,
         out = out)
}

save_forecast <- function(forecast, elec_date, from_date) {
    tt_elec = round(elec_date - from_date)
    fmt_trunc = \(x) as.character(round(x, digits=5))

    # history
    if (file.exists(hist_path <- here("docs/history.csv"))) {
        d_hist = read_csv(hist_path, show_col_types=FALSE)
    } else {
        d_hist = tibble()
    }

    d_hist = bind_rows(
        d_hist,
        mutate(as_tibble(forecast$out),
               from_date = from_date,
               timestamp = Sys.time(),
               tte = as.integer(tt_elec),
               version = hash_version(),
               .before=everything())
    ) |>
        group_by(from_date) |>
        arrange(desc(timestamp)) |>
        slice_head(n=1) |>
        arrange(from_date) |>
        mutate(across(where(is.numeric), fmt_trunc))
    write_csv(d_hist, hist_path)

    # other outputs
    if (from_date == Sys.Date()) {
        write_json(forecast$out, here("docs/summary.json"), auto_unbox=TRUE, digits=5)

        forecast$d_pred_22 |>
            mutate(across(where(is.numeric), fmt_trunc)) |>
            write_csv(here("docs/districts.csv"))

        d_intent = summarize_natl(forecast$gcb, elec_date, c(0.5, 0.8)) |>
            pivot_wider(names_from=.width, values_from=c(.lower, .upper)) |>
            rename(q10 = .lower_0.8,
                   q25 = .lower_0.5,
                   q75 = .upper_0.5,
                   q90 = .upper_0.8) |>
            mutate(across(where(is.numeric), fmt_trunc))
        write_csv(d_intent, here("docs/natl_intent.csv"))

        N_sim = length(forecast$pred_seats)
        tibble(dem_seats = 1:400,
               pr = 100*tabulate(forecast$pred_seats, nbins=400) / N_sim) |>
            mutate(cdf = cumsum(pr/100),
                   across(where(is.numeric), fmt_trunc)) |>
            write_csv(here("docs/seats_hist.csv"))

        write_rds(forecast$m_pred, here("docs/draws_matrix.rds"), compress="gz")

        forecast$d_polls |>
            mutate(across(where(is.numeric), fmt_trunc)) |>
            slice_head(n=50) |>
            write_csv(here("docs/polls.csv"))
    }
}
