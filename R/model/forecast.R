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

DEM_VP = TRUE
D_SEN_SEATS_NOTUP = 36

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

    # Senate polls
    d_sen_polls <- get_senate_poll_avg(from_date=from_date, force=refresh_polls)
    cli_alert_success("Senate polls loaded.")

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

    d_house = prep_house_d(from_date)
    d_senate = prep_senate_d(from_date, d_sen_polls)

    # mix the national intent and outcome draws
    pr_mix_natl = seq(0.5/N_mix_natl, 1, 1/N_mix_natl)
    mix_natl = quantile(draws_of(pred_natl)[, 1], probs=pr_mix_natl, names=FALSE) # evenly space natl draws (reduce var)

    house_forecast = pred_house(d_house, mix_natl, N_mix_natl)
    senate_forecast = pred_senate(d_senate, mix_natl, N_mix_natl)

    list(
        pred_natl = pred_natl,
        gcb = m_gcb$draws("natl_dem"),
        mix_natl = mix_natl,
        n_polls = stan_d$N,
        d_polls = d_polls,
        house = house_forecast,
        senate = senate_forecast,
        out = c(list(
            i_prob = E(pred_natl > 0),
            i_med = plogis(median(pred_natl)),
            i_q10 = plogis(quantile(pred_natl, 0.1)),
            i_q25 = plogis(quantile(pred_natl, 0.25)),
            i_q75 = plogis(quantile(pred_natl, 0.75)),
            i_q90 = plogis(quantile(pred_natl, 0.9)),
            pr_DsDh = mean(house_forecast$pred_seats >= 218 & senate_forecast$pred_seats >= 50),
            pr_DsRh = mean(house_forecast$pred_seats < 218 &  senate_forecast$pred_seats >= 50),
            pr_RsDh = mean(house_forecast$pred_seats >= 218 & senate_forecast$pred_seats < 50),
            pr_RsRh = mean(house_forecast$pred_seats < 218 &  senate_forecast$pred_seats < 50)
        ), house_forecast$out, senate_forecast$out)
    )
}

prep_house_d <- function(from_date) {
    d_house_22 = read_csv(here("data-raw/produced/hist_house_races.csv.gz"), show_col_types=FALSE) |>
        filter(year == 2022) |>
        rows_update(tibble(state="AK", district=1,
                           inc_seat=ifelse(from_date > ymd("2022-08-16"), "dem", "open")),
                           by=c("state", "district")) |>
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
        mutate(pr_dem = if_else(inc_seat == 1, 1, 1*(ldem_pres > ldem_pres_natl)),
               part_base = plogis(ldem_pres_adj))

    list(all=d_house_22, pred=d_house_pred, unopp=d_house_unopp)
}

pred_house <- function(d_house, mix_natl, N_mix_natl) {
    m_outcomes = read_rds(here("stan/outcomes_house_m.rds"))
    N_outcomes = sum(m_outcomes$fit@sim$n_save)
    N_per_chunk = N_outcomes %/% N_mix_natl
    iter_grp = lapply(seq_len(N_mix_natl), \(i) (i-1)*N_per_chunk + seq_len(N_per_chunk))

    pb = cli_progress_along(1:N_mix_natl, name="Combining draws for House model")
    set.seed(5118)
    m_pred = do.call(rbind, map(pb, function(i) {
        d_tmp = d_house$pred |>
            mutate(ldem_gen = mix_natl[i],
                   ldem_pred = ldem_pres_adj + ldem_gen)
        posterior_predict(m_outcomes, newdata=d_tmp, draw_ids=iter_grp[[i]],
                          sample_new_levels="uncertainty", allow_new_levels=TRUE, cores=1)
    }))
    colnames(m_pred) = with(d_house$pred, str_c(state, "-", district))

    # adjust RCV
    inv_cdf_AK = with(read_rds(here("data/rcv/rcv_AK.rds")),
                      suppressWarnings(splinefun(pr_dem, dshare)))
    pseudo_adj = 0.5 - inv_cdf_AK(runif(nrow(m_pred)))
    m_pred[, "AK-1"] = qlogis(plogis(m_pred[, "AK-1"]) + pseudo_adj)


    cli_progress_done()
    cli_alert_success("House predictions complete.")

    pred_seats = sum(d_house$unopp$pr_dem) + rowSums(m_pred > 0)

    d_pred_22 = bind_rows(
        d_house$unopp,
        mutate(d_house$pred,
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
        house_prob = mean(pred_seats >= 218),
        house_med = round(median(pred_seats)),
        house_q10 = round(quantile(pred_seats, 0.1)),
        house_q25 = round(quantile(pred_seats, 0.25)),
        house_q75 = round(quantile(pred_seats, 0.75)),
        house_q90 = round(quantile(pred_seats, 0.9))
    )

    list(pred_seats = pred_seats,
         m_pred = m_pred,
         d_pred_22 = d_pred_22,
         out = out)
}

prep_senate_d <- function(from_date, d_sen_polls) {
    d_sen_22 <- read_csv(here("data-raw/produced/hist_sen_races.csv.gz"), show_col_types=FALSE) |>
        filter(year == 2022) |>
        select(-poll_avg, -miss_polls) |>
        left_join(d_sen_polls, by=c("state", "cand_dem", "cand_rep")) |>
        mutate(dem_cand_full = cand_dem,
               rep_cand_full = cand_rep,
               cand_dem = coalesce(cand_dem, "<other>"),
               cand_rep = coalesce(cand_rep, "<other>"),
               ldem_pres_adj = ldem_pres - ldem_pres_natl,
               miss_polls = 1*is.na(poll_avg),
               poll_avg = coalesce(poll_avg, ldem_pres_adj),
               inc = coalesce(inc, "open"),
               inc = c(dem=1, open=0, oth=0, rep=-1)[inc],
               midterm = year %% 4 == 2,
               unopp = coalesce(unopp, 0))
    d_sen_pred = d_sen_22 |>
        filter(unopp == 0)
    d_sen_unopp = d_sen_22 |>
        filter(unopp == 1) |>
        mutate(pr_dem = if_else(inc == 1, 1, 1*(ldem_pres > ldem_pres_natl)),
               part_base = plogis(ldem_pres_adj))

    list(all=d_sen_22, pred=d_sen_pred, unopp=d_sen_unopp)
}

pred_senate <- function(d_senate, mix_natl, N_mix_natl) {
    m_outcomes = read_rds(here("stan/outcomes_sen_m.rds"))
    N_outcomes = sum(m_outcomes$fit@sim$n_save)
    N_per_chunk = N_outcomes %/% N_mix_natl
    iter_grp = lapply(seq_len(N_mix_natl), \(i) (i-1)*N_per_chunk + seq_len(N_per_chunk))

    pb = cli_progress_along(1:N_mix_natl, name="Combining draws for Senate model")
    set.seed(5118)
    m_pred = do.call(rbind, map(pb, function(i) {
        d_tmp = d_senate$pred |>
            mutate(ldem_gen = mix_natl[i],
                   ldem_pred = ldem_pres_adj + ldem_gen)
        posterior_predict(m_outcomes, newdata=d_tmp, draw_ids=iter_grp[[i]],
                          sample_new_levels="uncertainty", allow_new_levels=TRUE, cores=1)
    }))
    colnames(m_pred) = d_senate$pred$state
    cli_progress_done()
    cli_alert_success("Senate predictions complete.")

    pred_seats = D_SEN_SEATS_NOTUP + sum(d_senate$unopp$pr_dem) + rowSums(m_pred > 0)

    d_pred_22 = bind_rows(
        d_senate$unopp,
        mutate(d_senate$pred,
               part_base = plogis(ldem_pres_adj),
               pr_dem = colMeans(m_pred > 0),
               dem_mean = colMeans(plogis(m_pred)),
               dem_q10 = plogis(apply(m_pred, 2, quantile, probs=0.1)),
               dem_q25 = plogis(apply(m_pred, 2, quantile, probs=0.25)),
               dem_q75 = plogis(apply(m_pred, 2, quantile, probs=0.75)),
               dem_q90 = plogis(apply(m_pred, 2, quantile, probs=0.9)))
    ) |>
        mutate(inc_seat = c("gop", "open", "dem")[inc + 2]) |>
        select(state, dem_cand=dem_cand_full, rep_cand=rep_cand_full,
               inc_seat, unopp, part_base, pr_dem, dem_mean:dem_q90) |>
        arrange(state)

    out = list(
        sen_prob = mean(pred_seats >= 51 - DEM_VP),
        sen_med = round(median(pred_seats), 1),
        sen_q10 = round(quantile(pred_seats, 0.1), 1),
        sen_q25 = round(quantile(pred_seats, 0.25), 1),
        sen_q75 = round(quantile(pred_seats, 0.75), 1),
        sen_q90 = round(quantile(pred_seats, 0.9), 1)
    )

    list(pred_seats = pred_seats,
         m_pred = m_pred,
         d_pred_22 = d_pred_22,
         out = out)
}

save_forecast <- function(forecast, elec_date, from_date, detail=FALSE) {
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

        forecast$house$d_pred_22 |>
            mutate(across(where(is.numeric), fmt_trunc)) |>
            write_csv(here("docs/house_districts.csv"), na="")
        forecast$senate$d_pred_22 |>
            mutate(across(where(is.numeric), fmt_trunc)) |>
            write_csv(here("docs/senate_races.csv"), na="")

        d_intent = summarize_natl(forecast$gcb, elec_date, c(0.5, 0.8)) |>
            pivot_wider(names_from=.width, values_from=c(.lower, .upper)) |>
            rename(q10 = .lower_0.8,
                   q25 = .lower_0.5,
                   q75 = .upper_0.5,
                   q90 = .upper_0.8) |>
            mutate(across(where(is.numeric), fmt_trunc))
        write_csv(d_intent, here("docs/natl_intent.csv"))

        N_sim = length(forecast$house$pred_seats)
        tibble(dem_seats = 1:400,
               pr = 100*tabulate(forecast$house$pred_seats, nbins=400) / N_sim) |>
            mutate(cdf = cumsum(pr/100),
                   across(where(is.numeric), fmt_trunc)) |>
            write_csv(here("docs/seats_hist_house.csv"))
        tibble(dem_seats = 1:65,
               pr = 100*tabulate(forecast$senate$pred_seats, nbins=65) / N_sim) |>
            mutate(cdf = cumsum(pr/100),
                   across(where(is.numeric), fmt_trunc)) |>
            write_csv(here("docs/seats_hist_senate.csv"))

        forecast$d_polls |>
            mutate(across(where(is.numeric), fmt_trunc)) |>
            slice_head(n=50) |>
            write_csv(here("docs/polls.csv"))

        N_rep = length(forecast$house$pred_seats) / length(forecast$mix_natl)
        tibble(natl = rep(plogis(forecast$mix_natl), each=N_rep),
                         seats = forecast$house$pred_seats) |>
            mutate(natl = round(natl*200)/200) |>
            group_by(natl) |>
            summarize(pr_win = mean(seats >= 218),
                      q01 = quantile(seats, 0.01),
                      q05 = quantile(seats, 0.05),
                      q10 = quantile(seats, 0.10),
                      q25 = quantile(seats, 0.25),
                      q50 = quantile(seats, 0.50),
                      q75 = quantile(seats, 0.75),
                      q90 = quantile(seats, 0.9),
                      q95 = quantile(seats, 0.95),
                      q99 = quantile(seats, 0.99)) |>
            write_csv(here("docs/house_shifts.csv"))

        if (isTRUE(detail)) {
            # save matrix as image for JS
            colnames(forecast$senate$m_pred)[26] = "OK-S"
            m_out = cbind(pred_natl = rep(5*plogis(forecast$mix_natl) - 2, each=N_rep), # scale to 0-1
                          seats_house = round(256*(forecast$house$pred_seats/200 - 0.5))/256, # scale to 0-1
                          seats_sen = forecast$senate$pred_seats/25 - 1.5, # scale to 0-1
                          plogis(forecast$house$m_pred),
                          plogis(forecast$senate$m_pred))
            png::writePNG(m_out, here("docs/draws.png"))
            write_json(colnames(m_out), here("docs/draws_cols.json"))
        }
    }
}
