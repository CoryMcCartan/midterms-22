library(tidyverse)
library(lubridate)
library(here)
source(here("R/utils.R"))

# Load and combine data --------

raw_hist_all = read_csv("https://github.com/fivethirtyeight/data/raw/master/pollster-ratings/raw-polls.csv",
                        show_col_types=FALSE)
raw_generic = read_csv("https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls_historical.csv",
                        show_col_types=FALSE)
raw_pres20 = read_csv("https://projects.fivethirtyeight.com/polls/data/president_polls_historical.csv",
                      show_col_types=FALSE)
raw_pres16 = read_csv(here("data/president_general_polls_2016.csv"),
                      show_col_types=FALSE) |>
    suppressWarnings()

d_hist_all = raw_hist_all |>
    filter(location == "US") |>
    transmute(year = as.integer(str_sub(race, 1, 4)),
              race = c(H="house", P="pres")[str_sub(race, 6, 6)],
              firm = pollster, firm_id = pollster_rating_id,
              type = str_to_lower(methodology),
              n = samplesize,
              tte = as.integer(mdy(electiondate) - mdy(polldate)),
              est = qlogis(cand1_pct / (cand1_pct + cand2_pct)),
              act = qlogis(cand1_actual / (cand1_actual + cand2_actual)))

d_generic = raw_generic |>
    transmute(year = as.integer(cycle),
              race = "house",
              firm = pollster, firm_id = pollster_rating_id,
              type = str_to_lower(methodology),
              n = coalesce(sample_size, 600),
              tte = as.integer(mdy(election_date) -
                                   date_midpt(mdy(start_date), mdy(end_date))),
              est = qlogis(dem / (dem + rep)))

d_pres20 = raw_pres20 |>
    filter(is.na(state), party %in% c("DEM", "REP")) |>
    transmute(poll_id = poll_id,
              year = as.integer(cycle),
              race = "pres",
              firm = pollster, firm_id = pollster_rating_id,
              type = str_to_lower(methodology),
              pop = factor(population, levels=c("lv", "v", "rv", "a")),
              n = coalesce(sample_size, 600),
              tte = as.integer(mdy(election_date) -
                                   date_midpt(mdy(start_date), mdy(end_date))),
              party = party,
              pct = pct) |>
    group_by(poll_id) |>
    arrange(pop, .by_group=TRUE) |>
    slice_head(n=2) |>
    ungroup() |>
    pivot_wider(names_from=party, values_from=pct) |>
    mutate(est = qlogis(DEM / (DEM + REP))) |>
    select(-poll_id, -pop, -DEM, -REP)

d_pres16 = raw_pres16 |>
    filter(state == "U.S.") |>
    transmute(year = 2016L,
              race = "pres",
              firm = pollster,
              n = coalesce(samplesize, 100),
              tte = as.integer(make_date(2016, 11, 8) -
                                   date_midpt(mdy(startdate), mdy(enddate))),
              est = qlogis(rawpoll_clinton / (rawpoll_clinton + rawpoll_trump))) |>
    distinct()

guess_types = bind_rows(d_generic, d_pres20) |>
    count(firm, firm_id, type) |>
    group_by(firm, firm_id) |>
    arrange(desc(n)) |>
    slice_head(n=1) |>
    ungroup() |>
    select(-n)

# match 2016 firms to IDs and methodologies
max_dist = 20
m_dist = adist(d_pres16$firm, guess_types$firm)
m_dist[m_dist >= max_dist] = Inf
idx_match = apply(m_dist, 1, function(x) {
    out = which.min(x)
    if (is.infinite(x[out])) out = NA_integer_
    out
})
d_pres16$firm = guess_types$firm[idx_match]
d_pres16 = bind_cols(d_pres16, select(guess_types, firm_id, type)[idx_match, ])

act_results = distinct(d_hist_all, year, race, act)

d = bind_rows(d_generic, d_pres20, d_pres16) |>
    left_join(act_results, by=c("year", "race")) |>
    bind_rows(d_hist_all) |>
    filter(tte >= 0) |>
    mutate(firm_id = as.integer(firm_id),
           n = as.integer(n),
           type = case_when(type == "live phone" ~ "phone",
                            str_detect(type, "ivr") ~ "ivr",
                            type == "live phone/online" ~ "mixed",
                            str_detect(type, "online") ~ "online"),
           err = est - act) |>
    drop_na() |>
    group_by(firm_id) |>
    filter(n() >= 3) |>
    ungroup() |>
    mutate(firm_id = factor(firm_id),
           year = factor(year),
           type = factor(type))

if (FALSE) {
    ggplot(d, aes(sample=err, group=type)) +
        geom_qq(size=0.3) +
        stat_qq_line(color='red')

    filter(d, type=="online") |>
        filter(firm == "Ipsos" | firm == "HarrisX") |>
        mutate(x = qnorm(cume_dist(err))) |>
    ggplot(aes(x, err, shape=fct_lump(firm, 5), color=str_c(race, year))) +
        geom_point(size=2, position=position_jitter(0, 0.05))

    d |>
        group_by(firm, race) |>
        mutate(err = err - mean(err)) |>
        filter(tte <= 120) |>
    ggplot(aes(sample=err, group=type)) +
        geom_qq(size=0.3) +
        stat_qq_line(color='red')

    ggplot(d, aes(log10(n), err^2)) +
        geom_point() +
        scale_y_log10() +
        geom_smooth()

}

# Model firm error ---------
library(cmdstanr)
library(tidybayes)
library(posterior)

if (!file.exists(fit_path <- here("out/firms_fit.rds"))) {
    stan_data = d |>
        select(years=year, firms=firm_id, types=type, n, tte, Y=err) |>
        compose_data(X_sigma = cbind(log(n), sqrt(tte)),
                     K_sigma = ncol(X_sigma),
                     prior_only = 0L,
                     grainsize = 1L,
                     .n_name=n_prefix("N"))

    sm = cmdstan_model(here("stan/firms.stan"), cpp_options=list(stan_threads=TRUE))


    fit = sm$sample(stan_data, init=0, chains=2, threads_per_chain=4,
                    iter_warmup=500, iter_sampling=500,
                    adapt_delta=0.97, step_size=0.05)

    fit$save_object(fit_path, compress="xz")
} else {
    fit = read_rds(fit_path)
}

firm_list = distinct(d, firm, firm_id) |>
    distinct(firm_id, .keep_all=TRUE) |>
    arrange(firm_id)
firm_lookup = firm_list$firm
names(firm_lookup) = as.character(firm_list$firm_id)

draws = as_draws_rvars(fit)
draws[which(str_starts(names(draws), "z_"))] = NULL
names(draws$r_firms) = firm_lookup[levels(d$firm_id)]
names(draws$m_herding) = names(draws$r_firms)
names(draws$r_sigma_firms) = names(draws$r_firms)
names(draws$r_years) = levels(d$year)
names(draws$r_types) = levels(d$type)


pred_sigma = with(draws, exp(
    b_sigma_intercept + log(median(d$n)) * b_sigma[1] + r_sigma_firms
))
hyp_year_re = rvar_rng(rnorm, 1, 0, 0.1*draws$sd_years)
modal_type = count(d, firm_id, type) |>
    group_by(firm_id) |>
    arrange(firm_id, desc(n)) |>
    slice_head(n=1)
pred_mean = with(draws, bias + r_firms + m_herding * hyp_year_re + r_types[modal_type$type])
pred_err = rvar_rng(rnorm, stan_data$N_firms, pred_mean, pred_sigma)
names(pred_err) = names(draws$r_firms)


d_firms = tibble(firm = names(draws$r_firms),
                 bias = E(pred_err),
                 stdev = sd(pred_err),
                 sigma = median(pred_sigma),
                 rmse = sqrt(E(pred_err^2)),
                 herding = E(draws$m_herding)) |>
    arrange(rmse)

library(geomtextpath)
library(wacolors)
library(scales)

d_contour = crossing(bias=with(d_firms, seq(min(bias)*1.05, max(bias)*1.05, 0.005)),
                     stdev=with(d_firms, seq(min(stdev)*0.95, max(stdev)*1.05, 0.005))) |>
    mutate(rmse = sqrt(bias^2 + stdev^2))


ggplot(d_firms, aes(bias, stdev, label=abbreviate(firm, 8), color=herding)) +
    geom_vline(xintercept=0, lty="dashed", size=0.3) +
    geom_textcontour(aes(bias, stdev, z=rmse, label=str_c("RMSE ", after_stat(level))),
                     data=d_contour, inherit.aes=F,
                     color="#777777", size=2.5, linewidth=0.2, hjust=0.05) +
    geom_text(size=2.8, fontface="bold") +
    scale_y_log10() +
    scale_color_wa_c("puget", trans="log10", reverse=T) +
    coord_cartesian(expand=FALSE)
