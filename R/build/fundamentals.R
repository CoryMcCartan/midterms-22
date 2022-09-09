library(tidyverse)
library(lubridate)
library(here)
library(brms)

d_econ <- read_csv(here("data-raw/produced/fred_ind.csv"), show_col_types=FALSE)
d_appr <- read_csv(here("data-raw/produced/oct_pres_approval.csv"), show_col_types=FALSE)
d_hist <- read_csv(here("data-raw/dfp/001_house_national_voteshare_input.csv"), show_col_types=FALSE) |>
    rename_with(~ str_replace(., "in_power", "inc"), starts_with("in_power")) |>
    rename(year=cycle) |>
    mutate(linc_vote_contest = qlogis(inc_contested_two_way),
           linc_vote = qlogis(inc_two_way),
           lg_retire = qlogis(inc_retire_pct))
d_control <- read_csv(here("data-raw/manual/party_control.csv"), show_col_types=FALSE) |>
    mutate(inc_house = if_else(dem_pres == 1, dem_house, 1 - dem_house),
           inc_senate = if_else(dem_pres == 1, dem_senate, 1 - dem_senate)) |>
    select(year=elect_year, inc_house:inc_senate) |>
    bind_rows(tibble(year=2022, inc_house=1, inc_senate=1))

d <- d_hist |>
    select(year, linc_vote_contest, linc_vote, lg_retire, inc_special_performance, dem_pres, midterm) |>
    left_join(d_control, by="year") |>
    left_join(d_econ, by="year") |>
    left_join(d_appr, by="year") |>
    mutate(midterm = 1 - midterm)

# save to public data
d |>
    select(year, linc_vote_contest, linc_vote, lg_retire, midterm,
           inc_house, dem_pres, gdp_chg, lunemp, lg_approval) |>
    filter(year == 2022 | !is.na(linc_vote)) |>
    mutate(across(where(is.numeric), round, 5)) |>
    write_csv(here("data/fundamentals.csv"))

form <- linc_vote ~ lg_retire + midterm*(inc_house + dem_pres) + gdp_chg + lunemp + lg_approval
bprior <- prior(R2D2(0.6), class=b)

m <- brm(form, data=drop_na(d, linc_vote_contest), prior=bprior,
         backend="cmdstan", cores=4, iter=2000, warmup=1000,
         file=here("stan/fund_m"), file_refit="on_change",
         save_pars = save_pars(all=TRUE),
         control=list(adapt_delta=0.9995, refresh=1000))


pred_fund_m <- function(pred_year = 2022) {
    d_fit <- drop_na(d, linc_vote_contest) |>
        filter(year < pred_year)

    m_fit <- update(m, newdata=d_fit, cores=4, iter=1250, warmup=1000,
                    control=list(adapt_delta=0.9995, refresh=0))

    d_pred = filter(d, year == pred_year)
    pred <- posterior_predict(m_fit, newdata=d_pred)[, 1]
}

walk(seq(2010, 2022, by=2), function(yr) {
    pred_fund_m(yr) |>
        write_rds(here(str_glue("data/fund_pred/fundamentals_pred_{yr}.rds")), compress="gz")
})

write_rds(m, here("data-raw/produced/fundamentals_model_2022.rds"), compress="xz")


# some diagnostics
if (FALSE) {
    library(patchwork)
    library(loo)

    # try some other specs
    form1 <- linc_vote ~ lg_retire + midterm*inc_house + midterm*dem_pres + gdp_chg + lunemp + lg_approval
    m1 <- brm(form1, data=drop_na(d, linc_vote_contest), prior=bprior,
              backend="cmdstan", cores=4, iter=2000, warmup=1000,
              control=list(adapt_delta=0.9995, refresh=1000))
    # change formula
    p1 <- mcmc_plot(m1) + labs(title="Full data + midterm/dem_pres interaction")
    # subset
    form2 <- linc_vote ~ lg_retire + inc_house + dem_pres + gdp_chg + lunemp + lg_approval
    m2 <- drop_na(d, linc_vote_contest) |>
        filter(midterm == 1) |>
        brm(form2, data=_, prior=bprior,
            backend="cmdstan", cores=4, iter=2000, warmup=1000,
            control=list(adapt_delta=0.9995, refresh=1000))
    p2 <- mcmc_plot(m2) + labs(title="Midterms only")
    p1 + p2

    form3 <- linc_vote ~ lg_retire + midterm*inc_house + dem_pres + gdp_chg + lunemp + lg_approval
    m3 <- brm(form3, data=drop_na(d, linc_vote_contest), prior=bprior,
              backend="cmdstan", cores=4, iter=2000, warmup=1000,
              control=list(adapt_delta=0.9995, refresh=1000))

    # regression diagnostics
    res = resid(m)[,1]
    qplot(res, fitted(m)[,1])
    # q-q plot vs replicates
    wrap_plots(c(list(qplot(sample=res, geom="qq")),
                 map(2:16, ~ qplot(sample=rnorm(nrow(d)), geom="qq"))
    ))
    p1 = pp_check(m, "hist")
    p2 = pp_check(m, "stat_2d", stat=c(median, \(x) quantile(x, 0.1))) +
        labs(x="median", y="10th %ile")
    yr = drop_na(d, linc_vote_contest)$year
    p3 = pp_check(m, "intervals", size=1.5) + scale_x_continuous(breaks=seq_along(yr), labels=yr)
    p4 = pp_check(m, "loo_pit_qq")
    plot_k = loo(m) |>
        pareto_k_values() |>
        qplot(yr, y=_) +
        labs(x="Year", y="Pareto k")
    plot_k2 = loo(m2) |>
        pareto_k_values() |>
        qplot(filter(drop_na(d, linc_vote_contest), midterm==1)$year, y=_)
        labs(x="Observation", y="Pareto k")

    y_fake = posterior_predict(m, ndraws=1)[1, ]
    m_fake = d |>
        drop_na(linc_vote_contest) |>
        mutate(linc_vote = y_fake) |>
        update(m, newdata=_, cores=4, iter=1200, warmup=1000,
               control=list(adapt_delta=0.9995, refresh=0))
    plot_k_fake = loo(m_fake) |>
        pareto_k_values() %>%
        qplot(seq_along(.), .) +
        labs(x="Observation", y="Pareto k")

    plot_k + plot_k_fake +
        plot_annotation(title="One of these is the real data, one is simulated from the model")

    m0 <- brm(form, data=drop_na(d, linc_vote_contest),
              backend="cmdstan", cores=4, iter=2000, warmup=1000,
              save_pars = save_pars(all=TRUE),
              control=list(adapt_delta=0.9995, refresh=1000))


    mcmc_plot(m, variable=c("b_gdp_chg", "b_lg_approval"), type="scatter")$data[, 1:2] |> cor()
    mcmc_plot(m1, variable=c("b_gdp_chg", "b_lg_approval"), type="scatter")$data[, 1:2] |> cor()

    # sensitivity / leave-one-out
    d_pred = filter(d, year == 2022)
    yr = drop_na(d, linc_vote_contest)$year
    loo = map_dbl(yr, function(loo_year) {
        d_fit <- drop_na(d, linc_vote_contest) |>
            filter(year != loo_year)

        m_fit <- update(m, newdata=d_fit, cores=4, iter=2000, warmup=1000,
                        control=list(adapt_delta=0.9995, refresh=0))

        mean(posterior_predict(m_fit, newdata=d_pred)[, 1])
    })
    names(loo) = yr
    qplot(yr, plogis(loo), xlab="Leave year out", ylab="2022 point prediction") +
        geom_hline(yintercept=0.5, lty="dashed") +
        scale_x_continuous(breaks=seq(1970, 2020, 4)) +
        scale_y_continuous(limits=c(0.47, 0.53), labels=scales::percent)

    tibble(
        pred_full = posterior_predict(m, newdata=d_pred, ndraws=4000)[, 1],
        pred_mid = posterior_predict(m3, newdata=d_pred, ndraws=4000)[, 1]
    ) |>
    ggplot() +
        geom_vline(xintercept=0.5, lty="dashed") +
        geom_density(aes(plogis(pred_full)), adjust=1.3, fill="black", alpha=0.2) +
        geom_density(aes(plogis(pred_mid)), adjust=1.3, fill="blue", alpha=0.2) +
        scale_x_continuous(labels=scales::percent)
}
