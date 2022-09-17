library(tidyverse)
library(lubridate)
library(here)
library(brms)

d_econ <- read_csv(here("data-raw/produced/fred_ind.csv"), show_col_types=FALSE)

pca = with(filter(d_econ, year < 2008), prcomp(cbind(gdp_chg, lunemp, cpi_chg)))
pca$rotation[, 1]

d <- read_csv(here("data/fundamentals.csv"), show_col_types=FALSE) |>
    left_join(select(d_econ, year, cpi_chg), by="year") |>
    mutate(econ = 0.678*gdp_chg - 0.038*lunemp - 0.734*cpi_chg) # pre-08 PCA weights

d_fit = d |>
    drop_na(linc_vote_imp)

form <- linc_vote_imp ~ nomidterm*dem_pres + polar*(inc_house + dem_pres + lg_approval) + econ + lg_retire
bprior <- prior(R2D2(0.7, cons_D2=2), class=b)
# bprior <- prior(normal(0, 0.2), class=b)

m <- brm(form, data=d_fit, prior=bprior,
         backend="cmdstan", cores=4, iter=2000, warmup=1000,
         file=here("stan/fund_m"), file_refit="on_change",
         save_pars = save_pars(all=TRUE),
         control=list(adapt_delta=0.9995, refresh=1000))


pred_fund_m <- function(pred_year = 2022) {
    d_fit_pre <- d_fit |>
        filter(year < pred_year, nomidterm == 0)
        # filter(year < pred_year)

    m_fit <- update(m_mid, newdata=d_fit_pre, cores=4, iter=1250, warmup=1000,
                    control=list(adapt_delta=0.9995, refresh=0))

    d_pred = filter(d, year == pred_year)
    posterior_predict(m_fit, newdata=d_pred)[, 1]
}

d_forecast = map_dfr(seq(2010, 2022, by=2), function(yr) {
    pred = pred_fund_m(yr)
    write_rds(pred, here(str_glue("data/fund_pred/fundamentals_pred_{yr}.rds")), compress="gz")
    tibble(year=yr, est=mean(pred), se=sd(pred))
})

write_rds(m, here("data-raw/produced/fundamentals_model_2022.rds"), compress="xz")


# some diagnostics
if (FALSE) {
    library(patchwork)
    library(loo)

    left_join(d_forecast, select(d, year, linc_vote_imp), by="year") |>
        drop_na() |>
    ggplot(aes(linc_vote_imp, est, label=year)) +
        geom_abline(slope=1, color="red") +
        geom_errorbar(aes(ymin=est-2*se, ymax=est+2*se)) +
        geom_text() +
        labs(x="Actual", y="Estimated", title="Full model")

    d_pred = filter(d, year == 2022)
    form2 <- linc_vote_imp ~ lg_retire + inc_house + dem_pres*polar + econ + lg_approval*polar - polar
    bprior_mid <- prior(R2D2(0.6, cons_D2=2), class=b)

    m_pres <- drop_na(d, linc_vote_imp) |>
        filter(nomidterm == 1) |>
        brm(form2, data=_, prior=bprior_mid,
            backend="cmdstan", cores=4, iter=2000, warmup=1000,
            control=list(adapt_delta=0.9995, refresh=1000))

    # midterm only
    m_mid <- drop_na(d, linc_vote_imp) |>
        filter(nomidterm == 0) |>
        brm(form2, data=_, prior=bprior_mid,
            backend="cmdstan", cores=4, iter=2000, warmup=1000,
            control=list(adapt_delta=0.9995, refresh=1000))

    p1 <- mcmc_plot(m) + labs(title="All years")
    p2 <- mcmc_plot(m_mid) + labs(title="Midterms only")
    p3 <- mcmc_plot(m_pres) + labs(title="Presidential years only")
    p1 + p2 + p3

    mean(plogis(posterior_predict(m, newdata=d_pred)[, 1]))
    mean(plogis(posterior_predict(m_mid, newdata=d_pred)[, 1]))
    mean(plogis(posterior_predict(m_pres, newdata=d_pred)[, 1]))

    sd(plogis(posterior_predict(m, newdata=d_pred)[, 1]))
    sd(plogis(posterior_predict(m_mid, newdata=d_pred)[, 1]))
    sd(plogis(posterior_predict(m_pres, newdata=d_pred)[, 1]))


    # compare to past predictions
    plot(fitted(m)[d_fit$nomidterm == 0, 1], rep(0, 11), cex=0.0)
    abline(v=fitted(m)[d_fit$nomidterm == 0, 1], col='#666666')
    abline(v=mean(posterior_predict(m, newdata=d_pred)[, 1]), col='red', lwd=2.0)

    plot(fitted(m_mid)[, 1], rep(0, 11), cex=0.0)
    abline(v=fitted(m_mid)[, 1], col='#666666')
    abline(v=mean(posterior_predict(m_mid, newdata=d_pred)[, 1]), col='red', lwd=2.0)


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
    yr = drop_na(d, linc_vote_imp)$year
    p3 = pp_check(m, "intervals", size=1.5) + scale_x_continuous(breaks=seq_along(yr), labels=yr)
    p4 = pp_check(m, "loo_pit_qq")
    plot_k = loo(m) |>
        pareto_k_values() |>
        qplot(yr, y=_) +
        labs(x="Year", y="Pareto k")
    plot_k2 = loo(m_mid) |>
        pareto_k_values() |>
        qplot(filter(drop_na(d, linc_vote_imp), midterm==1)$year, y=_)
        labs(x="Observation", y="Pareto k")

    # simpler
    form0 <- linc_vote_imp ~ nomidterm + inc_house + dem_pres*polar + econ + lg_approval + lg_retire
    bprior2 <- prior(normal(0, 1), class=b)

    m0 <- brm(form, data=d_fit, prior=bprior,
              backend="cmdstan", cores=4, iter=2000, warmup=1000,
              save_pars = save_pars(all=TRUE),
              control=list(adapt_delta=0.9995, refresh=1000))


    mcmc_plot(m, variable=c("b_gdp_chg", "b_lg_approval"), type="scatter")$data[, 1:2] |> cor()
    mcmc_plot(m0, variable=c("b_gdp_chg", "b_lg_approval"), type="scatter")$data[, 1:2] |> cor()

    loo_compare(loo(m, reloo=TRUE), loo(m0, reloo=TRUE))

    # sensitivity / leave-one-out
    d_pred = filter(d, year == 2022)
    yr = drop_na(d, linc_vote_imp)$year
    loo = map_dbl(yr, function(loo_year) {
        d_fit <- drop_na(d, linc_vote_imp) |>
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
        pred_mid = posterior_predict(m_mid, newdata=d_pred, ndraws=4000)[, 1]
    ) |>
    ggplot() +
        geom_vline(xintercept=0.5, lty="dashed") +
        geom_density(aes(plogis(pred_full)), adjust=1.0, fill="black", alpha=0.2) +
        geom_density(aes(plogis(pred_mid)), adjust=1.0, fill="blue", alpha=0.2) +
        # geom_vline(aes(xintercept=plogis(linc_vote_imp*(2*dem_pres - 1)),
        #                color=as.factor(nomidterm)),
        #            alpha=0.5, data=d) +
        scale_color_manual(values=c("blue", "black"),
                           labels=c("Midterm", "Presidential"), name=NULL) +
        scale_x_continuous(labels=scales::percent) +
        coord_cartesian(xlim=c(0.4, 0.6))


    loo(log_lik(m)[, m$data$midterm==0], r_eff=looo$diagnostics$n_eff[m$data$midterm==0]/4000)$estimates
    loo(log_lik(m)[, m$data$midterm==0], r_eff=looo$diagnostics$n_eff[m$data$midterm==0]/4000)$estimates
    tibble(year = seq(2018, 1978, -4),
           lpd_full = colMeans(log_lik(m)[, m$data$midterm==0]),
           lpd_midonly = colMeans(log_lik(m_mid))) |>
        write.csv(row.names=F,quote=F)

    library(tidybayes)
    gather_draws(m, `b_.+`, regex=T) |>
    ggplot(aes(0.04 / .value, .variable)) +
        geom_vline(xintercept=0) +
        geom_vline(xintercept=1, lty='dashed') +
        geom_vline(xintercept=-1, lty='dashed') +
        stat_pointintervalh(color="#113377", .width=c(0.5, 0.8))
}
