library(tidyverse)
library(lubridate)
library(here)
library(brms)

d_econ <- read_csv(here("data-raw/produced/fred_ind.csv"), show_col_types=FALSE)
pca = with(filter(d_econ, year < 2008), prcomp(cbind(gdp_chg, lunemp, cpi_chg)))
pca$rotation[, 1]

d <- read_csv(here("data/fundamentals.csv"), show_col_types=FALSE) |>
    mutate(econ = 0.678*gdp_chg - 0.038*lunemp - 0.734*cpi_chg) # 1948-2006 PCA weights

d_mid = d |>
    drop_na(linc_vote_imp) |>
    filter(nomidterm == 0)
d_pres = d |>
    drop_na(linc_vote_imp) |>
    filter(nomidterm == 1)

form <- linc_vote_imp ~ inc_house + polar*(dem_pres + lg_approval) - polar + econ + lg_retire
bprior <- prior(R2D2(0.6, cons_D2=2), class=b)

m_mid <- brm(form, data=d_mid, prior=bprior,
             backend="cmdstan", cores=4, iter=2000, warmup=1000,
             file=here("stan/fund_m"), file_refit="on_change",
             save_pars = save_pars(all=TRUE),
             control=list(adapt_delta=0.9995, refresh=1000))
m_pres <- brm(form, data=d_pres, prior=bprior,
              backend="cmdstan", cores=4, iter=2000, warmup=1000,
              save_pars = save_pars(all=TRUE),
              control=list(adapt_delta=0.9995, refresh=1000))


pred_fund_m <- function(pred_year = 2022) {
    d_pred = filter(d, year == pred_year)
    d_fit_pre <- d |>
        drop_na(linc_vote_imp) |>
        filter(year < pred_year, nomidterm == d_pred$nomidterm)

    if (d_pred$nomidterm == 0) {
        m_fit <- update(m_mid, newdata=d_fit_pre, cores=4, iter=1250, warmup=1000,
                        control=list(adapt_delta=0.9995, refresh=0))
    } else {
        m_fit <- update(m_pres, newdata=d_fit_pre, cores=4, iter=1250, warmup=1000,
                        control=list(adapt_delta=0.9995, refresh=0))
    }

    posterior_predict(m_fit, newdata=d_pred)[, 1]
}

d_forecast = map_dfr(seq(2010, 2022, by=2), function(yr) {
    pred = pred_fund_m(yr)
    write_rds(pred, here(str_glue("data/fund_pred/fundamentals_pred_{yr}.rds")), compress="gz")
    tibble(year=yr, est=mean(pred), se=sd(pred))
})

write_rds(m_mid, here("data-raw/produced/fundamentals_model_2022.rds"), compress="xz")


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

    mean(plogis(posterior_predict(m_mid, newdata=d_pred)[, 1]))
    mean(plogis(posterior_predict(m_pres, newdata=d_pred)[, 1]))

    sd(plogis(posterior_predict(m_mid, newdata=d_pred)[, 1]))
    sd(plogis(posterior_predict(m_pres, newdata=d_pred)[, 1]))

    # compare to past predictions
    plot(fitted(m_mid)[, 1], rep(0, 11), cex=0.0)
    abline(v=fitted(m_mid)[, 1], col='#666666')
    abline(v=mean(posterior_predict(m_mid, newdata=d_pred)[, 1]), col='red', lwd=2.0)

    # regression diagnostics
    res = resid(m_mid)[,1]
    qplot(res, fitted(m_mid)[,1])
    # q-q plot vs replicates
    wrap_plots(c(list(qplot(sample=res, geom="qq")),
                 map(2:16, ~ qplot(sample=rnorm(nrow(d_mid)), geom="qq"))
    ))
    p1 = pp_check(m_mid, "hist")
    p2 = pp_check(m_mid, "stat_2d", stat=c(median, \(x) quantile(x, 0.1))) +
        labs(x="median", y="10th %ile")
    yr = d_mid$year
    p3 = pp_check(m_mid, "intervals", size=1.5) + scale_x_continuous(breaks=seq_along(yr), labels=yr)
    p4 = pp_check(m_mid, "loo_pit_qq")
    p1 + p2 + p3 + p4

    plot_k_pres = loo(m_pres) |>
        pareto_k_values() |>
        qplot(d_pres$year, y=_) +
        labs(x="Year", y="Pareto k", title="Presidential")
    plot_k_mid = loo(m_mid) |>
        pareto_k_values() |>
        qplot(d_mid$year, y=_) +
        labs(x="Year", y="Pareto k", title="Midterms")
    plot_k_pres + plot_k_mid

}
