library(tidyverse)
library(here)
library(brms)
library(patchwork)
library(wacolors)

# Load data --------

d <- read_csv(here("data-raw/produced/hist_sen_races.csv.gz"), show_col_types=FALSE) |>
    mutate(cand_dem = coalesce(cand_dem, "<other>"),
           cand_dem = fct_lump_min(cand_dem, min=2),
           cand_rep = coalesce(cand_rep, "<other>"),
           cand_rep = fct_lump_min(cand_rep, min=2))

d_fit <- d |>
    drop_na(ldem_gen, ldem_seat) |>
    mutate(ldem_pres_adj = ldem_pres - ldem_pres_natl,
           ldem_pred = ldem_pres_adj + ldem_gen,
           inc = c(dem=1, open=0, oth=0, rep=-1)[inc],
           inc_midterm = c(dem=-1, gop=1)[inc_pres]*(year %% 4 == 2)) |>
    filter(unopp == 0, multi == 0, !is.infinite(ldem_seat)) |>
    mutate(cand_dem = fct_drop(cand_dem),
           cand_rep = fct_drop(cand_rep))


# Fit model ------

form = ldem_seat ~ inc_pres + ldem_pres_adj + ldem_gen +
    ldem_pres_adj:ldem_gen + polar*inc - polar +
    (1 + edu_o15 | year) + (1 | cand_dem) + (1 | cand_rep)
bprior = c(
    prior(student_t(3, 0, 0.5), class=b),
    # about 0.5pp
    prior(gamma(4, 4/0.02), class=sd, group="year"),
    # about 1.0pp
    prior(gamma(4, 4/0.04), class=sd, coef="Intercept", group="year"),
    # about 3pp
    prior(gamma(4, 4/0.12), class=sd, group="cand_dem"),
    prior(gamma(4, 4/0.12), class=sd, group="cand_rep")
)

m = brm(bf(form, sigma ~ polar + I(ldem_pres_adj^2), decomp="QR"),
        data=d_fit, family=student(), prior=bprior,
        threads=2, chains=3, backend="cmdstanr", normalize=FALSE,
        iter=1000, warmup=500, control=list(adapt_delta=0.99, step_size=0.1),
        file=here("stan/outcomes_sen_m.rds"), file_refit="on_change",
        stan_model_args=list(stanc_options=list("O1")))

summary(m)

mcmc_plot(m, variable="b_[^s]", regex=TRUE) +
    geom_vline(xintercept=0, lty="dashed") +
    theme_bw() +
    theme(axis.text.y=element_text(face="bold"))
ggsave(here("readme-doc/outcomes_model_sen_ests.svg"), width=7, height=6)


## CHECKS ------

pp_check(m, "dens_overlay_grouped", group="year", ndraws=8, adjust=0.5, n_dens=256)
pp_check(m, "ecdf_overlay_grouped", group="year", ndraws=8, adjust=0.5, n_dens=256)
pp_check(m, "loo_pit_qq", ndraws=50, size=0.2)
pp_check(m, "stat_2d", stat=c(\(x) quantile(x, 0.75), \(x) quantile(x, 0.025)))
# pp_check(m, "loo_intervals", ndraws=200)
# pp_check(m, "error_scatter_avg_grouped", group="year", size=0.2)
marg_plot = function(eff, pal="puget", which=1:15) {
    me = conditional_effects(m, effects=eff, prob=0, resolution=8)
    plot(me, plot=FALSE)[[1]] +
        scale_color_wa_d(pal, which=which) +
        scale_fill_wa_d(pal, which=which) +
        labs(subtitle=str_c("By ", str_split(eff, ":", simplify=TRUE)[2])) +
        theme_minimal() +
        theme(axis.title=element_text(face="bold"),
              legend.title=element_text(face="bold"))
}
marg_plot("ldem_pres_adj:polar") +
    marg_plot("ldem_gen:ldem_pres_adj") +
    marg_plot("ldem_gen:inc_seat", "rainier", c(1, 3, 2)) +
    # marg_plot("polar:inc_pres", "rainier", c(1, 3)) +
    marg_plot("polar:inc_seat", "rainier", c(1, 3, 2)) +
    marg_plot("polar:region", "palouse") +
    plot_layout(guides="collect")

pairs(cbind(
    ranef(m)$year[, 1, ],
    ldem_gen=distinct(d_fit, year, ldem_gen)[[2]]
), cex=0.4)
ranef(m)$`division:year`

res = resid(m)[,1]
fit = fitted(m)[,1]
qplot(ldem_pres, res, color=as.factor(year), data=d_fit, size=I(0.3)) +
    geom_smooth(method=lm, se=FALSE) +
    scale_color_wa_d("sound_sunset") +
    coord_cartesian(ylim=c(-1, 1))


ggplot(d_fit, aes(ldem_pred, ldem_seat, color=year)) +
    geom_point(size=0.4)

ggplot(d_fit, aes(year, ldem_seat - ldem_pred, group=year)) +
    geom_boxplot()

filter(d, year==2022) |>
    summarize(low = sum(ldem_pres - ldem_pres_natl + qlogis(0.46) > 0),
              mid = sum(ldem_pres - ldem_pres_natl + qlogis(0.49)  > 0),
              high = sum(ldem_pres - ldem_pres_natl + qlogis(0.52)  > 0))
