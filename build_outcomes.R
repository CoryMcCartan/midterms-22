library(tidyverse)
library(here)
library(brms)
library(patchwork)
library(wacolors)

# Load data --------

d <- read_csv(here("data-raw/produced/hist_house_races.csv.gz"), show_col_types=FALSE) |>
    mutate(dem_cand = str_c(state, ": ", dem_cand),
           dem_cand = coalesce(dem_cand, "<other>"),
           dem_cand = fct_lump_min(dem_cand, min=2),
           rep_cand = str_c(state, ": ", rep_cand),
           rep_cand = coalesce(rep_cand, "<other>"),
           rep_cand = fct_lump_min(rep_cand, min=2))

d_fit <- d |>
    drop_na(ldem_gen) |>
    mutate(ldem_pres_adj = ldem_pres - ldem_pres_natl,
           ldem_pred = ldem_pres_adj + ldem_gen,
           ldem_exp = log(dem_exp) - log(rep_exp),
           inc_seat = c(dem=1, open=0, gop=-1)[inc_seat],
           exp_mis = 1*(is.na(ldem_exp) | is.infinite(ldem_exp)),
           ldem_exp = if_else(exp_mis == 1, 0, ldem_exp),
           inc_midterm = c(dem=-1, gop=1)[inc_pres]*(year %% 4 == 2),
           div_yr = str_c(division, "_", year)) |>
    filter(unopp == 0, !is.infinite(ldem_seat), year >= 2010) |>
    mutate(dem_cand = fct_drop(dem_cand),
           rep_cand = fct_drop(rep_cand))

# BRMS ------

form = ldem_seat ~ inc_pres + offset(ldem_pred) + ldem_pres_adj:ldem_gen +
    polar*(inc_seat + ldem_exp + exp_mis) - polar + region +
    (1 + edu_o15 | year) + (1 | division:year) +
    (1 | dem_cand) + (1 | rep_cand)
bprior = c(
    prior(student_t(3, 0, 0.5), class=b),
    # about 0.5pp
    prior(gamma(4, 4/0.02), class=sd, group="division:year"),
    prior(gamma(4, 4/0.02), class=sd, group="year"),
    # about 1.0pp
    prior(gamma(4, 4/0.04), class=sd, coef="Intercept", group="year"),
    # about 2pp
    prior(gamma(4, 4/0.08), class=sd, group="dem_cand"),
    prior(gamma(4, 4/0.08), class=sd, group="rep_cand")
)

m = brm(bf(form, sigma ~ polar + I(ldem_pres_adj^2), decomp="QR"),
        data=d_fit, family=student(), prior=bprior,
        threads=2, chains=3, backend="cmdstanr", normalize=FALSE,
        iter=2000, warmup=500, control=list(adapt_delta=0.99, step_size=0.05),
        file=here("stan/outcomes_m.rds"), file_refit="on_change",
        stan_model_args=list(stanc_options=list("O1")))


summary(m)

mcmc_plot(m, variable="b_[^s]", regex=TRUE) +
    geom_vline(xintercept=0, lty="dashed") +
    theme_bw() +
    theme(axis.text.y=element_text(face="bold"))
ggsave(here("readme-doc/outcomes_model_ests.svg"), width=7, height=6)



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
