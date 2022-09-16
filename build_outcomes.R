library(tidyverse)
library(here)
library(brms)
library(patchwork)
library(wacolors)
# library(lme4)

# Load data --------

d <- read_csv(here("data-raw/produced/hist_house_races.csv.gz"), show_col_types=FALSE)

d_fit <- d |>
    drop_na(ldem_gen) |>
    mutate(ldem_pres_adj = ldem_pres - ldem_pres_natl,
           ldem_pred = ldem_pres_adj + ldem_gen,
           midterm = 1*(year %% 4 == 2),
           div_yr = str_c(division, "_", year)) |>
    filter(unopp == 0, !is.infinite(ldem_seat), midterm == 1)
    # filter(unopp == 0, !is.infinite(ldem_seat))

ggplot(d_fit, aes(ldem_pred, ldem_seat, color=year)) +
    geom_point(size=0.4)

# BRMS ------

form = ldem_seat ~ (inc_seat + inc_pres + polar + ldem_pres_adj + ldem_gen)^3 +
    region * (polar + ldem_pres_adj + ldem_gen) +
    (0 + (age_u35 + edu_o15 + pov + suburban):ldem_pres_adj || year) +
    (1 | division:year)
bprior = prior(student_t(3, 0, 2), class=b)

m = brm(bf(form, sigma ~ polar*I(ldem_pres_adj^2), nu ~ polar, decomp="QR"),
        data=d_fit, family=student(), prior=bprior,
        threads=4, chains=2, backend="cmdstanr", normalize=FALSE,
        iter=1500, warmup=700, control=list(adapt_delta=0.99, step_size=0.05),
        file=here("stan/outcomes_m.rds"), file_refit="on_change",
        stan_model_args=list(stanc_options=list("O1")))

summary(m)

pp_check(m, "dens_overlay_grouped", group="year", ndraws=8, adjust=0.5, n_dens=256)
pp_check(m, "ecdf_overlay_grouped", group="year", ndraws=8, adjust=0.5, n_dens=256)
pp_check(m, "loo_pit_qq", ndraws=50, size=0.2)
pp_check(m, "stat_2d", stat=c(\(x) quantile(x, 0.75), \(x) quantile(x, 0.025)))
mcmc_plot(m, type="pairs",
          variable=c("b_polar", "b_ldem_pres_adj", "b_ldem_gen",
                     "b_polar:ldem_pres_adj", "b_polar:ldem_gen", "Intercept"),
          off_diag_args = list(size=0.5))
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
    marg_plot("ldem_gen:polar") +
    marg_plot("ldem_gen:inc_seat", "rainier", c(1, 3, 2)) +
    marg_plot("polar:inc_pres", "rainier", c(1, 3)) +
    marg_plot("polar:inc_seat", "rainier", c(1, 3, 2)) +
    marg_plot("polar:region", "palouse") +
    # marg_plot("ldem_gen:region") +
    plot_layout(guides="collect")

pairs(cbind(
    ranef(m)$year[, 1, ],
    ldem_gen=distinct(d_fit, year, ldem_gen)[[2]]
), cex=0.4)
ranef(m)$`division:year`

looo = loo(m)

res = resid(m)[,1]
fit = loo_predict(m, psis_object=looo$psis_object)
fit = fitted(m)[,1]
qplot(ldem_pres, res, color=as.factor(year), data=d_fit, size=I(0.3)) +
    geom_smooth(method=lm, se=FALSE) +
    scale_color_wa_d("sound_sunset") +
    coord_cartesian(ylim=c(-1, 1))

# BART --------

library(dbarts)

m_bart = rbart_vi(ldem_seat ~ inc_seat + inc_pres + ldem_pres_adj + polar + ldem_pred  +
                      age_u35 + white + black + edu_o15 + pov + suburban + rural + division,
                  group.by=div_yr, data=d_fit, n.thin=10L, n.samples=2000L, keepTrees=TRUE)

res_bart = resid(m_bart)
fit_bart = fitted(m_bart)

yardstick::rsq_vec(fit, d_fit$ldem_seat)
# yardstick::rsq_vec(fit1, d_fit$ldem_seat)
yardstick::rsq_vec(fit_bart, d_fit$ldem_seat)

plot(fit, d_fit$ldem_seat, cex=0.1); abline(a=0, b=1, col='red')
# plot(fit1, d_fit$ldem_seat, cex=0.1); abline(a=0, b=1, col='red')
plot(fit_bart, d_fit$ldem_seat, cex=0.1); abline(a=0, b=1, col='red')
plot(loo_predict(m), d_fit$ldem_seat, cex=0.1); abline(a=0, b=1, col='red')


# BART + ranef --------

library(stan4bart)

m_s4b = stan4bart(ldem_seat ~ polar*(ldem_pres_adj + ldem_gen) +
                      bart(inc_seat + inc_pres + polar + ldem_pres_adj +
                               ldem_pred + age_u35 + white + black +
                               edu_o15 + pov + suburban) + (1 | division:year),
                  data=d_fit)

fit_s4b = fitted(m_s4b)
res_s4b = d_fit$ldem_seat - fit_s4b

sd(res)
sd(res_bart)
sd(res_s4b)
yardstick::rsq_vec(fit_s4b, d_fit$ldem_seat)

plot(fit_s4b, d_fit$ldem_seat, cex=0.1)
qqnorm(res_s4b)

# LMER --------
library(lme4)

form0 = ldem_seat ~ (inc_seat + inc_pres + polar + ldem_pres_adj + ldem_gen)^3 +
    region * (polar + ldem_pres_adj + ldem_gen) +
    (0 + (edu_o15 + pov + suburban):ldem_pres_adj || year) + (1 | division:year)

m0 = lmer(form0, data=d_fit, control=lmerControl())
yardstick::rsq_vec(fitted(m0), d_fit$ldem_seat)
summary(m0)
plot(fitted(m0), d_fit$ldem_seat, cex=0.1)
qplot(fitted(m0), fit_bart, color = abs(res_s4b) - abs(resid(m0)), size=I(0.5)) +
    scale_color_wa_c("vantage", midpoint=0)


# VI
m0 = brm(form, data=d_fit, family=student(), prior=bprior,
         threads=4, chains=2, backend="cmdstanr", normalize=FALSE,
         algorithm="meanfield", iter=30e3, tol_rel_obj=0.001, eta=0.1, adapt_engaged=FALSE,
         stan_model_args=list(stanc_options=list("O1")))
