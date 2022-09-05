library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(posterior)
library(here)

# Load data ----
# also adjust types, and limit to firms with 10+ polls
d <- read_csv(here("data-raw/produced/hist_polls_house_pres.csv"),
              show_col_types=FALSE) |>
    mutate(type = case_when(year <= 2008 ~ "phone",
                            year <= 2004 & is.na(type) ~ "phone",
                            TRUE ~ type),
           type = coalesce(type, "unknown"),
           firm_id = fct_lump_min(as.factor(firm_id), min=10, other_level="other")) |>
    group_by(year) |>
    slice_sample(prop=1, replace=FALSE) |>
    slice_head(n=1000) |> # limit to max 1000 per year to avoid 2016-2020 being too heavy
    ungroup()

# Fit the model ----
fit_firm_model <- function(pred_year=2022, refit=FALSE, save=FALSE,
                           iter=25e3, eta=0.25, draws=4000) {
    fit_path <- here(str_glue("data-raw/produced/firms_model_{pred_year}.rds"))
    d_fit <- d |>
        filter(year < pred_year) |>
        mutate(years = factor(year),
               types = factor(type),
               not_lv = 1 - is_lv) |>
        select(years, firms=firm_id, types, not_lv, n, tte, Y=err)

    if (!file.exists(fit_path) || isTRUE(refit)) {
        stan_data = compose_data(d_fit,
                                 X_sigma = cbind(log(n), sqrt(tte), not_lv),
                                 K_sigma = ncol(X_sigma),
                                 prior_only = 0L,
                                 grainsize = 1L,
                                 .n_name=n_prefix("N"))

        sm = cmdstan_model(here("stan/firms.stan"),
                           cpp_options=list(stan_threads=TRUE),
                           stanc_options=list("O1"),
                           quiet=FALSE)

        # find MLE to use as init to VB
        fit_opt = sm$optimize(stan_data, init=0, threads=4, iter=5e3, refresh=500)

        fit = sm$variational(stan_data,
                             init=list(lapply(as_draws_rvars(fit_opt), E)), # init at MLE
                             seed=5118, threads=4,
                             eta=eta, adapt_engaged=FALSE, tol_rel_obj=0.0005,
                             elbo_samples=50, grad_samples=4, iter=iter,
                             refresh=500, eval_elbo=500,
                             algorithm="meanfield", output_samples=draws)

        # evaluate quality of mean-field variational approximation
        lw = fit$lp() - fit$lp_approx()
        lw = lw - max(lw)
        if (FALSE) {
            plot(fit$lp(), fit$lp_approx())
            hist(lw, breaks=200)
        }
        try({
            cat("Pareto k: ", suppressWarnings(loo::pareto_k_values(loo::psis(lw, r_eff=1))))
        })
        # 1/sum((exp(lw) / sum(exp(lw)))^2) # n_eff

        # Full HMC ~ 15min; some divergences (not good)
        if (FALSE) {
            fit = sm$sample(stan_data, init=0, chains=2, threads_per_chain=4,
                            iter_warmup=500, iter_sampling=500,
                            adapt_delta=0.98, step_size=0.05)
        }

        if (isTRUE(save)) {
            fit$save_object(fit_path, compress="xz")
        }
    } else {
        fit = read_rds(fit_path)
    }

    firm_list = count(d, firm, firm_id) |>
        arrange(desc(n)) |>
        distinct(firm_id, .keep_all=TRUE) |>
        arrange(firm_id)
    firm_lookup = firm_list$firm
    names(firm_lookup) = as.character(firm_list$firm_id)
    firm_lookup["other"] = "<other>"

    draws = as_draws_rvars(fit)
    draws[which(str_starts(names(draws), "z_"))] = NULL
    names(draws$r_firms) = firm_lookup[levels(d$firm_id)]
    names(draws$m_herding) = names(draws$r_firms)
    names(draws$r_sigma_firms) = names(draws$r_firms)
    names(draws$r_years) = levels(d_fit$years)
    names(draws$lv_diff) = levels(d_fit$years)
    names(draws$r_types) = levels(d_fit$types)

    list(draws = draws,
         firms = firm_lookup,
         loc = lapply(draws, mean),
         scale = lapply(draws, sd))
}

# Fit the model for the past few elections -----

fit_2018 = fit_firm_model(2018, eta=0.5)
fit_2020 = fit_firm_model(2020, eta=0.4)
fit_2022 = fit_firm_model(2022, draws=10e3)

save_fit <- function(fit) {
    name = deparse(substitute(fit))
    fit$draws = NULL
    path = here(str_c("data/firms_", name, ".rds"))
    write_rds(fit, path, compress="xz")
    invisible(path)
}

save_fit(fit_2018)
save_fit(fit_2020)
save_fit(fit_2022)


# Predictive scores for pollster ratings (not used as part of the model) -----
draws = fit_2022$draws
pred_sigma = with(draws, exp(
    b_sigma_intercept + log(median(d$n)) * b_sigma[1] +
        b_sigma[3] + r_sigma_firms
))
hyp_year_re = rvar_rng(rnorm, 1, 0, 0.1*draws$sd_years)
hyp_lv_re = rvar_rng(rnorm, 1, 0, 0.1*draws$sd_lv)
modal_type = count(d, firm_id, type) |>
    group_by(firm_id) |>
    arrange(firm_id, desc(n)) |>
    slice_head(n=1)
modal_lv = count(d, firm_id, not_lv=1-is_lv) |>
    group_by(firm_id) |>
    arrange(firm_id, desc(n)) |>
    slice_head(n=1)
pred_mean = with(draws, bias + r_firms + m_herding * hyp_year_re +
                     r_types[modal_type$type] + modal_lv$not_lv*hyp_lv_re)
pred_err = rvar_rng(rnorm, length(fit_2022$draws$r_firms), pred_mean, pred_sigma)
names(pred_err) = names(draws$r_firms)
poll_counts <- count(d, firm=firm_id) |>
    mutate(firm = fit_2022$firms[firm])

# summarize (and convert to rough pct. scale rather than logit)
d_firms = tibble(firm = names(draws$r_firms),
                 bias = E(pred_err) / 4,
                 stdev = sd(pred_err) / 4,
                 sigma = median(pred_sigma) / 4,
                 rmse = sqrt(E(pred_err^2 / 16)),
                 herding = E(draws$m_herding)) |>
    left_join(poll_counts, by="firm") |>
    arrange(rmse)

write_csv(d_firms, here("data/firms_pred_eval.csv"))
