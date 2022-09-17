/*
 * NATIONAL INTENT MODEL
 */

data {
    int<lower=1> N; // total observations
    vector[N] Y; // response variable

    int<lower=1> N_days;
    array[N] int<lower=1, upper=N_days> day;

    real prior_eday_loc;
    real<lower=0> prior_eday_scale;
    real<lower=1> prior_eday_df;

    real<lower=1> nu_delta; // df of random walk
    real<lower=0> prior_sd_delta_shape;
    real<lower=0> prior_sd_delta_loc;

    // data and parameter estimates from firm error model -----
    // data
    int<lower=1> N_firms;
    int<lower=1> N_types;
    array[N] int<lower=1, upper=N_firms> firms;
    array[N] int<lower=1, upper=N_types> types;
    vector<lower=0, upper=1>[N] not_lv;

    int<lower=1> K_sigma; // number of population-level effects
    matrix[N, K_sigma] X_sigma; // population-level design matrix

    // priors
    vector[N_firms] prior_z_firms_loc;
    vector[N_firms] prior_z_sigma_firms_loc;
    vector[N_types] prior_z_types_loc;
    vector<lower=0>[N_firms] prior_z_firms_scale;
    vector<lower=0>[N_firms] prior_z_sigma_firms_scale;
    vector<lower=0>[N_types] prior_z_types_scale;

    real prior_bias_loc;
    real prior_b_intercept_sigma_loc;
    vector[K_sigma] prior_b_sigma_loc;
    real<lower=0> prior_bias_scale;
    real<lower=0> prior_b_intercept_sigma_scale;
    vector<lower=0>[K_sigma] prior_b_sigma_scale;

    // corresponding standard errors
    real<lower=0> sd_firms;
    real<lower=0> sd_sigma_firms;
    real<lower=0> sd_types;
    real<lower=0> sd_years;
    real<lower=0> sd_lv;
}


parameters {
    vector[N_days] delta; // steps of random walk
    real<lower=0> sd_delta; // scale of random walk

    // Parameters copied from firm error model -----
    vector[N_firms] z_firms;
    vector[N_firms] z_sigma_firms;
    vector[N_types] z_types;

    real bias; // global intercept
    real z_year;
    real lv_diff;
    real b_intercept_sigma;
    vector[K_sigma] b_sigma; // population-level effects
}

transformed parameters {
    vector[N_days] mu; // national voter intention, logit

    mu[1] = delta[1];
    mu[2:N_days] = mu[1] + 0.001 * sd_delta * cumulative_sum(delta[2:N_days]);

    real r_year = 0.1*sd_years * z_year;

    vector[N_firms] r_firms = prior_z_firms_loc + prior_z_firms_scale .* z_firms;
    vector[N_firms] r_sigma_firms = prior_z_sigma_firms_loc + prior_z_sigma_firms_scale .* z_sigma_firms;
    vector[N_types] r_types = prior_z_types_loc + prior_z_types_scale .* z_types;

    vector[N] poll_loc = mu[day] + bias + r_firms[firms] + r_year + r_types[types] + lv_diff * not_lv;
    vector[N] poll_scale = exp(
            b_intercept_sigma + X_sigma * b_sigma +
            r_sigma_firms[firms]
        );
}

model {
    // likelihood
    Y ~ normal(poll_loc, poll_scale);

    // random walk
    delta[1] ~ student_t(prior_eday_df, prior_eday_loc, prior_eday_scale);
    delta[2:N_days] ~ student_t(nu_delta, 0, 1);
    // sd_delta ~ student_t(3, 0, 1.0);
    sd_delta ~ gamma(prior_sd_delta_shape, prior_sd_delta_shape/prior_sd_delta_loc);

    // firm error model
    bias ~ normal(prior_bias_loc, prior_bias_scale);
    lv_diff ~ normal(0, 0.1*sd_lv);
    b_intercept_sigma ~ normal(prior_b_intercept_sigma_loc, prior_b_intercept_sigma_scale);
    b_sigma ~ normal(prior_b_sigma_loc, prior_b_sigma_scale);

    z_firms ~ std_normal();
    z_sigma_firms ~ std_normal();
    z_types ~ std_normal();
    z_year ~ std_normal();
}

generated quantities {
    vector[N] log_lik;
    vector[N_days] natl_dem = inv_logit(mu);

    for (i in 1:N) {
        log_lik[i] = normal_lpdf(Y[i] | poll_loc[i], poll_scale[i]);
    }
}

