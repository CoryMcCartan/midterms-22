/*
 * POLLING FIRM ERROR MODEL
 */

functions {
    // for use in transformed data
    array[] int sequence(int start, int end) {
        array[end - start + 1] int seq;
        for (n in 1 : num_elements(seq)) {
            seq[n] = n + start - 1;
        }
        return seq;
    }

    // log-lik for outcome
    real partial_log_lik_lpmf(array[] int seq, int start, int end,
                              data vector Y, real bias, data matrix Xc_sigma,
                              real intercept_sigma, vector b_sigma,
                              data array[] int firms, vector r_firms,
                              vector r_sigma_firms,
                              data array[] int years,  vector r_years,
                              data array[] int types, vector r_types,
                              data vector not_lv, vector lv_diff) {
        int N = end - start + 1;

        vector[N] mu = bias + r_firms[firms[start:end]] +
            r_years[years[start:end]] +
            r_types[types[start:end]] + lv_diff[years[start:end]] .* not_lv[start:end];
        vector[N] sigma = exp(
            intercept_sigma + Xc_sigma[start:end] * b_sigma +
            r_sigma_firms[firms[start:end]]
            );

        return normal_lupdf(Y[start:end] | mu, sigma);
    }
}

data {
    int<lower=1> N; // total observations
    vector[N] Y; // response variable

    // random effects
    int<lower=1> N_firms;
    int<lower=1> N_years;
    int<lower=1> N_types;
    array[N] int<lower=1, upper=N_firms> firms;
    array[N] int<lower=1, upper=N_years> years;
    array[N] int<lower=1, upper=N_types> types;
    vector<lower=0, upper=1>[N] not_lv;

    int<lower=1> K_sigma; // number of population-level effects
    matrix[N, K_sigma] X_sigma; // population-level design matrix

    int prior_only; // ignore likelihood?
    int grainsize; // grainsize for threading
}

transformed data {
    array[N] int seq = sequence(1, N);
    matrix[N, K_sigma] Xc_sigma; // centered version of X_sigma without an intercept
    vector[K_sigma] means_X_sigma; // column means of X_sigma before centering
    for (i in 1:K_sigma) {
        means_X_sigma[i] = mean(X_sigma[:, i]);
        Xc_sigma[:, i] = X_sigma[:, i] - means_X_sigma[i];
    }
}

parameters {
    vector[N_firms] z_firms;
    vector[N_firms] z_sigma_firms;
    vector[N_years] z_years;
    vector[N_types] z_types;
    vector[N_years] z_lv;
    // corresponding standard errors
    real<lower=0> sd_firms;
    real<lower=0> sd_sigma_firms;
    real<lower=0> sd_years;
    real<lower=0> sd_types;
    real<lower=0> sd_lv;

    real bias; // global intercept
    real<lower=0, upper=1> rho; // AR1 strength
    real intercept_sigma;
    vector[K_sigma] b_sigma; // population-level effects
}

transformed parameters {
    vector[N_firms] r_firms = 0.1*sd_firms * z_firms;
    vector[N_firms] r_sigma_firms = 0.1*sd_sigma_firms * z_sigma_firms;
    vector[N_types] r_types = 0.1*sd_types * z_types;
    vector[N_years] r_years;
    vector[N_years] lv_diff = 0.1*sd_lv * z_lv;

    r_years[1] = 0.1 * sd_years * z_years[1];
    for (i in 2:N_years) {
        r_years[i] = rho*r_years[i-1] + 0.1 * sd_years * z_years[i];
    }
}

model {
    if (!prior_only) {
        target += reduce_sum(partial_log_lik_lpmf, seq, grainsize,
                             Y, bias, Xc_sigma, intercept_sigma, b_sigma,
                             firms, r_firms, r_sigma_firms,
                             years, r_years,
                             types, r_types, not_lv, lv_diff);
    }

    bias ~ student_t(3, 0, 0.05);
    intercept_sigma ~ student_t(3, 0, 1);

    sd_firms ~ student_t(3, 0, 1.0);
    sd_sigma_firms ~ student_t(3, 0, 1.0);
    sd_years ~ student_t(3, 0, 1.0);
    sd_types ~ student_t(3, 0, 1.0);
    sd_lv ~ student_t(3, 0, 1.0);

    z_lv ~ std_normal();
    z_firms ~ std_normal();
    z_sigma_firms ~ std_normal();
    z_years ~ std_normal();
    z_types ~ std_normal();
}

generated quantities {
  // actual population-level intercept
  real b_sigma_intercept = intercept_sigma - dot_product(means_X_sigma, b_sigma);
}
