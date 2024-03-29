
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Federal Election Predictions 2022

#### Cory McCartan

<img src="readme-doc/cover.jpg" alt="U.S. Capitol at sunset" style="width: 100%;">

A dynamic Bayesian model to forecast the 2022 U.S. midterm elections.

## Directory structure

- Code for all the analyses in [`R/`](R/) and [`stan/`](stan/). README
  files in each subdirectory contain more information.
- Tracked, processed data are in [`data/`](data/); untracked and raw
  data are in [`data-raw/`](data-raw/).
- Model outputs are in [`docs`/](docs/); files for this documentation
  page are in [`readme-doc/`](readme-doc/).

## Model structure and details

**Jump to: [Fundamentals](#fundamentals-model) •
[Firms](#firm-error-model) • [National intent](#national-intent-model) •
[Outcomes](#outcomes-model)**

``` mermaid
graph TD
    mod_firms[<font size=5>FIRMS]:::model
    mod_firms --> |Prior on firm error| mod_natl[<font size=5>NATIONAL INTENT]:::model
    mod_fund[<font size=5>FUNDAMENTALS]:::model --> |Prior on E-day intent| mod_natl
    d_ret([Historical<br />House returns]):::data -.-> mod_race
    mod_natl --> |Covariate| mod_race[<font size=5>OUTCOMES]:::model
    d_gen([Historical generic<br />ballot polling]):::data -.-> mod_firms
    d_fund([Historical economic<br />and approval data]):::data -.-> mod_fund
    d_22([2022 partisanship<br />and incumbency]):::data -.-> mod_race
    classDef model fill:#aa2,stroke:#000,font-size:16pt,font-weight:bold
    classDef data fill:#efeff4,stroke#aaa,line-height:1.5,font-size:9pt
```

### Fundamentals model

The fundamentals model is Bayesian linear regression of national two-way
vote share for the incumbent president’s party on logit retirements;
house control (1 if incumbent president’s party controls the House), and
presidential control (1 for a Dem. president); an economic indicator;
logit presidential approval; and several interactions with polarization,
measured as the correlation between House and presidential results in
the previous election. The model is fit separately to presidential and
midterm years. The economic indicator is the first principal component
of three economic indicators: GDP change over the past year, log
unemployment rate; and urban CPI change over the past year (inflation).
The principal components are calculated only on data from 1948–2006 to
allow the weights to be used in predictive models after 2008. To build
national two-way vote share, we impute vote share for uncontested House
races using a BART model fit on contested House elections from 1976 to
2020. Coefficients are given an [R2-D2
prior](https://arxiv.org/abs/2111.10718). The data are available
[here](data/fundamentals.csv).

**Parameter estimates:**
<img src="readme-doc/fund_model_est-1.svg" width="100%" />

**Fundamentals-only prediction for 2022:**
<img src="readme-doc/fund_model_pred-1.svg" width="100%" />

### Firm error model

The firm error model goes hand-in-hand with the firm error component of
the national intent model, below. The idea is to use historical firm
performance in polling the generic ballot and presidential races as a
prior for firm performance this cycle. We can decompose firm error into
several components:

- Constant year-to-year bias in all firms in polling these races.
- Year-specific bias shared by all firms.
- Firm-specific bias.
- Bias from polling methodology (IVR/online/phone/mixed/unknown).
- Bias from LV polls. Due to limited data we only code an indicator for
  if a poll is not an LV poll—we don’t distinguish between RV/A/V polls.

Given total firm bias from all these sources, firms also vary in how
close their results cluster around this bias. If a firm consistently
reports numbers 5pp too favorable for Democrats, we can adjust for that.
Less consistency means less adjustment is possible. Polling variance is
affected by several factors:

- Sample size
- Time to the election
- LV vs. other polls
- Firm variance

We operationalize this framework with the following model, which is fit
to around 5,100 historical polling results.

$$
\begin{align*}
y_i &\sim \mathcal{N}(\mu_i, \sigma_i^2) \\
\mu_i &= \beta_\mu + \alpha_{f[i]}^{(f)} + \alpha_{c[i]}^{(c)} +
        + \alpha_{u[i]}^{(u)} + \alpha_{c[i]}^{(v)}v[i] \\
\sigma_i &= \exp(\beta_\sigma + x_i^\top\gamma_\sigma + \phi_{f[i]}^{(f)}) \\
\alpha^{(f)} &\stackrel{iid}{\sim} \mathcal{N}(0, \tau^2_f), \quad
\alpha^{(u)} \stackrel{iid}{\sim} \mathcal{N}(0, \tau^2_u), \quad
\alpha^{(v)} \stackrel{iid}{\sim} \mathcal{N}(0, \tau^2_v)\\
\alpha_{c}^{(c)}& \sim \mathrm{AR1}(\rho), \quad
\phi^{(f)} \stackrel{iid}{\sim} \mathcal{N}(0, \tau^2_\phi)
\end{align*}
$$

where $i$ indexes the polls, $f[i]$ is the firm, $c[i]$ is the
year/cycle, $u[i]$ is the methodology, $v[i]$ is the survey population
indicator (1 if not LV), $m_{f[i]}$ is the herding variable for each
firm, and $x_i$ is a vector of poll variance predictors: $\log(N_i)$,
$\sqrt{\text{time to elec.}}$, and the not-LV indicator. Further
details, including the weakly informative priors on all the parameters,
may be found in the [Stan model code](stan/firms.stan) and [fitting
code](R/build/firms.R).

We can simulate from the model to get *predictive* values of firm bias
and variance in hypothetical election-day likely voter polls for the
2022 election. These predictive values are the best way to evaluate each
firm’s overall quality for this election. A firm is better—that is, its
polls contain more information about the race—if it has lower variance
(std. dev.), a lower herding value, and bias closer to 0 (though this
will be adjusted for).

**Summary of firm performance:**
<img src="readme-doc/firm_perf-1.svg" width="100%" />

### National intent model

The intent model estimates latent national vote intent, which is assumed
to evolve as a random walk, with and observation model that is closely
related to the firm error model, above.

$$
\begin{align*}
y_i &\sim \mathcal{N}(\mu_i, \sigma_i^2) \\
\mu_i &= x_{t[i]} + \beta_\mu + \alpha_{f[i]}^{(f)} + \alpha^{(c)}
        + \alpha_{u[i]}^{(u)} + \alpha^{(v)}v[i] \\
\sigma_i &= \exp(\beta_\sigma + x_i^\top\gamma_\sigma + \phi_{f[i]}^{(f)}) \\
x_t &= x_{t-1} + \delta_t,\quad
\delta_t \stackrel{iid}{\sim} \mathrm{t_5}(0, \sigma^2_\delta) \\
\alpha^{(f)} &\stackrel{iid}{\sim} \mathcal{N}(0, \tau^2_f), \quad
\alpha^{(c)} \sim \mathcal{N}(\rho\alpha_{c_{old}}^{(c)}, \tau^2_c), \quad
\alpha^{(u)} \stackrel{iid}{\sim} \mathcal{N}(0, \tau^2_u), \quad
\alpha^{(v)} \sim \mathcal{N}(0, \tau^2_v)\\
\phi^{(f)} &\stackrel{iid}{\sim} \mathcal{N}(0, \tau^2_\phi),
\end{align*}
$$

where $i$ indexes the polls and $t$ indexes the days before the
election, $y$ is the poll outcome, $x$ is the latent intent, $f[i]$ is
the firm, $u[i]$ is the methodology, $v[i]$ is the survey population
indicator (1 if not LV), and $x_i$ is a vector of poll variance
predictors: $\log(N_i)$, $\sqrt{\text{time to elec.}}$, and the not-LV
indicator. Priors for most variables are taken from the posterior of the
firm error model (above), with some adjustments as noted below. The
prior on $x$ for election day is taken from the posterior predictive
distribution of the fundamentals model, shown above in the histogram. A
relatively strong prior is needed on $\sigma^2_\delta$ to regularize the
effect of firms who release panel survey results daily. We also cap the
number of polls from any one firm at 100 to further avoid biasing
effects from imbalance (which is observed in historical back-testing).
Firms with more than 100 polls have a subset of 100 selected at random
for inference. Since the random effects $\alpha^{(c)}$ and
$\alpha^{(v)}$ are unknown for this particular cycle, they are sampled
from their predictive distributions. Further details, including the
weakly informative priors on all the parameters, may be found in the
[Stan model code](stan/intent.stan), [fitting code](R/model/intent.R),
and [diagnostic code](R/build/build_intent.R).

Estimates for the 2010–2020 cycles, based only on previous years, are
shown below.

![2010 intent estimates](readme-doc/intent_backtest_2010.svg)

![2012 intent estimates](readme-doc/intent_backtest_2012.svg)

![2014 intent estimates](readme-doc/intent_backtest_2014.svg)

![2016 intent estimates](readme-doc/intent_backtest_2016.svg)

![2018 intent estimates](readme-doc/intent_backtest_2018.svg)

![2020 intent estimates](readme-doc/intent_backtest_2020.svg)

### Outcomes models

The outcomes models maps district partisanship, the national
environment, and other district and national factors onto vote shares in
each House district and Senate race. We use a multilevel model with a
student-t response, as described by the following (`brms`) model syntax.

**House:**

``` r
ldem_seat ~ ldem_seat ~ inc_pres + offset(ldem_pred) + ldem_pres_adj:ldem_gen +
    polar*(inc_seat + ldem_exp + exp_mis) - polar + region +
    (1 + edu_o15 | year) + (1 | division:year) + (1 | dem_cand) + (1 | rep_cand)
    
sigma ~ polar + I(ldem_pres_adj^2)
```

**Senate:**

``` r
ldem_seat ~ ldem_pres_adj * ldem_gen +
    (midterm + inc_pres + inc_seat)^2 + miss_polls*inc_seatc +
    (1 + white + edu_o15 + poll_avg | year) + (1 | region) +
    (1 | cand_dem) + (1 | cand_rep)
    
sigma ~ polar + I(ldem_pres_adj^2)
```

Here, `inc_pres` is the party of the incumbent president, coded as plus
or minus 1; `inc_seat` is the party of the seat’s incumbent, coded as 1
for a Democrat, -1 for a Republican and 0 if open; `ldem_pres_adj` is
the logit last presidential result in the district, shifted back to a
neutral national environment (i.e., subtracting off the national
presidential result); `ldem_gen` is the logit generic ballot;
`ldem_pred` is the sum of these two; `polar` measures polarization as
the lagged correlation between House and presidential results;
`ldem_exp` is the logit share of campaign expenditures by the Democrat;
`exp_mis` codes whether expenditure data are missing for the race (as
they unfortunately often are); `poll_avg` is the average of polls
conducted in the last 30 days, shrunk to `ldem_pres_adj` based on the
number of polls; `miss_polls` is an indicator for no polling being
available; and `dem_cand` and `rep_cand` are the Democratic and
Republican candidates, respectively.

For the House model, the standard deviation of the year random effects
is estimated around 0.05 (on the logit scale); the standard deviation of
the division-year random effects is estimated around 0.04. The model is
fit to all 2,320 contested House elections from 2010 to 2020. Posterior
summaries for all coefficients are shown below. The overall model $R^2$
is around 0.97 for the House model.

![House outcome model summary](readme-doc/outcomes_model_house_ests.svg)

For the Senate model, the standard deviation of the year random effects
is estimated around 0.03 (on the logit scale). The model is fit to all
258 contested, two-way Senate elections from 2006 to 2020. Posterior
summaries for all coefficients are shown below. The overall model $R^2$
is around 0.89.

![Senate outcome model summary](readme-doc/outcomes_model_sen_ests.svg)

#### Alaska RCV adjustment

There are two Republican candidates running against a single Democrat in
the Alaska at-large district. This poses a challenge to predicting a
winner, since the dynamics of rank-choice voting could be determinative.
As an ad-hoc adjustment, we simulate hypothetical Alaska at-large
elections based on the results of the 2022 special election, with
randomness added. We use the results of this simulation to understand
the probability of a Democratic win based on the first-round balloting
results. We can then translate this into a (random) vote boost to apply
to the Democratic candidate in the first round in order to produce a
rough approximation of the final rank-choice reallocated vote.
