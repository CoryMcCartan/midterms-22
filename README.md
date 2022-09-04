
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Federal Election Predictions 2022

#### Cory McCartan

<img src="doc/cover.jpg" style="width: 100%;">

A dynamic Bayesian model to forecast the 2022 U.S. House elections.

## Directory structure

- Code for all the analyses in [`R/`](R/) and [`stan/`](stan/). README
  files in each subdirectory contain more information.
- Tracked, processed data are in [`data/`](data/); untracked and raw
  data are in [`data-raw/`](data-raw/).

## Model details

#### Model structure

``` mermaid
graph TD
    mod_firms[<font size=5>FIRMS]:::model --> |Prior on firm error| mod_natl[<font size=5>NATIONAL INTENT]:::model
    mod_fund[<font size=5>FUNDAMENTALS]:::model --> |Prior on E-day intent| mod_natl
    d_ret([Historical<br />House returns]):::data -.-> mod_race
    mod_natl --> |Covariate| mod_race[<font size=5>OUTCOMES]:::model
    d_gen([Historical generic<br />ballot polling]):::data -.-> mod_firms
    d_fund([Historical economic<br />and approval data]):::data -.-> mod_fund
    d_22([2022 partisanship<br />and incumbency]):::data -.-> mod_race
    classDef model fill:#aa2,stroke:#000,font-size:16pt,font-weight:bold
    classDef data fill:#efeff4,stroke#aaa,line-height:1.5,font-size:9pt
```

### Fundamentals Model

Bayesian linear regression of national two-way contested vote share for
the incumbent president’s party on logit retirements; midterm, house,
and presidential control indicators; GDP change over the past year, log
unemployment rate; and logit presidential approval. Coefficients are
given an [R2-D2 prior](https://arxiv.org/abs/2111.10718). Coefficient
estimates: <img src="doc/fund_model_est-1.svg" width="100%" />

Fundamentals-only prediction for 2022:
<img src="doc/fund_model_pred-1.svg" width="100%" />
