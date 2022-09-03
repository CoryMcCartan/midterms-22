library(tidyverse)
library(lubridate)
library(brms)
library(posterior)

raw = read_csv("https://github.com/fivethirtyeight/data/raw/master/pollster-ratings/raw-polls.csv",
               show_col_types=FALSE)

d = raw |>
    filter(!str_ends(type_simple, "-P")) |>
    transmute(race=race, firm=pollster, type=methodology,
              n = samplesize,
              tte = as.integer(mdy(electiondate) - mdy(polldate)),
              est = qlogis(cand1_pct / (cand1_pct + cand2_pct)),
              act = qlogis(cand1_actual / (cand1_actual + cand2_actual)),
              err = est - act)

form = bf(err ~ (1|firm),# + (1|race),
          sigma ~ log(n) + log(tte) + (1|firm))

# fit = brm(form, data=d, family=student(),
#           normalize=FALSE, backend="cmdstanr", threads=4,
#           chains=2, iter=1300, warmup=800)
fit = brm(form, data=d, family=student(),
          normalize=FALSE, backend="cmdstanr", algorithm="meanfield",
          threads=6, tol_rel_obj=0.001, iter=10e3, eta=0.5, adapt_engaged=FALSE)


d_pred = distinct(d, firm) %>%
    mutate(n = median(d$n),
           tte = median(d$tte))
err_pred = posterior_predict(fit, newdata=d_pred, re_formula= ~ (1|firm), resp="err") |>
    rvar()

d_firms = tibble(firm = d_pred$firm,
       mad = median(abs(err_pred)),
       rmse = sqrt(mean(err_pred^2)),
       bias = median(err_pred),
       stdev = sd(err_pred))
ratings = read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/pollster-ratings.csv",
               show_col_types=FALSE) |>
    janitor::clean_names() |>
    select(rank:simple_expected_error, herding_penalty, house_effect) |>
    inner_join(d_firms, by=c("pollster" = "firm"))



library(ggrepel)
library(geomtextpath)
library(wacolors)
library(scales)

d_contour = crossing(bias=seq(-0.12, 0.12, 0.005),
                     stdev=seq(0.065, 0.19, 0.005)) |>
    mutate(rmse = sqrt(bias^2 + stdev^2))

ratings %>%
    filter(polls_analyzed > 15) %>%
ggplot(aes(bias, stdev,
           label=if_else(polls_analyzed >= 250 | rank <= 20 |
                             rmse <= 0.12 | stdev <= 0.1,
                         abbreviate(pollster, 10), ""),
           size=polls_analyzed, color=rank)) +
    geom_textcontour(aes(bias, stdev, z=rmse, label=str_c("RMSE ", after_stat(level))),
                     data=d_contour, inherit.aes=F,
                     color="#999999", size=2.8, hjust=0.05) +
    geom_point() +
    scale_x_continuous("Predictive bias (logit)", labels=number, limits=range(d_contour$bias)) +
    scale_y_continuous("Predictive std. dev. (logit)", labels=number, limits=range(d_contour$stdev)) +
    coord_cartesian(expand=FALSE) +
    geom_text_repel(fontface="bold", size=3.2) +
    scale_size_continuous(range=c(0, 5), trans="sqrt", guide="none") +
    scale_color_wa_b("sea_star", breaks=c(1, 5, 10, 20, 50),
                               trans="log", reverse=TRUE, name="538 Rank") +
    theme_bw(base_family="Times", base_size=12) +
    theme(panel.grid.minor=element_blank())
ggsave("firms_bias_var.pdf", width=9, height=6)
