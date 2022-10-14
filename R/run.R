#!/usr/bin/env Rscript

#####################################
#     U.S. HOUSE MODEL 2022         #
#     CORY McCARTAN                 #
#     (c) 2022                      #
#####################################

start_date = as.Date("2022-03-01")
elec_date = as.Date("2022-11-08")

# Parse options -----
suppressMessages({
    library(optparse)
    library(here)
})

option_list = list(
    make_option("--dry", action="store_true", default=F,
                help="Dry run, results not saved."),
    make_option("--date", type="character",
                default=as.character(min(max(Sys.Date(), start_date), elec_date)),
                help=paste0("The date to estimate from, between
                    Between ", start_date, " and ", elec_date, " (election day).")),
    make_option("--iter", type="integer", default=800,
                help="Number of MCMC iterations for voter intent estimation per chain,
                    not including warmup iterations."),
    make_option("--n_mix", type="integer", default=40,
                help="Number of national intent draws to mix into the outcome model."),
    make_option("--chains", type="integer", default=4,
                help="Number of MCMC chains for voter intent estimation."),
    make_option("--refresh_polls", action="store_true", default=F,
                help="Force redownloading of polls")
)
opt = parse_args(OptionParser(option_list=option_list,
                              description="Forecast the 2022 U.S. House election."))

source(here("R/model/forecast.R"))

from_date = as_date(opt$date)

# Run --------
forecast = run_forecast(elec_date, start_date, from_date,
                        refresh_polls=opt$refresh_polls,
                        chains=opt$chains, iter=opt$iter,
                        N_mix_natl=opt$n_mix)

cli_alert_success("Forecast complete.")

# Output ------

tt_elec = round(elec_date - from_date)
with(forecast$out, cat(str_glue("\n
 ===========================================
  2022 U.S. Midterms Forecast
  {as.character(Sys.Date(), format='%B %d, %Y')}
 -------------------------------------------
  Forecast from: {as.character(from_date, format='%B %d, %Y')}
  {tt_elec} day{if (tt_elec > 1) 's'} until the election.
  {forecast$n_polls} polls.

  Dem. share of two-party vote:      {sprintf('%.1f%%', 100*i_med)}%
  Median House seat estimate:        {round(house_med)}
    80% CI:                          {round(house_q10)} - {round(house_q90)}
  Median Senate seat estimate:       {round(sen_med)}
    80% CI:                          {round(sen_q10)} - {round(sen_q90)}
  Prob. of keeping House control:    {round(100*house_prob)}%
  Prob. of keeping Senate control:   {round(100*sen_prob)}%
  Prob. of winning pop. vote:        {round(100*i_prob)}%
 ===========================================
\n\n")))
system("osascript -e 'display notification \"Model run complete.\" with title \"Midterms Forecast\"'")

if (isFALSE(opt$dry)) {
    save_forecast(forecast, elec_date, from_date)
    cli_alert_success("Forecast saved.")
}

d_shift = tibble(natl = rep(plogis(forecast$mix_natl),
                  each=length(forecast$house$pred_seats)/opt$n_mix),
       seats = forecast$house$pred_seats) |>
    mutate(natl = round(natl*200)/200) |>
    group_by(natl) |>
    mutate(pr_win = mean(seats >= 218)) |>
    ungroup()
ggplot(d_shift, aes(natl, seats, group=natl, fill=pr_win)) +
    geom_hline(yintercept=217.5, lty="dashed") +
    geom_vline(xintercept=0.5, lty="dashed") +
    geom_boxplot() +
    ggredist::scale_fill_party_c(name="Win prob.") +
    scale_x_continuous(labels=scales::percent) +
    theme_bw()

d_22_small = forecast$house$d_pred_22 |>
    mutate(district=str_c(state, "-", district)) |>
    select(district, dem_mean)
closest = head(order(abs(colMeans(forecast$house$m_pred > 0) - 0.5)), 30)

apply(forecast$house$m_pred[, closest], 2, function(x) {
    tapply(x > 0, d_shift$natl, mean)
}) |>
    as.data.frame() |>
    rownames_to_column("natl") |>
    as_tibble() |>
    mutate(natl = as.numeric(natl)) |>
    pivot_longer(-natl, names_to="district", values_to="pr_dem") |>
    left_join(d_22_small, by="district") |>
ggplot(aes(natl, reorder(district, dem_mean), fill=pr_dem)) +
    geom_tile() +
    geom_vline(xintercept=0.5, lty="dashed") +
    coord_cartesian(expand=F) +
    ggredist::scale_fill_party_c(name="Win prob.") +
    labs(y=NULL) +
    scale_x_continuous(labels=scales::percent) +
    theme_bw() +
    theme(axis.text.y=element_text(face="bold"))

tibble(house_seats = forecast$house$pred_seats,
       sen_seats = forecast$senate$pred_seats) |>
    mutate(win_house = house_seats >= 218) |>
    group_by(win_house, sen_seats) |>
    summarize(win_house = win_house[1],
              n = n() * (2*win_house - 1)) |>
ggplot(aes(n, sen_seats, fill=factor((sen_seats >= 50) + win_house))) +
    geom_col(orientation="y") +
    geom_vline(xintercept=0, size=2) +
    scale_fill_manual(values=c(.GOP, "#A054A0", .DEM), guide='none') +
    theme_bw()
