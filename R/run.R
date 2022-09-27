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
  2022 U.S. House Forecast
  {as.character(Sys.Date(), format='%B %d, %Y')}
 -------------------------------------------
  Forecast from: {as.character(from_date, format='%B %d, %Y')}
  {tt_elec} day{if (tt_elec > 1) 's'} until the election.
  {forecast$n_polls} polls.

  Dem. share of two-party vote:  {sprintf('%.1f%%', 100*i_med)}%
  Median seat estimate:          {round(s_med)}
  80% CI for seats:              {round(s_q10)} - {round(s_q90)}
  Prob. of keeping control:      {round(100*s_prob)}%
  Prob. of winning pop. vote:    {round(100*i_prob)}%
 ===========================================
\n\n")))
system("osascript -e 'display notification \"Model run complete.\" with title \"House Model\"'")

if (isFALSE(opt$dry)) {
    save_forecast(forecast, elec_date, from_date)
    cli_alert_success("Forecast saved.")
}
