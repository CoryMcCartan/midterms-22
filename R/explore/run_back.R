library(here)

start_date = as.Date("2022-03-01")
elec_date = as.Date("2022-11-08")

source(here("R/model/forecast.R"))

dates = as.Date("2022-05-01") + seq(112, 152, 3) - 1

options(readr.num_threads=1)
purrr::walk(dates, safely(function(from_date) {
    forecast = run_forecast(elec_date, start_date, from_date,
                            refresh_polls=FALSE,
                            chains=4, iter=400, N_mix_natl=10)

    save_forecast(forecast, elec_date, from_date)
    cli_alert_success("Forecast saved for {from_date}.")
}))
