
# compare
ci_ours = tibble(model="ours",
                 low=quantile(pred_seats, 0.1),
                 med=quantile(pred_seats, 0.5),
                 high=quantile(pred_seats, 0.9))

d_econ = read_csv("https://cdn-dev.economistdatateam.com/us-midterms-2022/data/house/histogram.csv",
                  show_col_types=FALSE) |>
    rename(icdf = prob_at_least_this_many_dem_seats)
ci_econ = tibble(model="economist",
                 low=filter(d_econ, abs(icdf - 0.9) == min(abs(icdf - 0.9)))$sim_house_dem_seats,
                 med=filter(d_econ, abs(icdf - 0.5) == min(abs(icdf - 0.5)))$sim_house_dem_seats,
                 high=filter(d_econ, abs(icdf - 0.1) == min(abs(icdf - 0.1)))$sim_house_dem_seats
)

d_538 = read_csv(here("data-raw/538/election-forecasts-2022/house_national_toplines_2022.csv"),
                 show_col_types=FALSE) |>
    mutate(date = mdy(forecastdate),
           version=str_sub(expression, 2)) |>
    filter(date == max(date)) |>
    split(~ version)
ci_538_lite = tibble(model="538_lite",
                     low=d_538$lite$p10_seats_Dparty,
                     med=d_538$lite$median_seats_Dparty,
                     high=d_538$lite$p90_seats_Dparty)
ci_538_classic = tibble(model="538_classic",
                     low=d_538$classic$p10_seats_Dparty,
                     med=d_538$classic$median_seats_Dparty,
                     high=d_538$classic$p90_seats_Dparty)
ci_538_deluxe = tibble(model="538_deluxe",
                       low=d_538$deluxe$p10_seats_Dparty,
                       med=d_538$deluxe$median_seats_Dparty,
                       high=d_538$deluxe$p90_seats_Dparty)

bind_rows(ci_ours, ci_econ, ci_538_lite, ci_538_classic, ci_538_deluxe) |>
ggplot(aes(model, med, ymin=low, ymax=high)) +
    geom_errorbar() +
    geom_point(size=5)
