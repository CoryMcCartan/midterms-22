date_midpt <- function(d1, d2) {
    int <- lubridate::interval(d1, d2)
    start <- lubridate::int_start(int)
    lubridate::as_date(start + (lubridate::int_end(int) - start)/2)
}
