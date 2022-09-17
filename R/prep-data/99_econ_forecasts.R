
# SPF: <https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/survey-of-professional-forecasters>
download_spf_micro <- function() {
    url <- paste0("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/",
                  "survey-of-professional-forecasters/historical-data/spfmicrodata.xlsx")
    download.file(url, here::here("data-raw/SPFmicrodata.xlsx"))
}


# WSJ data from <https://www.wsj.com/articles/economic-forecasting-survey-archive-11617814998>

get_wsj_econ <- function(year=2022, month=7) {
    path <- download_wsj_econ(year, month)
    raw = readxl::read_excel(path, sheet=1)
}

download_wsj_econ <- function(year=2022, month=7) {
    cutoff_subd = 2021 + 5/12
    cutoff_xlsx = 2022 + 5/12
    date = year + month/12
    date_str = stringr::str_c(
        stringr::str_pad(month, 2, pad="0"),
        stringr::str_sub(as.character(year), 3)
    )

    if (date < cutoff_subd) {
        url <- stringr::str_glue("https://online.wsj.com/public/resources/documents/wsjecon{date_str}.xls")
    } else if (date < cutoff_xlsx) {
        url <- stringr::str_glue("https://s.wsj.net/public/resources/documents/wsjecon{date_str}.xls")
    } else {
        url <- stringr::str_glue("https://s.wsj.net/public/resources/documents/wsjecon{date_str}.xlsx")
    }


    path <- here::here("data-raw", basename(url))
    download.file(url, path)
    invisible(path)
}
