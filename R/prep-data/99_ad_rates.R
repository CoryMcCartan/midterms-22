library(tidyverse)
library(here)
library(rvest)
library(curl)
library(collapse)
library(brms)
library(pdftools)

d_dma = read_rds(here("data-raw/produced/media_markets.rds")) |>
    drop_na(station) |>
    distinct(market, state, households, station)

load_pdf = function(pdf) {
    pages = bind_rows(pdf_data(pdf), .id="page") |>
        suppressMessages() |>
        mutate(page = as.integer(page),
               text = str_squish(text))
    if (nrow(pages) == 0) stop("no text")
    page_height = 10 + max(pages$y)
    mutate(pages, y = y + page_height * (page - 1))
}

extract_station_rate = function(url) {
    pages = load_pdf(curl_fetch_memory(url)$content)

    find_in_pdf = function(rg) {
        which(str_detect(pages$text, rg))
    }
    find_nearest = function(row_ids, rg) {
        ok = which(str_detect(pages$text, rg))
        m = qM(ss(pages, ok, c("x", "y")))
        m_row = qM(ss(pages, row_ids, c("x", "y")))
        idxs = apply(m_row, 1, function(r) which.min(rowSums((m %r-% r)^2)))
        ok[idxs]
    }
    get_text = function(row_ids) pages$text[row_ids]

    med_rate = find_in_pdf("Rate") |>
        find_nearest("\\$\\d") |>
        get_text() |>
        parse_number() |>
        median(na.rm=TRUE)

    gross_loc = find_in_pdf("(Gross|Grand)")
    n_spots = find_nearest(gross_loc, "Spots") |>
        find_nearest("[1-9][0-9]*") |>
        get_text() |>
        as.integer()
    gross_amt = find_nearest(gross_loc, "\\$\\d") |>
        get_text() |>
        parse_number()
    avg_rate = mean(gross_amt / n_spots, na.rm=TRUE)

    coalesce(avg_rate, med_rate)
}

regex_folder = "Political Files/2022/(Federal|State|Local)"
get_station_rate = function(station, i="") {
    cat(i, station, "\n")
    rss = read_html(str_glue("https://publicfiles.fcc.gov/tv-profile/{str_to_lower(station)}/rss"))
    entries = html_elements(rss, "entry")
    pdfs = map(entries, function(entry) {
        title = html_text(html_element(entry, "title"))
        if (str_detect(title, regex_folder)) {
            html_attr(html_element(entry, "link"), "href")
        } else {
            NULL
        }
    }) |>
        compact()

    avg_rate = 0
    n_ok = 0L
    for (i in seq_along(pdfs)) {
        rate = possibly(extract_station_rate, NA)(pdfs[[i]])
        if (!is.na(rate)) {
            avg_rate = avg_rate + rate
            n_ok = n_ok + 1
        }
        if (n_ok >= 3) break
    }
    avg_rate = na_if(avg_rate / n_ok, 0)

    tibble(station = station,
           rate = avg_rate)
}

d_rates = imap_dfr(d_dma$station, possibly(get_station_rate, NULL)) |>
    drop_na()

d_rates |>
    write_rds("data-raw/produced/station_rates.rds")

d_dma_rates = d_dma |>
    left_join(d_rates, by="station") |>
    group_by(market, state, households) |>
    summarize(lrate_avg = mean(log(rate), na.rm=TRUE),
              lrate_n = n(),
              lrate_se = sd(log(rate), na.rm=TRUE) / sqrt(lrate_n),
              .groups="drop") |>
    mutate(lrate_se = coalesce(lrate_se, sqrt(2)*mean(lrate_se, na.rm=T)))

m = brm(lrate_avg | resp_se(lrate_se, sigma=TRUE) ~ log(households) + (1 | state),
        data=d_dma_rates, backend="cmdstanr", control=list(adapt_delta=0.99))

pred = posterior_epred(m, newdata=d_dma_rates)

d_dma_rates |>
    mutate(pred_avg = colMeans(pred),
           pred_prec = 0.3^2 / matrixStats::colVars(pred), # 1/3 sd (overfitting etc)
           lrate_prec = lrate_se^(-2),
           rate = exp((lrate_avg*lrate_prec + pred_avg*pred_prec) /
               (lrate_prec + pred_prec)),
           rate_imp = coalesce(rate, exp(pred_avg))) |>
    select(market, state, households, rate_imp) |>
    arrange(desc(rate_imp)) |>
write_csv("data-raw/produced/ad_rates.csv")

