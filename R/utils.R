# UTILITY FUNCTIONS

plot_time <- function(d, qty, election_date, ylab=NULL, thin=1) {
    yr <- strftime(election_date, "%Y")
    ggplot(filter(d, day %% thin == 1), aes(date, {{ qty }})) +
        geom_hline(yintercept=0.5, lty="dashed") +
        geom_ribbon(aes(ymin=pmin(.lower, 0.5), ymax=pmin(.upper, 0.5),
                        group=.width), fill="#a020103a") +
        geom_ribbon(aes(ymin=pmax(.lower, 0.5), ymax=pmax(.upper, 0.5),
                        group=.width), fill="#1020c03a") +
        geom_textvline(xintercept=election_date, label="Election Day",
                       linewidth=0.3, hjust=0.99, vjust=-0.3, size=3, fontface="bold") +
        {if (yr != "2022" && deparse(substitute(qty)) == "natl_dem") {
            tmp <- read_csv(here("data/fundamentals.csv"), progress=FALSE, show_col_types=FALSE)
            idx = which(as.character(tmp$year) == yr)
            y = plogis(tmp$linc_vote[idx] * (2*tmp$dem_pres[idx] - 1))
            geom_texthline(yintercept=y, label="Actual", linewidth=0.5,
                           hjust=0.8, vjust=-0.3, size=3, fontface="bold")
        }}  +
        {if (Sys.Date() < election_date)
            geom_textvline(xintercept=Sys.Date(), label="Today",
                           linewidth=0.3, hjust=0.99, vjust=-0.3, size=3, fontface="bold")
        }  +
        geom_line(aes(lty=date >= Sys.Date()), lwd=1.2) +
        scale_y_continuous(ylab, labels=scales::percent, breaks=seq(0, 1, 0.01)) +
        scale_x_date(NULL, date_breaks="1 month", date_labels="%B",
                     minor_breaks=election_date - seq(-14, 300, 7),
                     expand=expansion(mult=c(0, 0.05))) +
        guides(lty="none") +
        labs(title=yr) +
        theme_bw()
}


date_midpt <- function(d1, d2) {
    int <- lubridate::interval(d1, d2)
    start <- lubridate::int_start(int)
    lubridate::as_date(start + (lubridate::int_end(int) - start)/2)
}


match_manual <- function(xx, yy) {
    gen_regex = " (the|surveys?|data|co\\.?|company|inc\\.?|panel|university|college) "
    xx = unique(xx)
    yy = unique(yy)
    xx_short = str_squish(str_remove_all(str_to_lower(str_c(" ", xx, " ")), gen_regex))
    yy_short = str_squish(str_remove_all(str_to_lower(str_c(" ", yy, " ")), gen_regex))
    dists = adist(str_squish(str_to_lower(xx)),
                  str_squish(str_to_lower(yy)), costs=list(ins=1, del=1, subst=20))
    dists_rem = adist(xx_short, yy_short, costs=list(ins=1, del=1, subst=20))
    dists[dists_rem < dists] = dists_rem[dists_rem < dists]

    out = xx
    names(out) = xx
    for (i in seq_along(xx)) {
        dd = dists[i, ]
        if (sum(dd == 0) == 1) {
            out[i] = yy[which(dd == 0)]
        } else {
            opts <- yy[head(order(dd), 16)]
            choice <- select.list(opts, title=xx[i])
            if (choice != "") out[i] = choice
        }
    }
    out
}
