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
