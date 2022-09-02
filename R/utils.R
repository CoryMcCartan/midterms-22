date_midpt = function(d1, d2) {
    int = interval(d1, d2)
    as_date(int_start(int) + (int_end(int) - int_start(int))/2)
}
