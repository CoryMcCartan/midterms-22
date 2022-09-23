export function fmt_pct(x) {
    if (x <= 0.01)
        return "<1%";
    else if (x > 0.99)
        return ">99%";
    else
        return (100*x).toFixed(0) + "%";
};
