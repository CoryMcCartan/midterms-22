library(tidyverse)
library(sf)
library(rmapshaper)
library(here)

d_states = tigris::states(cb=TRUE, resolution="5m") |>
    tigris::shift_geometry() |>
    filter(!STUSPS %in% c("DC", "PR", "VI", "GU", "MP", "AS")) |>
    transmute(state = STUSPS,
              area = as.numeric(st_area(geometry)),
              scale = sqrt(min(area) / area))
# OK special
d_special = d_states[d_out$state == "OK", ]
d_special$state = "OK-S"
d_special$scale = d_special$scale / sqrt(2)
d_states$scale[d_out$state == "OK"] = d_special$scale
d_states = bind_rows(d_states, d_special)

bbox = st_bbox(d_states)
sc = c(bbox[3] - bbox[1], bbox[4] - bbox[2])

offsets = list(
    list(states = c("WA", "OR", "CA", "NV", "ID", "MT", "WY", "UT", "CO",
                    "AZ", "NM", "ND", "SD", "NE", "KS", "OK", "TX"),
         offset = c(0.01, -0.005)),
    list(states = c("WA", "OR", "CA", "NV", "ID", "MT", "WY", "UT", "CO", "AZ", "NM"),
         offset = c(0.025, -0.005)),
    list(states = c("WA", "OR", "CA", "NV", "ID", "UT", "AZ"),
         offset = c(0.015, -0.002)),
    list(states = c("WA", "OR", "CA", "NV"), offset = c(0.01, -0.002)),
    list(states = "ND", offset=c(0.0, -0.01)),
    list(states = "MT", offset=c(0.0, 0.018)),
    list(states = "ID", offset=c(0.002, 0.002)),
    list(states = "OH", offset=c(-0.002, 0.004)),
    list(states = c("IL", "IN", "OH"), offset=c(-0.001, 0.004)),
    list(states = "MI", offset=c(0.004, 0.002)),
    list(states = "WI", offset=c(0.002, 0.001)),
    list(states = "TN", offset=c(-0.003, -0.005)),
    list(states = "GA", offset=c(-0.007, -0.001)),
    list(states = "LA", offset=c(-0.01, 0.0)),
    list(states = "FL", offset=c(-0.015, 0.005)),
    list(states = "TX", offset=c(0.01, 0.01)),
    list(states = "IN", offset=c(-0.001, 0.004)),
    list(states = "VA", offset=c(0.012, -0.004)),
    list(states = c("VA", "NC", "SC"), offset=c(0.00, -0.008)),
    list(states = "SC", offset=c(0.002, -0.007)),
    list(states = c("MD", "DE", "PA", "NJ", "NY", "MA", "CT", "VT", "NH", "RI", "ME"),
         offset = c(0.025, 0.03)),
    list(states = "WV", offset=c(0.001, 0.003)),
    list(states = "MD", offset=c(-0.022, 0.030)),
    list(states = c("PA", "NY"), offset=c(-0.035, 0.045)),
    list(states = "DE", offset=c(0.030, -0.014)),
    list(states = "NJ", offset=c(0.057, 0.013)),
    list(states = "NY", offset=c(-0.005, 0.00)),
    list(states = c("MA", "CT", "VT", "NH", "RI", "ME"),
         offset = c(0.007, 0.055)),
    list(states = c("MA", "VT", "NH", "ME"),
         offset = c(0.01, 0.04)),
    list(states = "CT", offset=c(-0.011, -0.004)),
    list(states = "VT", offset=c(-0.050, -0.008)),
    list(states = "NH", offset=c(-0.021, 0.038)),
    list(states = "ME", offset=c(-0.003, 0.03)),
    list(states = "RI", offset=c(0.054, -0.053)),
    list(states = "HI", offset=c(-0.04, 0.03)),
    list(states = "OK", offset=c(-0.04, 0.000)),
    list(states = "OK-S", offset=c(0.018, 0.014)),
    list(states = c("AK", "HI"), offset=c(0.08, 0.08))
)

scaled = do.call(c, imap(d_states$geometry, function(g, i) {
    ctr = st_centroid(g)
    offset = matrix(c(0, 0), nrow=1)
    for (obj in offsets) {
        if (d_states$state[i] %in% obj$states) {
            offset = offset + sc*obj$offset
        }
    }

    fac = d_states$scale[i]
    g = (g - ctr) * diag(5 * rep(fac, 2)) + ctr + offset
    ms_simplify(st_sfc(g), keep=0.03 + 0.1*fac)
}))
plot(scaled)


d_out = st_drop_geometry(d_states) |>
    transmute(state=state, geometry=scaled) |>
    st_as_sf() |>
    st_set_crs(5070) |>
    st_transform(3857)
write_rds(d_out, here("data-raw/manual/states_eqarea.rds"), compress="xz")

json = geojsonio::topojson_json(d_out, quantization=1e5)
write_file(json, here("docs/states.json"))
