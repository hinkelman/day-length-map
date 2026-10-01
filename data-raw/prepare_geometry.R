# Pre-project the contiguous US to Albers Equal Area (EPSG:5070) and export
# everything the Elm front end needs, already in SVG pixel coordinates:
#
#   - nation : outline path (used for the clip path and the outer stroke)
#   - states : state boundary path
#   - bands  : 0.1-degree latitude bands clipped to the outline, each tagged
#              with its central latitude in tenths of a degree (the key Elm
#              computes day length on). Day length does not vary with
#              longitude, so a band is the natural unit: ~250 smooth polygons
#              instead of thousands of grid cells.
#   - parallels : reference lines of constant latitude
#
# Output is written to public/geometry.js as `window.GEOMETRY = {...}` so the
# page also works when opened straight from disk (no fetch, no server).
#
# Run from the repo root:  Rscript data-raw/prepare_geometry.R

library(sf)
library(jsonlite)

svg_width <- 960 # pixels
pad <- 10 # pixels around the outline
band_deg <- 0.1 # latitude band height in degrees
parallel_lats <- c(25, 30, 35, 40, 45)

# ---- Source geometry: Census cartographic boundary file (1:20M) -------------

src_url <- "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_state_20m.zip"
cache_dir <- file.path("data-raw", "cache")
zip_path <- file.path(cache_dir, basename(src_url))

if (!file.exists(zip_path)) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  download.file(src_url, zip_path, mode = "wb")
}
unzip(zip_path, exdir = cache_dir)

states <- st_read(
  file.path(cache_dir, sub("\\.zip$", ".shp", basename(src_url))),
  quiet = TRUE
)
states <- states[!states$STUSPS %in% c("AK", "HI", "PR"), ]
states <- st_transform(states, 5070)
nation <- st_union(states)

# ---- Projected metres -> SVG pixels -----------------------------------------

bb <- st_bbox(nation)
scale <- (svg_width - 2 * pad) / (bb[["xmax"]] - bb[["xmin"]])
svg_height <- ceiling((bb[["ymax"]] - bb[["ymin"]]) * scale + 2 * pad)

to_px_x <- function(x) (x - bb[["xmin"]]) * scale + pad
to_px_y <- function(y) (bb[["ymax"]] - y) * scale + pad

fmt <- function(v) formatC(round(v, 1), format = "f", digits = 1, drop0trailing = TRUE)

# Build one SVG path string from any (multi)polygon or (multi)linestring sfc.
# st_coordinates() puts ring/part ids in the L* columns; every distinct
# combination of them is one subpath.
to_path <- function(geom, close = TRUE) {
  xy <- st_coordinates(geom)
  ids <- grep("^L", colnames(xy), value = TRUE)
  key <- do.call(paste, c(as.data.frame(xy[, ids, drop = FALSE]), sep = "_"))
  parts <- split(seq_len(nrow(xy)), factor(key, levels = unique(key)))
  subpaths <- vapply(parts, function(i) {
    pts <- paste(fmt(to_px_x(xy[i, "X"])), fmt(to_px_y(xy[i, "Y"])), sep = ",")
    paste0("M", pts[1], "L", paste(pts[-1], collapse = " "), if (close) "Z" else "")
  }, character(1))
  paste(subpaths, collapse = "")
}

# ---- Latitude bands ---------------------------------------------------------

lat_range <- st_bbox(st_transform(nation, 4326))[c("ymin", "ymax")]
keys <- seq(floor(lat_range[[1]] / band_deg), ceiling(lat_range[[2]] / band_deg))

# Densify along longitude so the band edges follow the curved Albers parallels.
lons <- seq(-130, -60, by = 1)
strip <- function(k) {
  lo <- (k - 0.5) * band_deg
  hi <- (k + 0.5) * band_deg
  st_polygon(list(rbind(cbind(lons, lo), cbind(rev(lons), hi), c(lons[1], lo))))
}

strips <- st_sf(
  key = keys,
  geometry = st_transform(st_sfc(lapply(keys, strip), crs = 4326), 5070)
)
bands <- suppressWarnings(st_intersection(strips, nation))
bands <- bands[!st_is_empty(bands), ]
bands <- st_collection_extract(bands, "POLYGON")
bands <- aggregate(bands["key"], by = list(k = bands$key), FUN = function(x) x[1])

band_list <- lapply(seq_len(nrow(bands)), function(i) {
  list(lat = bands$key[i], path = to_path(st_geometry(bands)[i]))
})

# ---- Parallels --------------------------------------------------------------

parallels <- lapply(parallel_lats, function(lat) {
  line <- st_sfc(st_linestring(cbind(seq(-128, -64, by = 0.5), lat)), crs = 4326)
  line <- st_transform(line, 5070)
  xy <- st_coordinates(line)
  # Label at the western end, nudged inside the frame.
  list(
    lat = lat,
    path = to_path(line, close = FALSE),
    labelX = max(pad, round(to_px_x(xy[1, "X"]), 1)),
    labelY = round(to_px_y(xy[1, "Y"]), 1)
  )
})

# ---- Write ------------------------------------------------------------------

out <- list(
  width = svg_width,
  height = svg_height,
  nation = to_path(nation),
  states = to_path(st_geometry(states)),
  bands = band_list,
  parallels = parallels
)

dir.create("public", showWarnings = FALSE)
json <- toJSON(out, auto_unbox = TRUE, digits = NA)
writeLines(paste0("window.GEOMETRY = ", json, ";"), file.path("public", "geometry.js"))

message(sprintf(
  "Wrote public/geometry.js: %d x %d px, %d bands, latitudes %.1f to %.1f",
  svg_width, svg_height, length(band_list), min(bands$key) * band_deg, max(bands$key) * band_deg
))
