# Day length map

**Live:** https://hinkelman.github.io/day-length-map/

How many days a year does each part of the contiguous US get at least *X* hours
of daylight? Drag the slider to pick *X*. You can also switch "day" from
sunrise–sunset to civil twilight.

Day length depends only on latitude and date, so the map is a set of latitude
stripes that curve with the Albers projection. A chart beside it shows the same
numbers by latitude, and hovering either one reads out a latitude.

## How it works

- **R (offline, run once):** `data-raw/prepare_geometry.R` downloads the Census
  1:20M state boundaries and keeps the lower 48. It projects them to Albers
  Equal Area (EPSG:5070), cuts the outline into 0.1° latitude bands, and writes
  everything in SVG pixel coordinates to `public/geometry.js`.
- **Elm (in the browser):** `src/Solar.elm` computes day length from the NOAA
  declination series and the sunrise hour-angle equation (−0.833° for
  sunrise/sunset, −6° for civil twilight). `src/Main.elm` works out 365 day
  lengths per band once per definition. Moving the slider only re-counts them
  and recolors 250 SVG paths, with no server involved.

## Build and run

Requires Elm 0.19. R with `sf` and `jsonlite` is needed only to regenerate the
geometry, because `public/geometry.js` is committed.

```bash
make serve
```

Then open http://localhost:8000. Opening `public/index.html` directly from disk
also works after `make build`.

To regenerate the geometry (for example, after changing band size or map width
at the top of the R script):

```bash
make geometry
```

## Deploy

`.github/workflows/pages.yml` builds the Elm app and publishes `public/` to
GitHub Pages on every push to `main`. One-time setup: in the repository's
**Settings → Pages**, set **Source** to **GitHub Actions**.
