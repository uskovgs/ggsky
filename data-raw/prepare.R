library(tidyverse)
library(xrayr)
library(httr2)

# Download

txt <- request("https://vizier.cds.unistra.fr/viz-bin/asu-tsv") |>
  req_url_query(
    `-source` = "J/A+A/687/A183",
    `-out.max` = "unlimited"
  ) |>
  req_perform() |>
  resp_body_string()

artss15 <- read_tsv(txt, comment = "#", show_col_types = FALSE, skip = 3) |>
  janitor::clean_names() |>
  _[-c(1,2), ] |>
  type_convert(col_types = 
    cols(
      raj2000 = col_double(),
      dej2000 = col_double(),
      flux = col_double(),
      e_flux = col_double(),
      type = col_character()
    )
  ) |>
  mutate(
    flux = 1e-12 * flux,
    e_flux = 1e-12 * e_flux,
    skycoord = xrayr::ra_dec(raj2000, dej2000),
    l = xrayr::to_galactic(skycoord)$l,
    b = xrayr::to_galactic(skycoord)$b,
    log_flux = log10(flux),
    # flux_factor = cut(log_flux, breaks = c(-Inf, -12, -10, Inf), labels = c("<-12", "-12 to -10",  ">-10"))
  ) |>
  rename(ra = raj2000, dec = dej2000) |>
  select(name, ra, dec, e_pos, l, b, flux, e_flux, type, z, c_name) 








artss15 <- as.data.frame(artss15)
usethis::use_data(artss15, overwrite = TRUE)


# Equator
ra <- 0:359
coords_eq0 <- xrayr::ra_dec(ra, rep(0, length(ra)))
coords_eq0_gal <- xrayr::to_galactic(coords_eq0)
equator <- data.frame(
  l = coords_eq0_gal$l,
  b = coords_eq0_gal$b,
  ra = xrayr::ra(coords_eq0)
)

usethis::use_data(equator, overwrite = TRUE)
