# projection center: 0° (zero at center, seam at ±180°)
.center_deg <- 0
.center_rad <- 0

# Hammer-Aitoff (all arguments in radians), lon0 is the center
proj_hammer <- function(lon, lat, lon0 = .center_rad) {
  dl <- wrap_pi(lon - lon0)
  D  <- sqrt(pmax(1 + cos(lat)*cos(dl/2), .Machine$double.eps))
  x  <- 2*sqrt(2) * cos(lat) * sin(dl/2) / D
  y  <-   sqrt(2) * sin(lat) / D
  list(x = x, y = y)
}

# native -> npc: scale X and Y separately so the ellipse touches panel bounds.
to_npc <- function(x, y) {
  list(
    x = 0.5 + x / (4 * sqrt(2)),
    y = 0.5 + y / (2 * sqrt(2))
  )
}
