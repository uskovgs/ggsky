# центр проекции: 0° (ноль по центру, шов на ±180°)
.center_deg <- 0
.center_rad <- 0

# Hammer–Aitoff (все аргументы в радианах), lon0 — центр
proj_hammer <- function(lon, lat, lon0 = .center_rad) {
  dl <- wrap_pi(lon - lon0)
  D  <- sqrt(pmax(1 + cos(lat)*cos(dl/2), .Machine$double.eps))
  x  <- 2*sqrt(2) * cos(lat) * sin(dl/2) / D
  y  <-   sqrt(2) * sin(lat) / D
  list(x = x, y = y)
}

# native -> npc: X и Y масштабируем отдельно, чтобы эллипс касался границ панели.
to_npc <- function(x, y) {
  list(
    x = 0.5 + x / (4 * sqrt(2)),
    y = 0.5 + y / (2 * sqrt(2))
  )
}
