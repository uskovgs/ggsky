`%||%`      <- function(a, b) if (is.null(a)) b else a
is_waiver   <- function(x) inherits(x, "waiver")
rad         <- function(d) d * pi/180
wrap_pi     <- function(a) { a <- (a + pi) %% (2*pi) - pi; ifelse(a <= -pi, a + 2*pi, a) }
.is_blank   <- function(el) inherits(el, "element_blank")
.calc_el    <- function(name, theme) tryCatch(ggplot2:::calc_element(name, theme), error = function(e) NULL)

.line_gp <- function(el, default_col = "grey35", default_lwd = 0.9, default_lty = 1) {
  grid::gpar(
    col = el$colour %||% default_col,
    lwd = (el$linewidth %||% el$size %||% default_lwd),
    lty = el$linetype %||% default_lty,
    lineend = el$lineend %||% "butt"
  )
}

.rect_gp <- function(el, default_fill = NA, default_col = NA, default_lwd = 0.5, default_lty = 1) {
  grid::gpar(
    fill = el$fill %||% default_fill,
    col = el$colour %||% default_col,
    lwd = (el$linewidth %||% el$size %||% default_lwd),
    lty = el$linetype %||% default_lty
  )
}

.text_gp <- function(el, default_col = "grey25", default_size = 11) {
  grid::gpar(
    col = el$colour %||% default_col,
    fontsize = el$size %||% default_size,
    fontface = el$face %||% 1,
    fontfamily = el$family %||% ""
  )
}
.rel_ticks     <- function(step, eps = 1e-9) { n <- floor((180 - eps)/step); (-n:n) * step }
.lat_ticks     <- function(step) { k <- floor(90/step); seq(-k*step, k*step, by = step) }
.setdiff_near  <- function(x, y, tol = 1e-9) { if (!length(y)) return(x); x[vapply(x, function(xi) all(abs(xi - y) > tol), logical(1))] }
near_seam      <- function(L, center = .center_deg, tol = 1e-9) abs(((L - center) %% 360) - 180) < tol
label_lon_mirr <- function(rel_deg) paste0(ifelse(rel_deg <= 0, abs(rel_deg), 360 - rel_deg), "°")
fmt_deg_lat    <- function(v) paste0(ifelse(v < 0, "\u2212", ""), abs(v), "°")
fmt_ra_hours   <- function(deg) {
  h <- (deg / 15) %% 24
  r <- round(h)
  is_int <- abs(h - r) < 1e-9
  out <- ifelse(is_int, as.character(r), format(round(h, 1), nsmall = 1, trim = TRUE))
  paste0(out, "h")
}

.offset_outside_npc <- function(x, y, off = 0.02) {
  dx <- x - 0.5; dy <- y - 0.5; r <- sqrt(dx*dx + dy*dy)
  dx <- ifelse(r > 0, dx/r, 0); dy <- ifelse(r > 0, dy/r, 0)
  list(x = x + off*dx, y = y + off*dy)
}
