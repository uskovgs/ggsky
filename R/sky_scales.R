# ---- scales (поведение как у scale_*_continuous) -----------------------------
scale_gal_lat <- function(breaks = waiver(), minor_breaks = waiver()) {
  structure(list(breaks = breaks, minor_breaks = minor_breaks), class = "sky_scale_lat")
}
scale_gal_lon <- function(breaks = waiver(), minor_breaks = waiver()) {
  structure(list(breaks = breaks, minor_breaks = minor_breaks), class = "sky_scale_lon")
}
ggplot_add.sky_scale_lat <- function(object, plot, object_name) {
  if (!inherits(plot$coordinates, "CoordGalactic")) plot <- plot + coord_galactic()
  plot$coordinates$lat_breaks_major <- object$breaks
  plot$coordinates$lat_breaks_minor <- object$minor_breaks
  plot
}
ggplot_add.sky_scale_lon <- function(object, plot, object_name) {
  if (!inherits(plot$coordinates, "CoordGalactic")) plot <- plot + coord_galactic()
  plot$coordinates$lon_breaks_major <- object$breaks   # абсолютные ℓ (0..360)
  plot$coordinates$lon_breaks_minor <- object$minor_breaks
  plot
}
