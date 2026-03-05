# ---- scales (behavior similar to scale_*_continuous) -------------------------
#' Galactic latitude scale settings
#'
#' @param breaks,minor_breaks Break specification passed to `coord_galactic()`.
#' @returns An object consumed by `ggplot_add()`.
#' @export
scale_gal_lat <- function(breaks = ggplot2::waiver(), minor_breaks = ggplot2::waiver()) {
  structure(list(breaks = breaks, minor_breaks = minor_breaks), class = "sky_scale_lat")
}
#' Galactic longitude scale settings
#'
#' @param breaks,minor_breaks Break specification passed to `coord_galactic()`.
#' @returns An object consumed by `ggplot_add()`.
#' @export
scale_gal_lon <- function(breaks = ggplot2::waiver(), minor_breaks = ggplot2::waiver()) {
  structure(list(breaks = breaks, minor_breaks = minor_breaks), class = "sky_scale_lon")
}
#' @export
ggplot_add.sky_scale_lat <- function(object, plot, ...) {
  if (!inherits(plot$coordinates, "CoordGalactic")) plot <- plot + coord_galactic()
  plot$coordinates$lat_breaks_major <- object$breaks
  plot$coordinates$lat_breaks_minor <- object$minor_breaks
  plot
}
#' @export
ggplot_add.sky_scale_lon <- function(object, plot, ...) {
  if (!inherits(plot$coordinates, "CoordGalactic")) plot <- plot + coord_galactic()
  plot$coordinates$lon_breaks_major <- object$breaks   # absolute l (0..360)
  plot$coordinates$lon_breaks_minor <- object$minor_breaks
  plot
}
