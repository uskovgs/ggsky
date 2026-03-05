CoordEquatorial <- ggplot2::ggproto(
  "CoordEquatorial", CoordGalactic,
  render_fg = function(self, panel_params, theme) {
    elx <- .calc_el("axis.text.x", theme)
    ely <- .calc_el("axis.text.y", theme)
    el_plot_bg <- .calc_el("plot.background", theme)
    el_border <- .calc_el("panel.border", theme)
    el_maj_x <- .calc_el("panel.grid.major.x", theme)
    el_maj_y <- .calc_el("panel.grid.major.y", theme)

    gp_lon <- .text_gp(elx, default_col = "grey25", default_size = 11)
    gp_lat <- .text_gp(ely, default_col = "grey25", default_size = 11)
    mask_inset <- 0.0015
    draw_mask <- isTRUE(self$clip_on_boundaries)
    mask_fill <- "white"
    if (!is.null(el_plot_bg) && !.is_blank(el_plot_bg)) {
      candidate <- el_plot_bg$fill %||% NA
      if (!is.na(candidate)) mask_fill <- candidate
    }
    gp_mask <- grid::gpar(fill = mask_fill, col = NA)
    gp_mask$col <- NA
    gp_outline <- if (!is.null(el_border) && !.is_blank(el_border)) {
      .line_gp(el_border, default_col = "black", default_lwd = 1)
    } else {
      .line_gp(el_maj_x %||% el_maj_y, default_col = "black", default_lwd = 1)
    }
    draw_lon_labels <- !is.null(elx) && !.is_blank(elx)
    draw_lat_labels <- !is.null(ely) && !.is_blank(ely)

    lat_labels <- if (is_waiver(panel_params$lat_breaks_major)) {
      .lat_ticks(30)
    } else {
      panel_params$lat_breaks_major %||% numeric(0)
    }
    lon_labels <- panel_params$lon_breaks_major
    if (!draw_lon_labels) lon_labels <- NULL
    if (!draw_lat_labels) lat_labels <- numeric(0)

    ggplot2:::ggname("equatorial-grid-fg",
      grobTree(
        if (draw_mask) grob_outside_mask(inset = mask_inset, gp = gp_mask) else ggplot2::zeroGrob(),
        grob_outline(gp = gp_outline),
        grob_labels(
          lat_labels = lat_labels,
          lon_labels_abs = lon_labels,
          major_step = 30,
          lon_label_fn = fmt_ra_hours,
          gp_lon = gp_lon,
          gp_lat = gp_lat,
          off_lon = self$off_lon %||% 0.025,
          off_lat = self$off_lat %||% 0.035
        )
      )
    )
  }
)

#' Title
#'
#' @param clip
#' @param label_offset_lon
#' @param label_offset_lat
#' @param munch_deg
#' @param pad_top_pt
#' @param pad_bottom_pt
#' @param pad_left_pt
#' @param pad_right_pt
#' @param clip_on_boundaries
#'
#' @returns
#' @export
#'
#' @examples
coord_equatorial <- function(clip = "on",
                             label_offset_lon = 0.025,
                             label_offset_lat = 0.035,
                             munch_deg = 1,
                             pad_top_pt = NULL,
                             pad_bottom_pt = NULL,
                             pad_left_pt = NULL,
                             pad_right_pt = NULL,
                             clip_on_boundaries = TRUE) {
  ggplot2::ggproto(NULL, CoordEquatorial,
    limits = list(x = NULL, y = NULL),
    clip = clip,
    off_lon = label_offset_lon,
    off_lat = label_offset_lat,
    munch_deg = munch_deg,
    pad_top_pt = pad_top_pt,
    pad_bottom_pt = pad_bottom_pt,
    pad_left_pt = pad_left_pt,
    pad_right_pt = pad_right_pt,
    clip_on_boundaries = clip_on_boundaries
  )
}

scale_eq_dec <- function(breaks = waiver(), minor_breaks = waiver()) {
  structure(list(breaks = breaks, minor_breaks = minor_breaks), class = "sky_scale_dec")
}

scale_eq_ra <- function(breaks = waiver(), minor_breaks = waiver()) {
  structure(list(breaks = breaks, minor_breaks = minor_breaks), class = "sky_scale_ra")
}

ggplot_add.CoordEquatorial <- function(object, plot, object_name) {
  plot <- ggplot2:::ggplot_add.Coord(object, plot, object_name)
  plot$labels$x <- ""
  plot$labels$y <- ""
  for (i in seq_along(plot$layers)) {
    if (inherits(plot$layers[[i]]$geom, "GeomPath")) {
      plot$layers[[i]]$geom <- GeomPathGalactic
    } else if (inherits(plot$layers[[i]]$geom, "GeomSegment")) {
      plot$layers[[i]]$geom <- GeomSegmentGalactic
    }
  }
  plot
}

ggplot_add.sky_scale_dec <- function(object, plot, object_name) {
  if (!inherits(plot$coordinates, "CoordEquatorial")) plot <- plot + coord_equatorial()
  plot$coordinates$lat_breaks_major <- object$breaks
  plot$coordinates$lat_breaks_minor <- object$minor_breaks
  plot
}

ggplot_add.sky_scale_ra <- function(object, plot, object_name) {
  if (!inherits(plot$coordinates, "CoordEquatorial")) plot <- plot + coord_equatorial()
  plot$coordinates$lon_breaks_major <- object$breaks
  plot$coordinates$lon_breaks_minor <- object$minor_breaks
  plot
}
