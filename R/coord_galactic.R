# ---- coord -------------------------------------------------------------------
CoordGalactic <- ggplot2::ggproto(
  "CoordGalactic", ggplot2::Coord,

  .text_probe = function(self, label, gp) {
    grid::textGrob(label, gp = gp)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    list(
      x_range = c(0, 1), y_range = c(0, 1),
      lat_breaks_major = self$lat_breaks_major %||% waiver(),
      lat_breaks_minor = self$lat_breaks_minor %||% waiver(),
      lon_breaks_major = self$lon_breaks_major %||% waiver(),
      lon_breaks_minor = self$lon_breaks_minor %||% waiver()
    )
  },

  # DATA PROJECTION: (l,b) [deg] -> Hammer -> npc, with X reflection
  transform = function(self, data, panel_params) {
    if (!nrow(data) || is.null(data$x) || is.null(data$y)) return(data)
    lon_deg <- (as.numeric(data$x) %% 360)
    lat_deg <- pmin(90, pmax(-90, as.numeric(data$y)))
    p  <- proj_hammer(rad(lon_deg), rad(lat_deg))   # lon0 = 0°
    np <- to_npc(-p$x, p$y)                         # mirror X
    data$x <- np$x; data$y <- np$y
    data
  },
  transform_polygon = function(self, data, panel_params) {
    if (!nrow(data)) return(data)
    max_deg <- self$munch_deg %||% 1

    # 1) discretize edges in (l,b)
    seg <- .gal_segmentize(
      data.frame(x = data$x, y = data$y, group = data$group %||% 1L),
      max_deg = max_deg
    )

    # 2) split by NA into real subgroups (geom_polygon ignores NA)
    .split_by_na_groups <- function(df) {
      if (!nrow(df)) return(list(df))
      br <- is.na(df$x) | is.na(df$y)
      if (!any(br)) return(list(df))
      idx <- which(!br)
      blk <- cumsum(c(0, diff(idx) != 1))[seq_along(idx)]
      split(df[idx, , drop = FALSE], blk)
    }
    subs <- .split_by_na_groups(seg)

    # 3) renumber group so pieces are not stitched by a chord
    base <- as.integer((data$group %||% 1L)[1])
    parts <- Map(function(df, k) { df$group <- base*1000L + k; df }, subs, seq_along(subs))
    data2 <- do.call(rbind, parts)

    # 4) project to npc using our transform()
    self$transform(
      data.frame(x = data2$x, y = data2$y, group = data2$group),
      panel_params
    )
  },
  transform_path = function(self, data, panel_params) {
    if (!nrow(data)) return(data)
    max_deg <- self$munch_deg %||% 1
    data2 <- .gal_segmentize(data, max_deg = max_deg)
    self$transform(data2, panel_params)
  },

  backtransform_range = function(self, panel_params) {
    list(x = c(0, 360), y = c(-90, 90))
  },

  # Hammer has width:height ratio 2:1, so h/w = 0.5.
  aspect    = function(self, panel_params) 0.5,
  is_linear = function() FALSE,
  range     = function(self, panel_params) list(x = panel_params$x_range, y = panel_params$y_range),

  render_bg = function(self, panel_params, theme) {
    el_bg <- .calc_el("panel.background", theme)
    gp_bg <- if (!is.null(el_bg) && !.is_blank(el_bg)) {
      .rect_gp(el_bg, default_fill = NA, default_col = NA)
    } else {
      grid::gpar(fill = NA, col = NA)
    }
    gp_bg$col <- NA
    ggplot2:::ggname("galactic-grid-bg",
                     grobTree(
                       grob_ellipse_fill(gp = gp_bg),
                       grob_grid(lat_breaks_major = panel_params$lat_breaks_major,
                                 lat_breaks_minor = panel_params$lat_breaks_minor,
                                 lon_breaks_major_abs = panel_params$lon_breaks_major,
                                 lon_breaks_minor_abs = panel_params$lon_breaks_minor,
                                 theme = theme)
                     )
    )
  },

  render_fg = function(self, panel_params, theme) {
    # styles from theme (X axis for longitudes, Y axis for latitudes)
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

    lat_labels <- if (is_waiver(panel_params$lat_breaks_major))
      .lat_ticks(30) else (panel_params$lat_breaks_major %||% numeric(0))
    lon_labels <- panel_params$lon_breaks_major  # waiver()/NULL/num
    if (!draw_lon_labels) lon_labels <- NULL
    if (!draw_lat_labels) lat_labels <- numeric(0)

    ggplot2:::ggname("galactic-grid-fg",
                     grobTree(
                       if (draw_mask) grob_outside_mask(inset = mask_inset, gp = gp_mask) else ggplot2::zeroGrob(),
                       grob_outline(gp = gp_outline),
                       grob_labels(lat_labels = lat_labels,
                                   lon_labels_abs = lon_labels,
                                   major_step = 30,
                                   gp_lon = gp_lon,
                                   gp_lat = gp_lat,
                                   off_lon = self$off_lon %||% 0.025,
                                   off_lat = self$off_lat %||% 0.035)
                     ))
  },

  render_axis_h = function(self, panel_params, theme) {
    elx <- .calc_el("axis.text.x", theme)
    ely <- .calc_el("axis.text.y", theme)
    show_lon <- !is.null(elx) && !.is_blank(elx)
    show_lat <- !is.null(ely) && !.is_blank(ely)
    gp_lon <- .text_gp(elx, default_col = "grey25", default_size = 11); gp_lon$col <- NA
    gp_lat <- .text_gp(ely, default_col = "grey25", default_size = 11); gp_lat$col <- NA

    lat_breaks <- panel_params$lat_breaks_major
    has_poles <- if (is_waiver(lat_breaks)) TRUE else any(abs((lat_breaks %||% numeric(0))) == 90)

    top <- if (!is.null(self$pad_top_pt)) {
      if (self$pad_top_pt > 0) grid::textGrob(" ", gp = grid::gpar(fontsize = self$pad_top_pt, col = NA)) else ggplot2::zeroGrob()
    } else if (show_lat && has_poles) {
      self$.text_probe("90°", gp_lat)
    } else {
      ggplot2::zeroGrob()
    }

    bottom <- if (!is.null(self$pad_bottom_pt)) {
      if (self$pad_bottom_pt > 0) grid::textGrob(" ", gp = grid::gpar(fontsize = self$pad_bottom_pt, col = NA)) else ggplot2::zeroGrob()
    } else if (show_lon) {
      self$.text_probe("360°", gp_lon)
    } else {
      ggplot2::zeroGrob()
    }
    list(top = top, bottom = bottom)
  },
  render_axis_v = function(self, panel_params, theme) {
    ely <- .calc_el("axis.text.y", theme)
    show_lat <- !is.null(ely) && !.is_blank(ely)
    gp_lat <- .text_gp(ely, default_col = "grey25", default_size = 11); gp_lat$col <- NA

    left <- if (!is.null(self$pad_left_pt)) {
      if (self$pad_left_pt > 0) grid::textGrob(" ", gp = grid::gpar(fontsize = self$pad_left_pt, col = NA)) else ggplot2::zeroGrob()
    } else if (show_lat) {
      self$.text_probe("\u221290\u00b0", gp_lat)
    } else {
      ggplot2::zeroGrob()
    }

    right <- if (!is.null(self$pad_right_pt)) {
      if (self$pad_right_pt > 0) grid::textGrob(" ", gp = grid::gpar(fontsize = self$pad_right_pt, col = NA)) else ggplot2::zeroGrob()
    } else if (show_lat) {
      self$.text_probe("\u221290\u00b0", gp_lat)
    } else {
      ggplot2::zeroGrob()
    }
    list(left = left, right = right)
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
coord_galactic <- function(clip = "on",
                           label_offset_lon = 0.025,
                           label_offset_lat = 0.035,
                           munch_deg = 1,
                           pad_top_pt = NULL,
                           pad_bottom_pt = NULL,
                           pad_left_pt = NULL,
                           pad_right_pt = NULL,
                           clip_on_boundaries = TRUE) {
  ggplot2::ggproto(NULL, CoordGalactic,
                   limits   = list(x = NULL, y = NULL),
                   clip     = clip,
                   off_lon  = label_offset_lon,
                   off_lat  = label_offset_lat,
                   munch_deg = munch_deg,
                   pad_top_pt = pad_top_pt,
                   pad_bottom_pt = pad_bottom_pt,
                   pad_left_pt = pad_left_pt,
                   pad_right_pt = pad_right_pt,
                   clip_on_boundaries = clip_on_boundaries
  )
}

GeomPathGalactic <- ggplot2::ggproto(
  "GeomPathGalactic", ggplot2::GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, lineend = "butt",
                        linejoin = "round", linemitre = 10, na.rm = FALSE) {
    if (inherits(coord, "CoordGalactic") && nrow(data) > 1) {
      max_deg <- coord$munch_deg %||% 1
      data <- .gal_segmentize_layer(data, max_deg = max_deg)
    }
    ggplot2::GeomPath$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm
    )
  }
)

GeomSegmentGalactic <- ggplot2::ggproto(
  "GeomSegmentGalactic", ggplot2::GeomSegment,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    if (!inherits(coord, "CoordGalactic")) {
      return(ggplot2::GeomSegment$draw_panel(
        data = data, panel_params = panel_params, coord = coord,
        arrow = arrow, arrow.fill = arrow.fill, lineend = lineend, linejoin = linejoin, na.rm = na.rm
      ))
    }

    data$xend <- data$xend %||% data$x
    data$yend <- data$yend %||% data$y
    if (!nrow(data)) return(ggplot2::zeroGrob())
    if (is.null(data$group)) data$group <- seq_len(nrow(data))

    starts <- subset(data, select = c(-xend, -yend))
    ends <- subset(data, select = c(-x, -y))
    names(ends)[names(ends) == "xend"] <- "x"
    names(ends)[names(ends) == "yend"] <- "y"
    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group), , drop = FALSE]

    max_deg <- coord$munch_deg %||% 1
    pieces <- .gal_segmentize_layer(pieces, max_deg = max_deg)
    if (!nrow(pieces)) return(ggplot2::zeroGrob())

    ggplot2::GeomPath$draw_panel(
      data = pieces,
      panel_params = panel_params,
      coord = coord,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm
    )
  }
)

ggplot_add.CoordGalactic <- function(object, plot, object_name) {
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
