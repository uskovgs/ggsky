# ---- primitives (all in npc) -------------------------------------------------
# (!) Mirror X everywhere: use -p$x so 359,358,... go to the right.
grob_outline <- function(eps = 1e-6, n = 2000, gp = grid::gpar(col = "black", lwd = 1)) {
  b  <- seq(-90, 90, length.out = n)
  lL <- (.center_deg - 180 + eps) %% 360
  lR <- (.center_deg + 180 - eps) %% 360
  pL <- proj_hammer(rad(lL), rad(b))
  pR <- proj_hammer(rad(lR), rad(b))
  npL <- to_npc(-pL$x, pL$y); npR <- to_npc(-pR$x, pR$y)
  grobTree(
    polylineGrob(unit(npL$x, "npc"), unit(npL$y, "npc"), gp = gp),
    polylineGrob(unit(npR$x, "npc"), unit(npR$y, "npc"), gp = gp)
  )
}

grob_ellipse_fill <- function(eps = 1e-6, n = 2000, gp = grid::gpar(fill = NA, col = NA)) {
  b  <- seq(-90, 90, length.out = n)
  lL <- (.center_deg - 180 + eps) %% 360
  lR <- (.center_deg + 180 - eps) %% 360
  pL <- proj_hammer(rad(lL), rad(b))
  pR <- proj_hammer(rad(lR), rad(b))
  npL <- to_npc(-pL$x, pL$y)
  npR <- to_npc(-pR$x, pR$y)
  polygonGrob(
    x = unit(c(npL$x, rev(npR$x)), "npc"),
    y = unit(c(npL$y, rev(npR$y)), "npc"),
    gp = gp
  )
}

grob_outside_mask <- function(eps = 1e-6, n = 2000, inset = 0, gp = grid::gpar(fill = "white", col = NA)) {
  b  <- seq(-90, 90, length.out = n)
  lL <- (.center_deg - 180 + eps) %% 360
  lR <- (.center_deg + 180 - eps) %% 360
  pL <- proj_hammer(rad(lL), rad(b))
  pR <- proj_hammer(rad(lR), rad(b))
  npL <- to_npc(-pL$x, pL$y)
  npR <- to_npc(-pR$x, pR$y)
  # Make the outer contour much larger than the panel so masking works
  # even with coord(clip = "off"), when geoms can go outside [0,1] npc.
  x_outer <- c(-5, 6, 6, -5, -5)
  y_outer <- c(-5, -5, 6, 6, -5)
  x_ell <- c(npL$x, rev(npR$x), npL$x[1])
  y_ell <- c(npL$y, rev(npR$y), npL$y[1])
  shr <- .shrink_npc_path(x_ell, y_ell, inset = inset)
  x_ell <- shr$x
  y_ell <- shr$y

  pathGrob(
    x = unit(c(x_outer, x_ell), "npc"),
    y = unit(c(y_outer, y_ell), "npc"),
    id = c(rep(1L, length(x_outer)), rep(2L, length(x_ell))),
    rule = "evenodd",
    gp = gp
  )
}

.shrink_npc_path <- function(x, y, inset = 0) {
  if (inset <= 0) return(list(x = x, y = y))
  cx <- 0.5
  cy <- 0.5
  dx <- x - cx
  dy <- y - cy
  r <- sqrt(dx * dx + dy * dy)
  nx <- ifelse(r > 0, dx / r, 0)
  ny <- ifelse(r > 0, dy / r, 0)
  list(x = x - inset * nx, y = y - inset * ny)
}

grob_parallel <- function(b_deg, eps = 1e-6, n = 721, gp = grid::gpar()) {
  lonL <- (.center_deg + seq(-180 + eps, -eps, length.out = n)) %% 360
  lonR <- (.center_deg + seq(   eps,  180 - eps, length.out = n)) %% 360
  pL <- proj_hammer(rad(lonL), rad(b_deg))
  pR <- proj_hammer(rad(lonR), rad(b_deg))
  npL <- to_npc(-pL$x, pL$y); npR <- to_npc(-pR$x, pR$y)
  grobTree(
    polylineGrob(unit(npL$x, "npc"), unit(npL$y, "npc"), gp = gp),
    polylineGrob(unit(npR$x, "npc"), unit(npR$y, "npc"), gp = gp)
  )
}

grob_meridian <- function(l_deg, eps = 1e-6, n = 361, gp = grid::gpar()) {
  L <- l_deg %% 360; if (near_seam(L)) return(ggplot2::zeroGrob())
  lat <- seq(-90 + eps, 90 - eps, length.out = n)
  p   <- proj_hammer(rad(L), rad(lat))
  np  <- to_npc(-p$x, p$y)
  polylineGrob(unit(np$x, "npc"), unit(np$y, "npc"), gp = gp)
}

# ---- labels (in npc) ---------------------------------------------------------
grob_labels <- function(lat_labels,
                        lon_labels_abs = waiver(),
                        major_step = 30,
                        eps = 1e-6,
                        lon_label_fn = function(v) paste0(v, "°"),
                        gp_lon = grid::gpar(col = "grey25"),
                        gp_lat = grid::gpar(col = "grey25"),
                        off_lon = 0.025,   # below the equator (npc)
                        off_lat = 0.035) { # outward from the outline (npc)
  lon_text_grob <- function(labs_abs) {
    p  <- proj_hammer(rad(labs_abs), rad(0))
    np <- to_npc(-p$x, p$y)
    y  <- np$y - off_lon
    labels <- lon_label_fn(labs_abs)

    edge <- 0.015
    nudge <- 0.006
    i_left  <- np$x <= edge
    i_right <- np$x >= (1 - edge)
    i_mid   <- !(i_left | i_right)

    parts <- list()
    if (any(i_mid)) {
      parts[[length(parts) + 1]] <- grid::textGrob(
        labels[i_mid], x = unit(np$x[i_mid], "npc"), y = unit(y[i_mid], "npc"),
        just = "center", gp = gp_lon
      )
    }
    if (any(i_left)) {
      parts[[length(parts) + 1]] <- grid::textGrob(
        labels[i_left], x = unit(np$x[i_left] + nudge, "npc"), y = unit(y[i_left], "npc"),
        just = "left", gp = gp_lon
      )
    }
    if (any(i_right)) {
      parts[[length(parts) + 1]] <- grid::textGrob(
        labels[i_right], x = unit(np$x[i_right] - nudge, "npc"), y = unit(y[i_right], "npc"),
        just = "right", gp = gp_lon
      )
    }

    if (!length(parts)) ggplot2::zeroGrob() else do.call(grobTree, parts)
  }

  # ----- LONGITUDES -----
  if (is.null(lon_labels_abs)) {
    grob_lons <- ggplot2::zeroGrob()
  } else if (is_waiver(lon_labels_abs)) {
    rel      <- .rel_ticks(major_step, eps)
    labs_abs <- ifelse(rel < 0, -rel, ifelse(rel > 0, 360 - rel, 0))
    grob_lons <- lon_text_grob(labs_abs)
  } else {
    labs_abs <- (lon_labels_abs %% 360)
    grob_lons <- lon_text_grob(labs_abs)
  }

  # ----- LATITUDES -----
  lat_mid  <- lat_labels[abs(lat_labels) < 90]
  lat_pole <- lat_labels[abs(lat_labels) == 90]

  grob_lats_mid <- if (length(lat_mid)) {
    lR <- (.center_deg + 180 - 1e-3) %% 360
    lL <- (.center_deg - 180 + 1e-3) %% 360
    pr <- proj_hammer(rad(lR), rad(lat_mid)); nr <- to_npc(-pr$x, pr$y)
    pl <- proj_hammer(rad(lL), rad(lat_mid)); nl <- to_npc(-pl$x, pl$y)
    oR <- .offset_outside_npc(nr$x, nr$y, off = off_lat)
    oL <- .offset_outside_npc(nl$x, nl$y, off = off_lat)
    grobTree(
      grid::textGrob(fmt_deg_lat(lat_mid), x = unit(oR$x,"npc"), y = unit(oR$y,"npc"),
               just = "left", gp = gp_lat),
      grid::textGrob(fmt_deg_lat(lat_mid), x = unit(oL$x,"npc"), y = unit(oL$y,"npc"),
               just = "right", gp = gp_lat)
    )
  } else ggplot2::zeroGrob()

  grob_poles <- if (length(lat_pole)) {
    pp <- proj_hammer(rad(.center_deg), rad(lat_pole)); np <- to_npc(-pp$x, pp$y)
    o  <- .offset_outside_npc(np$x, np$y, off = off_lat)
    grid::textGrob(fmt_deg_lat(lat_pole), x = unit(o$x,"npc"), y = unit(o$y,"npc"),
             just = "center", gp = gp_lat)
  } else ggplot2::zeroGrob()

  grobTree(grob_lons, grob_lats_mid, grob_poles)
}


# ---- grid assembler (respects theme and waiver/NULL) -------------------------
grob_grid <- function(lat_breaks_major = waiver(), lat_breaks_minor = waiver(),
                      lon_breaks_major_abs = waiver(), lon_breaks_minor_abs = waiver(),
                      eps = 1e-6, default_major = 30, default_minor = 15,
                      theme = ggplot2::theme_get()) {
  el_maj_x <- .calc_el("panel.grid.major.x", theme)
  el_maj_y <- .calc_el("panel.grid.major.y", theme)
  el_min_x <- .calc_el("panel.grid.minor.x", theme)
  el_min_y <- .calc_el("panel.grid.minor.y", theme)
  el_border <- .calc_el("panel.border", theme)

  draw_lon_major <- !is.null(el_maj_x) && !.is_blank(el_maj_x)
  draw_lat_major <- !is.null(el_maj_y) && !.is_blank(el_maj_y)
  draw_lon_minor <- !is.null(el_min_x) && !.is_blank(el_min_x)
  draw_lat_minor <- !is.null(el_min_y) && !.is_blank(el_min_y)

  gp_maj_x <- .line_gp(el_maj_x, default_col = "grey35", default_lwd = 0.9)
  gp_maj_y <- .line_gp(el_maj_y, default_col = "grey35", default_lwd = 0.9)
  gp_min_x <- .line_gp(el_min_x, default_col = "grey55", default_lwd = 0.6)
  gp_min_y <- .line_gp(el_min_y, default_col = "grey55", default_lwd = 0.6)
  gp_outline <- if (!is.null(el_border) && !.is_blank(el_border)) {
    .line_gp(el_border, default_col = "black", default_lwd = 1)
  } else {
    .line_gp(el_maj_x %||% el_maj_y, default_col = "black", default_lwd = 1)
  }

  resolve_lat <- function(breaks, step) {
    if (is.null(breaks)) return(numeric(0))
    if (is_waiver(breaks)) return(.lat_ticks(step))
    breaks
  }
  resolve_lon <- function(breaks, step, exclude = numeric(0)) {
    vals <- if (is.null(breaks)) {
      numeric(0)
    } else if (is_waiver(breaks)) {
      rel <- .rel_ticks(step, eps)
      rel <- .setdiff_near(rel, exclude)
      (rel + .center_deg) %% 360
    } else {
      (breaks %% 360)
    }
    vals[!near_seam(vals)]
  }

  # LAT majors
  lat_labels <- resolve_lat(lat_breaks_major, default_major)
  lat_lines_major <- lat_labels[abs(lat_labels) < 90]

  # LAT minors
  lat_lines_minor <- resolve_lat(lat_breaks_minor, default_minor)
  lat_lines_minor <- .setdiff_near(lat_lines_minor, lat_labels)
  lat_lines_minor <- lat_lines_minor[abs(lat_lines_minor) < 90]

  # LON majors (relative to .center_deg)
  rel_maj <- .rel_ticks(default_major, eps)
  lon_major <- resolve_lon(lon_breaks_major_abs, default_major)

  # LON minors
  lon_minor <- resolve_lon(lon_breaks_minor_abs, default_minor, exclude = rel_maj)

  pars <- list()
  if (draw_lat_minor && length(lat_lines_minor)) pars <- c(pars, lapply(lat_lines_minor, grob_parallel, eps = eps, gp = gp_min_y))
  if (draw_lat_major && length(lat_lines_major)) pars <- c(pars, lapply(lat_lines_major, grob_parallel, eps = eps, gp = gp_maj_y))

  mers <- list()
  if (draw_lon_minor && length(lon_minor)) mers <- c(mers, lapply(lon_minor, grob_meridian, eps = eps, gp = gp_min_x))
  if (draw_lon_major && length(lon_major)) mers <- c(mers, lapply(lon_major, grob_meridian, eps = eps, gp = gp_maj_x))

  grobTree(
    if (length(pars)) do.call(grobTree, pars) else ggplot2::zeroGrob(),
    if (length(mers)) do.call(grobTree, mers) else ggplot2::zeroGrob(),
    grob_outline(eps = eps, gp = gp_outline)
  )
}
