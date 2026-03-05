# shortest longitude difference in [-180, 180]
.short_dlon <- function(l2, l1) {
  d <- ((l2 - l1 + 540) %% 360) - 180
  ifelse(d == -180, 180, d)
}

.unit_xyz <- function(lon_deg, lat_deg) {
  lam <- rad(lon_deg)
  phi <- rad(lat_deg)
  c(cos(phi) * cos(lam), cos(phi) * sin(lam), sin(phi))
}

.angular_sep_deg <- function(v1, v2) {
  acos(max(-1, min(1, sum(v1 * v2)))) * 180 / pi
}

.slerp_lonlat <- function(l1, b1, l2, b2, n) {
  if (n <= 2) return(list(lon = c(l1, l2), lat = c(b1, b2)))

  v1 <- .unit_xyz(l1, b1)
  v2 <- .unit_xyz(l2, b2)
  omega <- acos(max(-1, min(1, sum(v1 * v2))))
  if (omega < 1e-12) return(list(lon = rep(l1, n), lat = rep(b1, n)))

  t <- seq(0, 1, length.out = n)
  so <- sin(omega)

  if (abs(so) < 1e-10) {
    # Nearly antipodal: stable fallback via normalized linear interpolation.
    m <- vapply(t, function(tt) (1 - tt) * v1 + tt * v2, numeric(3))
    den <- sqrt(colSums(m * m))
    m <- sweep(m, 2, den, "/")
    x <- m[1, ]; y <- m[2, ]; z <- m[3, ]
  } else {
    A <- sin((1 - t) * omega) / so
    B <- sin(t * omega) / so
    x <- A * v1[1] + B * v2[1]
    y <- A * v1[2] + B * v2[2]
    z <- A * v1[3] + B * v2[3]
  }

  lon <- (atan2(y, x) * 180 / pi + 360) %% 360
  lat <- asin(pmax(-1, pmin(1, z))) * 180 / pi
  list(lon = lon, lat = lat)
}

# split a path at the seam (l = 180°): insert NA at seam crossings
.split_on_seam <- function(lon, lat, eps = 1e-6) {
  nudge_part <- function(lon_part) {
    if (length(lon_part) < 2) return(lon_part)
    if (near_seam(lon_part[1], tol = eps * 10)) {
      d <- .short_dlon(lon_part[2], lon_part[1])
      lon_part[1] <- (180 + sign(ifelse(d == 0, 1, d)) * eps + 360) %% 360
    }
    m <- length(lon_part)
    if (near_seam(lon_part[m], tol = eps * 10)) {
      d <- .short_dlon(lon_part[m], lon_part[m - 1])
      lon_part[m] <- (180 + sign(ifelse(d == 0, 1, d)) * eps + 360) %% 360
    }
    lon_part
  }

  side <- lon < (180 - eps)
  cross <- which(side[-1] != side[-length(side)])
  if (!length(cross)) return(list(list(lon = lon, lat = lat)))
  out <- list()
  start <- 1
  for (k in cross) {
    lon_part <- nudge_part(lon[start:k])
    out[[length(out) + 1]] <- list(lon = lon_part, lat = lat[start:k])
    start <- k + 1
  }
  lon_part <- nudge_part(lon[start:length(lon)])
  out[[length(out) + 1]] <- list(lon = lon_part, lat = lat[start:length(lat)])
  out
}

# Muncher for paths/polygons: great-circle segmentation.
.gal_segmentize <- function(data, max_deg = 1) {
  if (!nrow(data)) return(data)
  if (is.null(data$group)) data$group <- 1L

  pieces <- lapply(split(data, data$group), function(g) {
    g <- g[seq_len(nrow(g)), , drop = FALSE]
    out <- list()

    for (i in seq_len(nrow(g) - 1)) {
      l1 <- as.numeric(g$x[i]); l2 <- as.numeric(g$x[i + 1])
      b1 <- as.numeric(g$y[i]); b2 <- as.numeric(g$y[i + 1])
      if (is.na(l1) || is.na(l2) || is.na(b1) || is.na(b2)) {
        out[[length(out) + 1]] <- data.frame(x = NA, y = NA, group = g$group[1])
        next
      }

      omega_deg <- .angular_sep_deg(.unit_xyz(l1, b1), .unit_xyz(l2, b2))
      n <- max(2, ceiling(omega_deg / max_deg) + 1)
      gc <- .slerp_lonlat(l1, b1, l2, b2, n)
      lon <- gc$lon
      lat <- gc$lat

      parts <- .split_on_seam(lon, lat)      # clean seam split
      for (j in seq_along(parts)) {
        p <- parts[[j]]
        out[[length(out) + 1]] <- data.frame(x = p$lon, y = p$lat, group = g$group[1])
        if (j < length(parts)) {
          out[[length(out) + 1]] <- data.frame(x = NA, y = NA, group = g$group[1])
        }
      }
    }

    do.call(rbind, out)
  })

  do.call(rbind, pieces)
}

# Segment layer data for GeomPath while preserving aesthetics.
.gal_segmentize_layer <- function(data, max_deg = 1) {
  if (!nrow(data)) return(data)
  if (is.null(data$group)) data$group <- 1L
  if (!("PANEL" %in% names(data))) data$PANEL <- 1L

  groups <- split(data, interaction(data$PANEL, data$group, drop = TRUE), drop = TRUE)
  out <- list()
  next_gid <- 1L

  for (g in groups) {
    g <- g[seq_len(nrow(g)), , drop = FALSE]
    if (nrow(g) < 2) next

    for (i in seq_len(nrow(g) - 1)) {
      l1 <- as.numeric(g$x[i]); l2 <- as.numeric(g$x[i + 1])
      b1 <- as.numeric(g$y[i]); b2 <- as.numeric(g$y[i + 1])
      if (is.na(l1) || is.na(l2) || is.na(b1) || is.na(b2)) next

      omega_deg <- .angular_sep_deg(.unit_xyz(l1, b1), .unit_xyz(l2, b2))
      n <- max(2, ceiling(omega_deg / max_deg) + 1)
      gc <- .slerp_lonlat(l1, b1, l2, b2, n)
      parts <- .split_on_seam(gc$lon, gc$lat)

      for (part in parts) {
        row_tpl <- g[rep(i, length(part$lon)), , drop = FALSE]
        row_tpl$x <- part$lon
        row_tpl$y <- part$lat
        row_tpl$group <- next_gid
        out[[length(out) + 1]] <- row_tpl
        next_gid <- next_gid + 1L
      }
    }
  }

  if (!length(out)) return(data[0, , drop = FALSE])
  do.call(rbind, out)
}
