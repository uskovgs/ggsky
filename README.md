

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->


# ggsky
`ggsky` is an extension for `ggplot2` to draw sky maps with:

- galactic coordinates (`coord_galactic()`)
- equatorial coordinates (`coord_equatorial()`)

It includes custom coordinate systems and helper scales for longitude/latitude labels.

## Installation

From GitHub:

```r
remotes::install_github("uskovgs/ggsky")
```

## Quick examples

```r
  library(ggplot2)
  library(ggsky)

  N <- 100
  df1 <- data.frame(
    x = runif(N, 0, 360),
    y = runif(N, -90, 90)
  )

  ggplot(df1, aes(x, y)) +
    geom_point() +
    coord_galactic() +
    labs(title = "Galactic coordinate system")

  ggplot(df1, aes(x, y)) +
    geom_point() +
    labs(title = "Equatorial coordinate system") +
    coord_equatorial()
```
