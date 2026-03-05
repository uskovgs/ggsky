# ggsky

`ggsky` is an extension for `ggplot2` to draw sky maps with:

- galactic coordinates
  ([`coord_galactic()`](https://uskovgs.github.io/ggsky/reference/coord_galactic.md))
- equatorial coordinates
  ([`coord_equatorial()`](https://uskovgs.github.io/ggsky/reference/coord_equatorial.md))

It includes custom coordinate systems and helper scales for
longitude/latitude labels.

## Installation

From GitHub:

``` r
devtools::install_github("uskovgs/ggsky")
```

## Quick examples

``` r
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
