#' ARTSS1-5 X-ray Source Catalog
#'
#'
#' @format data.frame
#' \describe{
#'   \item{name}{Source name.}
#'   \item{raj}{Right ascension in J2000, degrees.}
#'   \item{de}{Declination in J2000, degrees.}
#'   \item{e_pos}{Position uncertainty, arcseconds.}
#'   \item{flux}{Flux in `erg/cm^2/s`.}
#'   \item{e_flux}{Flux uncertainty in `erg/cm^2/s`.}
#'   \item{c_name}{Cross-identification name.}
#'   \item{type}{Source type.}
#'   \item{z}{Redshift.}
#'   \item{l}{Galactic longitude, degrees.}
#'   \item{b}{Galactic latitude, degrees.}
#' }
#'
#' @source \url{https://cdsarc.cds.unistra.fr/viz-bin/cat/J/A+A/687/A183}
#'   (bibcode: 2024A&A...687A.183S)
"artss15"


#' Celestial equator is the great circle with declination equal to 0 deg.
#'
#'
#' @format data.frame
#' \describe{
#'   \item{l}{Galactic longitude, degrees.}
#'   \item{b}{Galactic latitude, degrees.}
#'   \item{ra}{Right ascension, degrees.}
#' }
#'
#' @source \url{https://en.wikipedia.org/wiki/Celestial_equator}
"equator"
