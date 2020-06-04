#' @export generate_occ_uncertain
#'
#' @title Generate random latitude and longitude coordinates accounting uncertainty values
#'
#' @description
#' \code{generate_occ_uncertain} Generate random latitude and longitude coordinates with uncertain values. The use case this function was developed for was to generate occurence point uncertainty coordinates.
#'
#' @param occs_df A \code{data.frame} of occurrence locations that includes
#'   \emph{at least these four columns} - latitude, longitude, latitude uncertainty and longitude uncertainty in degrees.
#' @param lat_col Name of column of latitude dbl values. Caps sensitive.
#' @param lon_col Name of column of longitude dbl values. Caps sensitive.
#' @param lat_uncertainty Name of column of latitude uncertainty in degree values. Caps sensitive.
#' @param lon_uncertainty Name of column of longitude uncertainty in degree values. Caps sensitive.
#' @param lon_random latitude random deviates of the interval from min to max. Caps sensitive.
#' @param lon_random longitude random deviates of the interval from min to max. Caps sensitive.
#' @param taxa_col Name of column of taxa (species) values. Caps sensitive.
#'
generate_occ_uncertain <- function(occs_df, lat_col = "latitude",
                                   lat_uncertainty = "latitude_uncertainty", lon_col = "longitude",
                                   lon_uncertainty = "longitude_uncertainty",
                                   taxa_col = "species"){
  lat <- occs_df[[lat_col]]
  lon <- occs_df[[lon_col]]
  lat_uncertainty <- occs_df[[lat_uncertainty]]
  lon_uncertainty <- occs_df[[lon_uncertainty]]

  lat_random <- runif(n = length(lat),
                      min = lat - lat_uncertainty, max = lat +
                        lat_uncertainty)
  lon_random <- runif(n = length(lon),
                      min = lon - lon_uncertainty, max = lon +
                        lon_uncertainty)
  random_dd <- data.frame(lat_random = lat_random, lon_random = lon_random, tax = occs_df[[taxa_col]] )
  return(random_dd)
}
