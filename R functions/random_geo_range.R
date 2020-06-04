#' @export random_geo_range
#'
#' @title Generate random geographic range of species area of occupancy (AOO) and extent of occurrence (EOO), from latitude and longitude coordinates accounting uncertainty values
#'
#' @description
#' \code{random_geo_range} Generate random geographic range of AOO and EOO, from latitude and longitude coordinates accounting uncertainty values. The use case this function was developed for was to generate several AOO and EOO.
#'
#' @param occs_df A \code{data.frame} of occurrence locations that includes
#'   \emph{at least these five columns} - length of loops to be run. latitude, longitude, latitude uncertainty and longitude uncertainty in degrees.
#' @param n_length Number of loops to be run.
#' @param lat_col Name of column of latitude values. Caps sensitive.
#' @param lon_col Name of column of longitude values. Caps sensitive.
#' @param lat_uncertainty Name of column of latitude uncertainty in degree values. Caps sensitive.
#' @param lon_uncertainty Name of column of longitude uncertainty in degree values. Caps sensitive.
#' @param lon_random latitude random deviates of the interval from min to max. Caps sensitive.
#' @param lon_random longitude random deviates of the interval from min to max. Caps sensitive.
#' @param taxa_col Name of column of taxa (species) values. Caps sensitive.
#'

random_geo_range <- function(n_length, occs_df, lat_col = "latitude", lon_col = "longitude",
                             lat_uncertainty = "lat_uncertainty",
                             lon_uncertainty = "lon_uncertainty",
                             taxa_col = "species",...)
{
  rand_EOOs = c()
  rand_AOOs = c()

  for (i in 1:n_length) {
    set.seed(i)
    occ_random <- generate_occ_uncertain(occs_df, 
      lat_col = lat_col, 
      lon_col = lon_col,
      lat_uncertainty = lat_uncertainty,
      lon_uncertainty = lon_uncertainty,
      taxa_col = taxa_col)
    
    
      # Calculate EOO
      EOO_temp <- EOO.computing(occ_random, exclude.area = T, country_map = land)$EOO
      # Add new EOO value to rand_EOOs
      rand_EOOs <- c(rand_EOOs, EOO_temp)

      # Calculate AOO
      AOO_temp <- AOO.computing(occ_random)
      # Add new AOO value to rand_AOOs
      rand_AOOs <- c(rand_AOOs, AOO_temp)
    

  }
  return (data.frame(rand_EOO = rand_EOOs, rand_AOO = rand_AOOs))
}




