#' @export random_geo_range
#'
#' @title Random geographic occurrences and preliminary conservation status assessment following IUCN Criterion B. Species area of occupancy (AOO) and extent of occurrence (EOO), from latitude and longitude coordinates accounting uncertainty values
#'
#' @description
#' \code{random_geo_range} Given georeferenced coordinates and associated uncertainty. This function generates random statistics values (Extent of Occurrence, Area of Occupancy, number of locations, number of subpopulations) and provide a preliminary conservation status following Criterion B of IUCN.  A graphical map output is also available.

#' @details
#' \strong{Input} as a \code{dataframe} should have the following structure:
#'
#' \tabular{ccccc}{ [,1] \tab ddlat \tab numeric, latitude (in decimal
#' degrees)\cr [,2] \tab ddlon \tab numeric, longitude (in decimal degrees)\cr
#' [,3] \tab ddlat unc \tab numeric, longitude uncertainty (in decimal degrees)\cr
#' [,4] \tab ddlon unc \tab numeric, longitude uncertainty (in decimal degrees)\cr
#' [,5] \tab tax \tab character or factor, taxa names\cr

#' \strong{It is mandatory to respect field positions, but field names do not
#' matter}
#' \strong{Starting position of the raster used for estimating the Area Of Occupancy}\cr
#'
#' Different starting position of the raster used for estimate
#' the AOO may provide different number of occupied cells. Hence, by default, 4
#' different translations of the raster is done (fixed increment of 1/4
#' resolution north and east) and the minimum number of occupied cells is used
#' for estimating AOO. It is also possible to define a given number of random
#' starting position of the raster using the argument
#' \code{nbe.rep.rast.AOO}\cr
#'
#' \strong{Estimating number of locations}\cr
#'
#' Locations are estimated by
#' overlaying a grid of a given resolution (see \code{Cell_size_locations} for
#' specifying the resolution). The number of locations is simply the number of
#' occupied locations. Note that the grid position is overlaid in order to
#' minimize the number of locations (several translation of the grid are
#' performed and the one providing the minimum number of occupied cells is
#' provided).
#'
#' \strong{Taking into account protected area for estimating the number of
#' locations}\cr A location is defined by the IUCN as a "geographically or
#' ecologically distinct area in which a single threatening event can affect
#' all individuals of the taxon". A simple way to include threat level is to
#' rely on a map of protected areas and assume that populations within and
#' 
#' outside protected areas are under different threat level.\cr
#'
#' If a map of protected area is provided, this one is used for estimating the
#' number of locations by the following procedure:\cr - if
#' \code{method_protected_area} is "no_more_than_one", all occurrences within a
#' given protected area will be considered as one location. Occurrences outside
#' protected area will be used for estimating the number of locations using
#' overlaying grid as described above. See the vignette for illustration. \cr -
#' if \code{method_protected_area} is NOT "no_more_than_one", number of
#' locations will be estimated by the overlaying grid as described above, but
#' by considering differently occurrences outside and inside protected area. \cr
#'
#' The protected areas layers should be given as as
#' \code{SpatialPolygonsocc_randomFrame} in \code{protec.areas}. The
#' \code{ID_shape_PA} should also be given and should represent the unique ID
#' of each protected area in the provided shapefile. This can be checked by the
#' following code:
#'
#' \code{colnames(ProtectedAreas@data)} Where ProtectedAreas is the name of
#' your shapefile.
#'
#' \strong{Limitation in the estimations of EOO}\cr
#'
#' For a species whose occurrences span more than 180 degrees, EOO is not
#' computed. This is the case for example for species whose distribution span
#' the 180th meridian.
#'
#'
#' @param occs_df a \code{dataframe} georeferenced occurrence
#' @param n_length Number of iterations
#' @param lat_col latitude values decimal degrees column 
#' @param lon_col longitude decimal degrees column 
#' @param lat_uncertainty latitude uncertainty decimal degrees column 
#' @param lon_uncertainty longitude uncertainty decimal degrees column 
#' @param taxa_col character or factor, taxa names
#' @param country_map a \code{SpatialPolygonsDataFrame} or
#' \code{SpatialPolygons} showing for example countries or continent borders.
#' This shapefile will be used for cropping the \code{SpatialPolygons} used for
#' EOO computation if \code{exclude.area} is TRUE. By default, it is
#' \code{land}
#' @param exclude.area a logical, if TRUE, areas outside of \code{country_map}
#' are cropped of \code{SpatialPolygons} used for EOO computation. By default,
#' it is TRUE
#' @param method.range a character string, if "convex.hull", EOO is based on a
#' convex hull.  if "alpha.hull", EOO is based on alpha hull of \code{alpha}
#' value. By default, it is "convex.hull"
#' @param export_shp a logical, if TRUE, shapefiles of \code{SpatialPolygons}
#' used for EOO computation are exported. By default, it is FALSE
#' @param write_shp a logical, if TRUE, shapefiles of \code{SpatialPolygons}
#' used for EOO computation are written as ESRI shapefiles in a sub-directory
#' in the working directory. By default, it is FALSE
#' @param map_pdf a logical, if TRUE, maps are exported in one pdf file.
#' Otherwise, each species map is exported in png. By default, it is FALSE
#' @param draw.poly.EOO a logical, if TRUE, the polygon used for estimating EOO
#' is drawn. By default, it is TRUE
#' @param protec.areas a \code{SpatialPolygonsDataFrame}, shapefile with
#' protected areas. If provided, this will be taken into account for
#' calculating number of location (see Details and
#' \code{method_protected_area}).  By default, it is the World Database on Protected 
#' Areas \code{WDPA}
#' @param method_protected_area a character string. By default is
#' "no_more_than_one"", which means occurrences within protected areas (if
#' provided) will not be taken into account for estimating the number of
#' locations following the grid system, see Details. By default, it is
#' "no_more_than_one"
#' @param ID_shape_PA a character string, indicating the field name of
#' \code{protec.areas} with ID of the \code{SpatialPolygonsDataFrame} of
#' protected areas
#' @param Cell_size_AOO a numeric, value indicating the grid size in kilometers
#' used for estimating Area of Occupancy.  By default, equal to 2
#' @param Cell_size_locations a numeric, value indicating the grid size in
#' kilometers used for estimating the number of location. By default, equal to
#' 10
#' @param DrawMap a logical, if TRUE a map is produced for each species in png
#' format, unless map_pdf is TRUE. By default, it is TRUE
#' @param add.legend a logical, if TRUE a legend and a submap showing
#' distribution in 'country_map' are displayed for each map. By default, it is
#' TRUE
#' @param write_results a logical, if TRUE, results are exported in a file
#' which can csv or excel, see write_file_option. By default, it is FALSE
#' @param write_file_option a character, if "excel", results are exported in
#' excel file, if "csv", results are exported in csv. By default, it is "excel"

random_geo_range <-
  function(n_length,
           occs_df,
           lat_col = "latitude",
           lon_col = "longitude",
           lat_uncertainty = "lat_uncertainty",
           lon_uncertainty = "lon_uncertainty",
           taxa_col = "species",
           country_map = NULL,
           exclude.area = TRUE,
           method.range = "convex.hull",
           export_shp = FALSE,
           write_shp = FALSE,
           map_pdf = FALSE, 
           draw.poly.EOO = TRUE,
           protec.areas = NULL,
           method_protected_area = "no_more_than_one",
           ID_shape_PA = NULL, 
           Cell_size_AOO = 2,
           Cell_size_locations = 10,
           DrawMap = TRUE,
           add.legend = TRUE, 
           write_results = FALSE,
           write_file_option = "excel",
           ...)
  {
    rand_EOOs = c()
    rand_AOOs = c()
    rand_CritB = c()
    rand_Nbe_loc = c()
    rand_Category_AOO = c()
    rand_Category_EOO = c()
    rand_Cateogry_code = c()
    
    for (i in 1:n_length) {
      occ_random <- generate_occ_uncertain(
        occs_df,
        lat_col = lat_col,
        lon_col = lon_col,
        lat_uncertainty = lat_uncertainty,
        lon_uncertainty = lon_uncertainty,
        taxa_col = taxa_col
        
      )
      
      #IUCN
      observed.IUCN <-
        IUCN.eval(
          occ_random,
          country_map = country_map,
          exclude.area = exclude.area,
          method.range = method.range,
          export_shp = export_shp,
          write_shp = write_shp,
          map_pdf = map_pdf,
          draw.poly.EOO = draw.poly.EOO,
          #protec.areas = protect.areas,
          method_protected_area = method_protected_area,
          ID_shape_PA = ID_shape_PA,
          Cell_size_AOO = Cell_size_AOO,
          Cell_size_locations = Cell_size_locations,
          DrawMap = DrawMap,
          add.legend = add.legend,
          write_results = write_results,
          write_file_option = write_file_option
        )
   
      
       if (DrawMap) {
                  file.rename(from = "results.png", to = paste0("results", i,".png"))
       }
      
      # Add new EOO to rand_EOOs
      EOO_temp <- observed.IUCN$EOO
      # Add new EOO value to rand_EOOs
      rand_EOOs <- c(rand_EOOs, EOO_temp)
      
      # Add new AOO to rand_AOOs
      AOO_temp <- observed.IUCN$AOO
      # Add new AOO value to rand_AOOs
      rand_AOOs <- c(rand_AOOs, AOO_temp)
      
      #Add new value to CritB
      rand_CritB <- c(rand_CritB, observed.IUCN$Category_CriteriaB)
      
      #Add new value to Nbe_Loc
      rand_Nbe_loc = c(rand_Nbe_loc, observed.IUCN$Nbe_loc)
      
      #Add AOO Category
      rand_Category_AOO = c(rand_Category_AOO, observed.IUCN$Category_AOO)
      
      #Add EOO Category
      rand_Category_EOO = c(rand_Category_EOO, observed.IUCN$Category_EOO)
      
      #Add code for Category
      rand_Cateogry_code = c(rand_Category_code, observed.IUCN$Category_code)
      
    }
  
    return (data.frame(
      EOO = rand_EOOs,
      AOO = rand_AOOs,
      Cat_CritB = rand_CritB,
      Category_code = rand_Cateogry_code,
      Category_AOO = rand_Category_AOO,
      Category_EOO = rand_Category_EOO,
      Num_Loc = rand_Nbe_loc,
      
    ))
  }
    
  
