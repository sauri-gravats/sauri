#' @name roques_read
#' 
#' @description Read a dataset of engraved rocks.
#'
#' @param roques Path to the file of engraved rocks
#' @param gpkg.layer The GeoPackage layer name
#' @param to.wgs84 convert to WGS84
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return a sf dataframe of points
#'
#' @examples
#' 
#' 
#' @export
roques_read <- function(roques = "C:/Rprojects/sauri/data/240413/Gravats_Sauri.gpkg",
                        gpkg.layer = "2019_2020— Roques_tot_zona_det",
                        to.wgs84 = TRUE,
                        verbose = TRUE){
  # read a geopackage (gpkg) of engraved rocks
  if(DescTools::SplitPath(roques)$extension == "gpkg"){
    if(verbose){
      print(paste0("Read a GeoPackage"))
    }
    roques.sf <- sf::st_read(roques, layer = gpkg.layer)
  }
  if(to.wgs84){
    if(verbose){
      print(paste0("Convert to WGS84"))
    }
    roques.sf <- st_transform(roques.sf, 4326)
  }
  return(roques.sf)
}

