#' @name roques_x
#' 
#' @description Statistics on engraved rocks only.
#'
#' @param roques A sf dataframe of engraved rocks.
#' @param stats Which stats will be performed.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list of ggplots and leaflet
#'
#' @examples
#' 
#' 
#' @export
roques_x <- function(roques = NA,
                     stats = c("map_leaflet"),
                     verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  lg <- list()
  if("map_leaflet" %in% stats){
    if(verbose){
      print(paste0("Leaflet map"))
    }
    gleaflet <- leaflet::leaflet(roques, height = "300px", width = "75%") %>%
      leaflet::addWMSTiles(
        # "http://geoserveis.icgc.cat/icc_ortohistorica/wms/service?",
        "http://www.ign.es/wms-inspire/pnoa-ma?",
        layers = "OI.OrthoimageCoverage",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "") %>% 
      # addTiles() %>%  # Add default OpenStreetMap map tiles
      # all rocks
      leaflet::addCircleMarkers(popup = "engraved<br>rock",
                       radius = 1,
                       opacity = 0.3)
    lg[['map_leaflet']] <- gleaflet
  }
  return(lg)
}
