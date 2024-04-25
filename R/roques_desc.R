#' @name roques_desc
#' 
#' @description Dsecriptive statistics on engraved rocks only.
#'
#' @param roques A sf dataframe of engraved rocks.
#' @param roques.id The field name of the engraved rocks ID. Default "roca".
#' @param stats Which stats will be performed.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list of ggplots and leaflet
#'
#' @examples
#' 
#' 
#' @export
roques_desc <- function(roques = NA,
                        roques.id = "roca",
                        stats = c("map_leaflet", "list_ordered"),
                        verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  lg <- list()
  if("list_ordered" %in% stats){
    if(verbose){
      print(paste0("Ordered list"))
    }
    # Example vector
    # vector <- unique(gravats$roche)
    vector <- unique(roques[[roques.id]])
    data <- data.frame(
      original = vector,
      numeric_part = as.numeric(gsub("[^0-9]", "", vector)),  # Remove non-numeric characters
      stringsAsFactors = FALSE
    )
    sorted_data <- data %>%
      dplyr::arrange(numeric_part, original)
    sorted_rocks <- sorted_data$original
    lg[['list_ordered']] <- sorted_rocks
  }
  if("map_leaflet" %in% stats){
    if(verbose){
      print(paste0("Leaflet map"))
    }
    gleaflet <- leaflet::leaflet(roques, height = "300px", width = "75%") %>%
      leaflet::addWMSTiles(
        # "http://geoserveis.icgc.cat/icc_ortohistorica/wms/service?",
        "http://www.ign.es/wms-inspire/pnoa-ma?",
        layers = "OI.OrthoimageCoverage",
        options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "") %>% 
      # addTiles() %>%  # Add default OpenStreetMap map tiles
      # all rocks
      leaflet::addCircleMarkers(popup = roques[[roques.id]],
                                radius = 1,
                                opacity = 0.3)
    lg[['map_leaflet']] <- gleaflet
  }
  return(lg)
}
