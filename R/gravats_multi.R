#' @name gravats_multi
#' 
#' @description Multifactorial statistics on engravings only.
#'
#' @param gravats Path to the file of engraved rocks.
#' @param stats Which stats will be performed.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list of ggplots
#'
#' @examples
#' 
#' 
#' @export
gravats_multi <- function(gravats = NA,
                          gravats.fields = c("roche", "thm", "tec", "lat", "thm_xt"),
                          stats = c("mult_ca"),
                          patts = c("estrella", "arboriforme", "pentacle", "zig−zag",
                                    "dona", "home", "personatge",
                                    "ballesta", "llança", "espasa", "arc",
                                    # "fletxa",
                                    "casc/barret/corona",
                                    "cornamusa",
                                    "ocell", "cavall|equid"),
                          verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  thm_xt <- subset(gravats, select = gravats.fields)
  # library(FactoMineR)
  # library("factoextra")
  # main themes
  lg <- list()
  if("mult_ca" %in% stats){
    if(verbose){
      print(paste0("CA on engravings main themes"))
    }
    df.patts <- thm_xt[0,]
    for (patt in patts){
      # patt <- "estrella"
      if(verbose){
        print(paste0(" .. read: ", patt))
      }
      thm_xt.thm <- thm_xt[grep(patt, thm_xt[,"thm_xt"]),]
      if(nrow(thm_xt.thm) > 0 ){
        thm_xt.thm$thm_xt <- patt
        df.patts <- rbind(df.patts, thm_xt.thm)
      } else {
        if(verbose){
          print(paste0(" .. .... /!\ no data for: ", patt))
        }
      }
    }
    df.afc <- table(df.patts$roche, df.patts$thm_xt)
    res.ca <- FactoMineR::CA(df.afc, graph = FALSE)
    gg.ca <- factoextra::fviz_ca_biplot(res.ca, repel = TRUE, labelsize = 2)
    lg[['mult_ca']] <- gg.ca
  }
  return(lg)
}
