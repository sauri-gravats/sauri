#' @name gravats_x
#' 
#' @description Statistics on engravings only.
#'
#' @param roques A dataframe of engraved rocks.
#' @param gravats Path to the file of engraved rocks.
#' @param lthms List of selected themes. Only useful if "spats_tema" is in `stats`.
#' @param stats Which stats will be performed.
#' @param seriated If TRUE, seriate the distribution
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list of ggplots
#'
#' @examples
#' 
#' 
#' @export
gravats_x <- function(gravats = NA,
                      gravats.fields = c("roche", "thm", "tec", "lat", "thm_xt"),
                      stats = c("tec"),
                      verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  thm_xt <- subset(gravats, select = gravats.fields)
  lg <- list()
  if("tec" %in% stats){
    if(verbose){
      print(paste0("Technic of engraving"))
    }
    tec.nb.t <- sum(str_detect(thm_xt$tec, "t"), na.rm = TRUE)
    tec.nb.g <- sum(str_detect(thm_xt$tec, "g"), na.rm = TRUE)
    tec.nb.s <- sum(str_detect(thm_xt$tec, "s"), na.rm = TRUE)
    tec.nb.n <- sum(str_detect(thm_xt$tec, "n"), na.rm = TRUE)
    tec.nb.p <- sum(str_detect(thm_xt$tec, "p"), na.rm = TRUE)
    tec.nb.patinaclara <- sum(str_detect(thm_xt$tec, "%"), na.rm = TRUE)
    df.tec <- data.frame("incisio fina" = tec.nb.t,
                         "incisio gruixuda" = tec.nb.g,
                         "incisió raspada"  = tec.nb.s,
                         "incisió 'V' profunda"  = tec.nb.n,
                         "incisió repicat"  = tec.nb.p,
                         check.names=FALSE)
    df.tec.t <- as.data.frame(t(df.tec))
    colnames(df.tec.t)[1] <- "n"
    df.tec.t$tecnica <- rownames(df.tec.t)
    gg_tek <- ggplot2::ggplot(df.tec.t, ggplot2::aes(x = tecnica, y = n, fill = tecnica)) + 
      ggplot2::geom_bar(stat = "identity") +
      ggrepel::geom_label_repel(ggplot2::aes(label = n, 
                                             y = n +.5)) +
      ggplot2::coord_polar() + 
      ggplot2::theme(legend.position="none",
                     panel.border = element_blank(),
                     panel.background = element_blank())
    lg[['grav_tec']] <- gg_tek
  }
  return(lg)
}
