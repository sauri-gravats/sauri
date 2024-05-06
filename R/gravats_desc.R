#' @name gravats_desc
#' 
#' @description Descriptive statistics on engravings only.
#'
#' @param roques A dataframe of engraved rocks.
#' @param gravats Path to the file of engraved rocks.
#' @param lthms List of selected themes. Only useful if "spats_tema" is in `stats`.
#' @param stats Which stats will be performed.
#' @param lg If a list is provided, will output results in this list, if not, will create a list from scratch.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list of ggplots
#'
#' @examples
#' 
#' 
#' @export
gravats_desc <- function(gravats = NA,
                         gravats.fields = c("roche", "thm", "tec", "lat", "thm_xt"),
                         gravats.main.thm = c("personatge", "zoomorf", "literal", "tècnic", "geomètric", "irreconeixible"),
                         stats = c("tec"),
                         lg = NA,
                         verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  thm_xt <- subset(gravats, select = gravats.fields)
  if(is.na(lg)){
    if(verbose){
      print(paste0("Overwrites/Creates the outut `lg` list"))
    }
    lg <- list()
  }
  if("thm" %in% stats){
    if(verbose){
      print(paste0("Themes of engraving"))
    }
    thm_xt$thm_xt <- stringr::str_trim(thm_xt$thm_xt)
    result <- thm_xt %>%
      tidyr::separate_rows(thm_xt, sep = "\\+") %>%
      dplyr::mutate(thm_xt = trimws(tolower(thm_xt))) %>%
      dplyr::filter(thm_xt != "") %>%
      dplyr::count(roche, thm_xt) %>%
      tidyr::pivot_wider(names_from = thm_xt, values_from = n, values_fill = list(n = 0)) %>%
      dplyr::select(roche, sort(names(.[-1]))) %>%
      # # TODO: deal with the main themes
      # dplyr::select(-all_of(gravats.main.thm)) %>%
      tibble::column_to_rownames('roche')
    lg[['grav_thm']] <- result
  }
  if("tec" %in% stats){
    if(verbose){
      print(paste0("Technics of engraving"))
    }
    tec.nb.t <- sum(stringr::str_detect(thm_xt$tec, "t"), na.rm = TRUE)
    tec.nb.g <- sum(stringr::str_detect(thm_xt$tec, "g"), na.rm = TRUE)
    tec.nb.s <- sum(stringr::str_detect(thm_xt$tec, "s"), na.rm = TRUE)
    tec.nb.n <- sum(stringr::str_detect(thm_xt$tec, "n"), na.rm = TRUE)
    tec.nb.p <- sum(stringr::str_detect(thm_xt$tec, "p"), na.rm = TRUE)
    tec.nb.patinaclara <- sum(stringr::str_detect(thm_xt$tec, "%"), na.rm = TRUE)
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
                     panel.border = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank())
    lg[['grav_tec']] <- gg_tek
  }
  return(lg)
}
