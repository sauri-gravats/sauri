#' @name roques_x_gravats
#' 
#' @description Statistics on the engraving distributions by engraved rocks: numbers, altitudes, etc.
#'
#' @param roques A dataframe of engraved rocks.
#' @param gravats Path to the file of engraved rocks.
#' @param lthms List of selected themes. Only useful if "spats_tema" is in `stats`.
#' @param stats Which stats will be performed.
#' @param seriated If TRUE, seriate the distribution
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A lis of ggplots
#'
#' @examples
#' 
#' 
#' @export
roques_x_gravats <- function(roques = NA,
                             gravats = NA,
                             roques.fields = c("geom", "z", "zona"),
                             gravats.fields = c("roche", "thm", "tec", "lat", "thm_xt"),
                             lthms = NA,
                             stats = c("ngrav", "altis_ngrav", "altis_tema", "spats_tema"),
                             seriated = TRUE,
                             verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  if("altis_ngrav" %in% stats | "altis_tema" %in% stats){
    stats <- c(stats, "altis")
  }
  roques_x_gravats.fields <- c(gravats.fields, roques.fields)
  thm_xt <- subset(gravats, select = gravats.fields)
  thm_xt <- merge(thm_xt, roques, by = "roche", all.x = T)
  thm_xt <- subset(thm_xt, select = roques_x_gravats.fields)
  lg <- list()
  if("ngrav" %in% stats){
    if(verbose){
      print(paste0("Distribution nb of engravings by engraved rocks"))
    }
    # merge engravings and rocks
    # thm_xt <- merge(thm_xt, roques, by = "roche", all.x = T)
    # thm_xt <- subset(thm_xt, select = roques_x_gravats.fields)
    nb.grav.roche <- thm_xt %>% 
      dplyr::count(roche)
    # statistical indices + labels
    mean.gr <- mean(nb.grav.roche$n)
    mean.label <- paste0("mitjana: ", round(mean.gr, 1))
    median.gr <- median(nb.grav.roche$n)
    median.label <- paste0("mediana: ", median.gr)
    mode.gr <- as.integer(names(sort(-table(nb.grav.roche$n)))[1])
    modo.label <- paste0("modo: ", mode.gr)
    g.dist.nbgrav <- ggplot2::ggplot(nb.grav.roche, ggplot2::aes(n)) +
      ggplot2::ggtitle("Número de grabados por roca - media, mediana, moda") +
      ggplot2::geom_freqpoly(binwidth = 1)+
      ggplot2::geom_vline(xintercept = mode.gr, linetype = "dotted",
                          color = "lightgrey", size=.5) +
      ggplot2::annotate("text", x = mode.gr, y = 40, label = modo.label, angle = 90, size = 3, color = "darkgrey") +
      ggplot2::geom_vline(xintercept = mean.gr, 
                          color = "lightgrey", size = .5) +
      ggplot2::annotate("text", x = mean.gr, y = 40, label = mean.label, angle = 90, size = 3, color = "darkgrey") +
      ggplot2::geom_vline(xintercept = median.gr, linetype = "dashed", 
                          color = "lightgrey", size = .5) +
      ggplot2::annotate("text", x = median.gr, y = 40, label= median.label, angle = 90, size = 3, color = "darkgrey") +
      ggplot2::xlab("nb gravats") + ylab("nb roques gravades") +
      ggplot2::scale_x_continuous(breaks = c(1, 5, 10, 20, 30, 50, 75, 100))+
      ggplot2::xlim(0.9, 100)+
      ggplot2::theme_bw()
    lg[['ngrav']] <- g.dist.nbgrav
  }
  if("altis" %in% stats){
    ## altitudes histograms
    # zones
    altis.zonas <- thm_xt %>% 
      dplyr::group_by(zona) %>% 
      dplyr::mutate(alt.min = min(z),
                    alt.max  = max(z),
                    alt.moy = mean(z))
    altis.zonas <- subset(altis.zonas, select = c("zona", "alt.min", "alt.max", "alt.moy"))
    altis.zonas <- altis.zonas[!duplicated(altis.zonas), ]
    # remove zones without altitudes
    altis.zonas <- altis.zonas[!is.na(altis.zonas$zona), ]
    # rocks
    thm_xt_sp <- sf::st_as_sf(thm_xt) # -> sf
    # remove rocks without altitudes
    thm_xt_sp <- thm_xt_sp[!is.na(thm_xt_sp$z), ]
    mycolo <- c("red", "blue", "pink", "yellow", "green")
    # regroupements
    thm_xt_sp <- thm_xt_sp[order(thm_xt_sp$thm_xt),] 
    # min.mean.sd.max <- function(x) {
    #   r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
    #   names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    #   r
    # }
    if("altis_ngrav" %in% stats){
      if(verbose){
        print(paste0("Distribution nb of engravings by altitudes"))
      }
      g.hist.thm <- ggplot2::ggplot(thm_xt_sp) +
        ggplot2::ggtitle("Distribució d'altituds del gravat") +
        ggplot2::xlab("altituds (msnm)") + ylab("total") +
        # annotate("rect",xmin=nprosp.alt[1],xmax=nprosp.alt[2],ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
        # annotate("text", x = 1500, y = 100, label= "zone 4 \n no prospectada",angle=90,size=3) +
        ggplot2::geom_text(data = altis.zonas, 
                           mapping = ggplot2::aes(x = alt.moy, y = 300, label = paste0("zona ", zona)), 
                           angle = 90, size = 3, vjust = -1, hjust = 1) +
        ggplot2:: geom_rect(data = altis.zonas, 
                            mapping = ggplot2::aes(xmin = alt.min, xmax = alt.max, ymin = 0, ymax = Inf),
                            fill = mycolo, alpha = 0.1) +
        ggplot2::geom_histogram(ggplot2::aes(x = z), colour = "black", fill = "white", binwidth = 10) +
        ggplot2::theme(axis.text.y = element_text(angle = 90)) +
        ggplot2::theme_bw()
      lg[['altis_ngrav']] <- g.hist.thm
    }
    if("altis_tema" %in% stats){
      if(!seriated){
        if(verbose){
          print(paste0("Distribution engraved themes by altitudes - alphabetical order"))
        }
        g.bx.thm <- ggplot2::ggplot(ggplot2::aes(y = z, x = thm_xt), data = thm_xt_sp) +
          ggtitle("Altitudes por temas - orden alfabético") +
          ggplot2::geom_boxplot(fatten = 1.5, width = 0.7, lwd = 0.1, outlier.shape = NA) +
          #stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position=position_dodge(width=.5), size=.5)+
          ggplot2::geom_jitter(position = position_jitter(width = .5), size = .2, aes(colour = zona)) +
          # annotate("rect", ymin = nprosp.alt[1], ymax = nprosp.alt[2], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black")+
          # annotate("text", y = 1500, x = 25, label= "zone 4 \n no prospectada",size=3,hjust=0) +
          ggplot2::labs(x = "temas", y = "altituds (msnm)", colour = "zonas")+
          #xlab("temas") + ylab("altituds (msnm)")+
          ggplot2::theme_bw()+
          ggplot2::theme(legend.title = element_text(size = 8),
                         legend.text = element_text(size = 6),
                         #legend.key.size = unit(6,"line"),
                         axis.text.y = element_text(angle = 90, size = 5),
                         axis.text.x = element_text(angle = 90, size = 5, hjust = 1, vjust = 0))+
          ggplot2::guides(color = guide_legend(override.aes = list(size = 2)))
      }
      if(seriated){
        if(verbose){
          print(paste0("Distribution engraved themes by altitudes - seriation"))
        }
        df <- thm_xt
        df <- df[!is.na(df$thm_xt), ]
        # zmean
        thm_xt_zmean <- df %>%
          dplyr::group_by(thm_xt) %>%
          dplyr::summarize(z_mean = mean(z, na.rm = TRUE))
        # count
        thm_xt_count <- df %>%
          dplyr::group_by(thm_xt) %>%
          dplyr::summarise(n = n())
        thm_xt_zmean <- merge(thm_xt_zmean, thm_xt_count, by = "thm_xt")
        thm_xt_zmean_order <- thm_xt_zmean[order(thm_xt_zmean$z_mean), "thm_xt"]
        df$thm_xt <- factor(df$thm_xt, levels = thm_xt_zmean_order)
        df <- merge(df, thm_xt_zmean, by="thm_xt", all.x = T)
        df_sp <- sf::st_as_sf(df) # -> sf
        # max/min for ggplot
        z.min <- plyr::round_any(min(df$z, na.rm = T)-50, 50, f = ceiling)
        z.max <- plyr::round_any(max(df$z, na.rm = T), 50, f = ceiling)
        # size depend from nb thms (dim output, y labels)
        siz.rec <- length(unique(df$thm_xt))
        if(siz.rec < 33){by <- 100} else {by <- 50}
        #
        a.marg <- z.max + 25
        g.bx.thm <- ggplot2::ggplot(data = df_sp, ggplot2::aes(y = z, x = thm_xt)) +
          ggplot2::ggtitle("Altitudes por temas - seriación") +
          ggplot2::geom_boxplot(fatten = 1.5, width = 0.7, lwd = 0.1, outlier.shape = NA) +
          ggplot2::geom_jitter(position = position_jitter(width = .5),
                               size=.2, alpha = 0.5, aes(colour = zona)) +
          # size=.2, alpha = 0.5, aes(colour = df_sp$zona)) +
          ggplot2::geom_point(y = df_sp$z_mean, x = df_sp$thm_xt,
                              shape= 3, color = "red", cex = .5) + # mean
          ggplot2::geom_text(y = z.max, x = df_sp$thm_xt, label = df_sp$n, size = 1.5, hjust = 0.5) + # numbers
          ggplot2::labs(x = "temas", y = "altituds (msnm)", colour = "zonas")+
          ggplot2::expand_limits(y = c(z.min, a.marg)) +
          ggplot2::scale_y_continuous(breaks = seq(z.min, z.max, by = by)) +
          ggplot2::theme_bw()+
          ggplot2::theme(legend.title = element_text(size = 8),
                         legend.text = element_text(size = 6),
                         #legend.key.size = unit(6,"line"),
                         axis.title = element_text(size = 7),
                         axis.text.y = element_text(size = 6),
                         axis.text.x = element_text(size = 6)) +
          ggplot2::coord_flip() +
          ggplot2::scale_colour_manual(values = mycolo) +
          ggplot2::guides(color = guide_legend(override.aes = list(size = 2)))
      }
      lg[['altis_tema']] <- g.bx.thm
    }
  }
  if("spats_tema" %in% stats & is.list(lthms)){
    if(verbose){
      print(paste0("Spatial distribution of selected engraved themes"))
    }
    # out dataframe
    df.thm <- data.frame(roche = character(),
                         thm = character(),
                         n = numeric(),
                         X = numeric(),
                         Y = numeric(),
                         z = numeric(),
                         zona = numeric())
    for (i in seq(1:length(lthms))){
      # i <- 1
      nom <- names(lthms[i])
      print(paste0(i, ": ", nom))
      patt <- as.character(lthms[i][[1]][2])
      col <- lthms[i][[1]][1]
      ico <- thm_xt[grep(patt, thm_xt[, col]),]
      thm_ct_r <- ico %>% 
        dplyr::count(roche)
      thm_ct_r$roche <- as.character(thm_ct_r$roche)
      roques.coords <- cbind(roques, st_coordinates(roques))
      sp_thm_ct_r <- merge(thm_ct_r, roques.coords, by.x = "roche", by.y= "roche", all.x = TRUE)
      sp_thm_ct_r <- sp_thm_ct_r[!duplicated(sp_thm_ct_r$roche), ]
      nrow(sp_thm_ct_r)
      # remove z = NA
      sp_thm_ct_r <- sp_thm_ct_r[!is.na(sp_thm_ct_r$z), ]
      sp_thm_ct_r$thm <- gsub("thm_", "", names(lthms[i])) # remvoe "thm_" from label
      sp_thm_ct_r <- sp_thm_ct_r[ , c("roche", "thm", "n", "X", "Y", "z", "zona")]
      df.thm <- rbind(df.thm, sp_thm_ct_r)
    }
    gg_ico <- ggplot(df.thm, aes(X, Y, label = roche)) +
      ggplot2::ggtitle("Distribución espacial de los temas seleccionados") +
      ggplot2::facet_grid(thm ~ .) +
      ggplot2::geom_point(data = df.thm, aes(X, Y), show.legend = FALSE, color="grey") +
      ggplot2::geom_point(aes(size = n, color = zona), show.legend = FALSE) +
      ggrepel::geom_text_repel(size = 1.5, segment.alpha = 0.3, segment.size = 0.1) +
      ggplot2::coord_fixed()+
      ggplot2::theme_bw()+
      ggplot2::theme(plot.title = element_text(size = 8))+
      ggplot2::theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank())
    lg[['spats_tema']] <- gg_ico
  }
  return(lg)
}
