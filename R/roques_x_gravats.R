#' @name roques_x_gravats
#' 
#' @description Descriptive statistics on the engraving distributions by engraved rocks: numbers, altitudes, etc.
#'
#' @param roques A dataframe of engraved rocks.
#' @param gravats Path to the file of engraved rocks.
#' @param lthms List of selected themes. Only useful if "spats_tema" is in `stats`.
#' @param stats Which stats will be performed. `altis_tema_full` will create a plot for the full engravings themes (ex: `personatge + casc/barret/corona + home + llança + espasa`) while `altis_tema_att` while split these full themes in how many attributes (ex: `personatge`,  `casc/barret/corona`, `home`, `llança`, `espasa`) to considerate theme separately.
#' @param lg If a list is provided, will output results in this list, if not, will create a list from scratch.
#' @param limit_ind A character (a vector of characters) of engraved rocks to highlight (ex: `c("R126", R11", ...)`). Default: `NA`.
#' @param title.add A complement to the title. Useful when `limit_ind` is TRUE to some of selected rocks (for example, rocks with 'arrows').
#' @param seriated If TRUE, seriate the distribution
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list of ggplots
#'
#' @examples
#' 
#' 
#' @export
roques_x_gravats <- function(roques = NA,
                             gravats = NA,
                             roques.fields = c("geom", "z", "zona"),
                             gravats.fields = c("roche", "thm", "tec", "lat", "thm_xt"),
                             gravats.main.thm = c("personatge", "zoomorf", "literal", "tècnic", "geomètric", "irreconeixible"),
                             lthms = NA,
                             stats = c("ngrav", "altis_ngrav", "altis_tema_full", "altis_tema_att", "spats_tema", "spats_limit_ind"),
                             lg = NA,
                             limit_ind = NA,
                             seriated = TRUE,
                             title.add = NA,
                             verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  if(!is.list(lg)){
    if(verbose){
      print(paste0("Overwrites/Creates the outut `lg` list"))
    }
    lg <- list()
  }
  if("altis_ngrav" %in% stats | "altis_tema_full" %in% stats){
    stats <- c(stats, "altis_full")
  }
  if("altis_tema_att" %in% stats){
    stats <- c(stats, "altis_att")
  }
  # modify `roques` GeoPackage column names if needed
  names(roques)[names(roques) == "cota"] <- "z"
  names(roques)[names(roques) == "roca"] <- "roche"
  roques_x_gravats.fields <- c(gravats.fields, roques.fields)
  # 'full'
  thm_xt <- subset(gravats, select = gravats.fields)
  thm_xt <- merge(thm_xt, roques, by = "roche", all.x = T)
  thm_xt <- subset(thm_xt, select = roques_x_gravats.fields)
  # 'att'
  source("R/gravats_desc.R")
  lg_thm <- gravats_desc(gravats = gravats, stats = c("thm"))
  thm_xt_att <- lg_thm[['grav_thm']]
  roques_x_gravats.fields <- c(colnames(thm_xt_att), roques.fields)
  thm_xt_att$roche <- rownames(thm_xt_att)
  thm_xt_att <- merge(thm_xt_att, roques, by = "roche", all.x = T)
  thm_xt_att <- subset(thm_xt_att, select = roques_x_gravats.fields)
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
      ggplot2::labs(title = "Número de grabados por roca - media, mediana, moda", 
                    # subtitle = "Plot of random data points", 
                    caption = paste0("nb rocas = ", nrow(nb.grav.roche), " | nb grabados = ", nrow(thm_xt))) +
      # ggplot2::ggtitle("Número de grabados por roca - media, mediana, moda",
      #                  caption = paste0("nrow = ", nrow(nb.grav.roche))) +
      ggplot2::geom_freqpoly(binwidth = 1)+
      ggplot2::geom_vline(xintercept = mode.gr, linetype = "dotted",
                          color = "lightgrey", linewidth =.5) +
      ggplot2::annotate("text", x = mode.gr, y = 40, label = modo.label, angle = 90, size = 3, color = "darkgrey") +
      ggplot2::geom_vline(xintercept = mean.gr, 
                          color = "lightgrey", linewidth = .5) +
      ggplot2::annotate("text", x = mean.gr, y = 40, label = mean.label, angle = 90, size = 3, color = "darkgrey") +
      ggplot2::geom_vline(xintercept = median.gr, linetype = "dashed", 
                          color = "lightgrey", linewidth = .5) +
      ggplot2::annotate("text", x = median.gr, y = 40, label= median.label, angle = 90, size = 3, color = "darkgrey") +
      ggplot2::xlab("nb gravats") + 
      ggplot2::ylab("nb roques gravades") +
      ggplot2::scale_x_continuous(breaks = c(1, 5, 10, 20, 30, 50, 75, 100))+
      ggplot2::xlim(0.9, 100)+
      ggplot2::theme_bw()
    lg[['ngrav']] <- g.dist.nbgrav
  }
  if("altis_full" %in% stats | "altis_att" %in% stats){
    if(verbose){
      print(paste0("Distribution of engravings by altitudes"))
    }
    ## altitudes histograms
    if("altis_full" %in% stats){
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
    }
    
    if("altis_att" %in% stats){
      # zones
      altis.zonas.att <- thm_xt_att %>% 
        dplyr::group_by(zona) %>% 
        dplyr::mutate(alt.min = min(z),
                      alt.max  = max(z),
                      alt.moy = mean(z))
      altis.zonas.att <- subset(altis.zonas.att, select = c("zona", "alt.min", "alt.max", "alt.moy"))
      altis.zonas.att <- altis.zonas.att[!duplicated(altis.zonas.att), ]
      # remove zones without altitudes
      altis.zonas.att <- altis.zonas.att[!is.na(altis.zonas.att$zona), ]
      # rocks
      thm_xt_sp_att <- sf::st_as_sf(thm_xt_att) # -> sf
      # remove rocks without altitudes
      thm_xt_sp_att <- thm_xt_sp_att[!is.na(thm_xt_sp_att$z), ]
      mycolo <- c("red", "blue", "pink", "yellow", "green")
      # regroupements
      # thm_xt_sp_att <- thm_xt_sp_att[order(thm_xt_sp_att$thm_xt_att),] 
    }
    
    # min.mean.sd.max <- function(x) {
    #   r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
    #   names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    #   r
    # }
    if("altis_ngrav" %in% stats){
      if(verbose){
        print(paste0("Distribution of total number of engravings by altitudes"))
      }
      g.hist.thm <- ggplot2::ggplot(thm_xt_sp) +
        ggplot2::ggtitle("Distribució d'altituds del gravat") +
        ggplot2::xlab("altituds (msnm)") + 
        ggplot2::ylab("total") +
        # annotate("rect",xmin=nprosp.alt[1],xmax=nprosp.alt[2],ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
        # annotate("text", x = 1500, y = 100, label= "zone 4 \n no prospectada",angle=90,size=3) +
        ggplot2::geom_text(data = altis.zonas, 
                           mapping = ggplot2::aes(x = alt.moy, y = 300, label = paste0("zona ", zona)), 
                           angle = 90, size = 3, vjust = -1, hjust = 1) +
        ggplot2:: geom_rect(data = altis.zonas, 
                            mapping = ggplot2::aes(xmin = alt.min, xmax = alt.max, ymin = 0, ymax = Inf),
                            fill = mycolo, alpha = 0.1) +
        ggplot2::geom_histogram(ggplot2::aes(x = z), colour = "black", fill = "white", binwidth = 10) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90)) +
        ggplot2::theme_bw()
      lg[['altis_ngrav']] <- g.hist.thm
    }
    if("altis_tema_full" %in% stats){
      if(!seriated){
        if(verbose){
          print(paste0("Distribution of 'full' engraved themes by altitudes - alphabetical order"))
        }
        g.bx.thm <- ggplot2::ggplot(ggplot2::aes(y = z, x = thm_xt), data = thm_xt_sp) +
          ggtitle("Altitudes por temas - orden alfabético") +
          ggplot2::geom_boxplot(fatten = 1.5, width = 0.7, lwd = 0.1, outlier.shape = NA) +
          #stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position=position_dodge(width=.5), size=.5)+
          ggplot2::geom_jitter(position = ggplot2::position_jitter(width = .5), size = .2, aes(colour = zona)) +
          # annotate("rect", ymin = nprosp.alt[1], ymax = nprosp.alt[2], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black")+
          # annotate("text", y = 1500, x = 25, label= "zone 4 \n no prospectada",size=3,hjust=0) +
          ggplot2::labs(x = "temas", y = "altituds (msnm)", colour = "zonas")+
          #xlab("temas") + ylab("altituds (msnm)")+
          ggplot2::theme_bw()+
          ggplot2::theme(legend.title = ggplot2::element_text(size = 8),
                         legend.text = ggplot2::element_text(size = 6),
                         #legend.key.size = unit(6,"line"),
                         axis.text.y = ggplot2::element_text(angle = 90, size = 5),
                         axis.text.x = ggplot2::element_text(angle = 90, size = 5, hjust = 1, vjust = 0))+
          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 2)))
      }
      if(seriated){
        if(verbose){
          print(paste0("Distribution of 'full' engraved themes by altitudes - seriation"))
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
          dplyr::summarise(n = dplyr::n())
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
          ggplot2::geom_jitter(position = ggplot2::position_jitter(width = .5),
                               size=.2, alpha = 0.5, ggplot2::aes(colour = zona)) +
          # size=.2, alpha = 0.5, aes(colour = df_sp$zona)) +
          ggplot2::geom_point(y = df_sp$z_mean, x = df_sp$thm_xt,
                              shape= 3, color = "red", cex = .5) + # mean
          ggplot2::geom_text(y = z.max, x = df_sp$thm_xt, label = df_sp$n, size = 1.5, hjust = 0.5) + # numbers
          ggplot2::labs(x = "temas", y = "altituds (msnm)", colour = "zonas")+
          ggplot2::expand_limits(y = c(z.min, a.marg)) +
          ggplot2::scale_y_continuous(breaks = seq(z.min, z.max, by = by)) +
          ggplot2::theme_bw()+
          ggplot2::theme(legend.title = ggplot2::element_text(size = 8),
                         legend.text = ggplot2::element_text(size = 6),
                         #legend.key.size = unit(6,"line"),
                         axis.title = ggplot2::element_text(size = 7),
                         axis.text.y = ggplot2::element_text(size = 6),
                         axis.text.x = ggplot2::element_text(size = 6)) +
          ggplot2::coord_flip() +
          ggplot2::scale_colour_manual(values = mycolo) +
          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 2)))
      }
      lg[['altis_tema_full']] <- g.bx.thm
    }
    if("altis_tema_att" %in% stats){
      if(verbose){
        print(paste0("Convert main themes columns to UPPERCASE"))
      }
      names(thm_xt_sp_att)[names(thm_xt_sp_att) %in% gravats.main.thm] <- toupper(gravats.main.thm)
      if(verbose){
        print(paste0("Convert df to a long format"))
      }
      # Convert to long format
      long_format <- thm_xt_sp_att %>%
        tidyr::pivot_longer(cols = -roques.fields,   # Change 'everything()' if you need to exclude some columns
                            names_to = "thm_xt_att",    # Column containing the original column names
                            values_to = "count")       # Column containing the counts
      # Repeat rows according to the counts using `uncount`
      expanded_format <- long_format %>%
        tidyr::uncount(count)   # Will repeat each row according to the 'count' column
      # sapply(thm_xt_sp_att, class)
      if(!seriated){
        if(verbose){
          print(paste0("Distribution of 'attribute' engraved themes by altitudes - alphabetical order"))
        }
        # plot
        g.bx.thm <- ggplot2::ggplot(ggplot2::aes(y = z, x = thm_xt_att), data = expanded_format) +
          ggplot2::ggtitle("Altitudes por temas - orden alfabético") +
          ggplot2::geom_boxplot(fatten = 1.5, width = 0.7, lwd = 0.1, outlier.shape = NA) +
          #stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position=position_dodge(width=.5), size=.5)+
          ggplot2::geom_jitter(position = ggplot2::position_jitter(width = .5), size = .2, ggplot2::aes(colour = zona)) +
          # annotate("rect", ymin = nprosp.alt[1], ymax = nprosp.alt[2], xmin = -Inf, xmax = Inf, alpha = 0.1, fill = "black")+
          # annotate("text", y = 1500, x = 25, label= "zone 4 \n no prospectada",size=3,hjust=0) +
          ggplot2::labs(x = "temas", y = "altituds (msnm)", colour = "zonas")+
          #xlab("temas") + ylab("altituds (msnm)")+
          ggplot2::theme_bw()+
          ggplot2::theme(legend.title = ggplot2::element_text(size = 8),
                         legend.text = ggplot2::element_text(size = 6),
                         #legend.key.size = unit(6,"line"),
                         axis.text.y = ggplot2::element_text(angle = 90, size = 5),
                         axis.text.x = ggplot2::element_text(angle = 90, size = 5, hjust = 1, vjust = 0))+
          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 2)))
      }
      if(seriated){
        if(verbose){
          print(paste0("Distribution of 'attribute' engraved themes by altitudes - seriation"))
        }
        df <- expanded_format
        # df <- df[!is.na(df$thm_xt), ]
        # zmean
        thm_xt_zmean <- df %>%
          dplyr::group_by(thm_xt_att) %>%
          dplyr::summarize(z_mean = mean(z, na.rm = TRUE))
        # count
        thm_xt_count <- df %>%
          dplyr::group_by(thm_xt_att) %>%
          dplyr::summarise(n = dplyr::n())
        thm_xt_zmean <- merge(thm_xt_zmean, thm_xt_count, by = "thm_xt_att")
        thm_xt_zmean_order <- thm_xt_zmean[order(thm_xt_zmean$z_mean), "thm_xt_att"]
        df$thm_xt_att <- factor(df$thm_xt_att, levels = thm_xt_zmean_order)
        df <- merge(df, thm_xt_zmean, by="thm_xt_att", all.x = T)
        df_sp <- sf::st_as_sf(df) # -> sf
        # max/min for ggplot
        z.min <- plyr::round_any(min(df$z, na.rm = T)-50, 50, f = ceiling)
        z.max <- plyr::round_any(max(df$z, na.rm = T), 50, f = ceiling)
        # size depend from nb thms (dim output, y labels)
        siz.rec <- length(unique(df$thm_xt_att))
        if(siz.rec < 33){by <- 100} else {by <- 50}
        #
        a.marg <- z.max + 25
        g.bx.thm.att <- ggplot2::ggplot(data = df_sp, ggplot2::aes(y = z, x = thm_xt_att)) +
          ggplot2::ggtitle("Altitudes por temas - seriación") +
          ggplot2::geom_boxplot(fatten = 1.5, width = 0.7, lwd = 0.1, outlier.shape = NA) +
          ggplot2::geom_jitter(position = ggplot2::position_jitter(width = .5),
                               size=.2, alpha = 0.5, ggplot2::aes(colour = zona)) +
          # size=.2, alpha = 0.5, aes(colour = df_sp$zona)) +
          ggplot2::geom_point(y = df_sp$z_mean, x = df_sp$thm_xt_att,
                              shape= 3, color = "red", cex = .5) + # mean
          ggplot2::geom_text(y = z.max, x = df_sp$thm_xt_att, label = df_sp$n, size = 1.5, hjust = 0.5) + # numbers
          ggplot2::labs(x = "temas", y = "altituds (msnm)", colour = "zonas")+
          ggplot2::expand_limits(y = c(z.min, a.marg)) +
          ggplot2::scale_y_continuous(breaks = seq(z.min, z.max, by = by)) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.title = ggplot2::element_text(size = 8),
                         legend.text = ggplot2::element_text(size = 6),
                         #legend.key.size = unit(6,"line"),
                         axis.title = ggplot2::element_text(size = 7),
                         axis.text.y = ggplot2::element_text(size = 6),
                         axis.text.x = ggplot2::element_text(size = 6)) +
          ggplot2::coord_flip() +
          ggplot2::scale_colour_manual(values = mycolo) +
          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 2)))
      }
      lg[['altis_tema_att']] <- g.bx.thm.att
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
      # i <- 2
      nom <- names(lthms[i])
      patt <- as.character(lthms[i][[1]][2])
      if(verbose){
        print(paste0(i, ": ", nom, " | pattern = ", patt))
      }
      # find the right column
      col <- lthms[i][[1]][1]
      ico <- thm_xt[grep(patt, thm_xt[, col]),]
      # ico <- thm_xt[grep("personatge.*ballesta", thm_xt[, col]),]
      # head(ico)
      if(nrow(ico) > 0){
        thm_ct_r <- ico %>% 
          dplyr::count(roche)
        thm_ct_r$roche <- as.character(thm_ct_r$roche)
        roques.coords <- cbind(roques, sf::st_coordinates(roques))
        sp_thm_ct_r <- merge(thm_ct_r, roques.coords, by.x = "roche", by.y= "roche", all.x = TRUE)
        sp_thm_ct_r <- sp_thm_ct_r[!duplicated(sp_thm_ct_r$roche), ]
        nrow(sp_thm_ct_r)
        # remove z = NA
        sp_thm_ct_r <- sp_thm_ct_r[!is.na(sp_thm_ct_r$z), ]
        sp_thm_ct_r$thm <- gsub("thm_", "", names(lthms[i])) # remvoe "thm_" from label
        sp_thm_ct_r <- sp_thm_ct_r[ , c("roche", "thm", "n", "X", "Y", "z", "zona")]
        df.thm <- rbind(df.thm, sp_thm_ct_r)
      } else {
        if(verbose){
          print(paste0("        /!\ None"))
        }
      }
    }
    gg_ico <- ggplot2::ggplot(df.thm, ggplot2::aes(X, Y, label = roche)) +
      ggplot2::ggtitle("Distribución espacial de los temas seleccionados") +
      ggplot2::facet_grid(thm ~ .) +
      ggplot2::geom_point(data = df.thm, ggplot2::aes(X, Y), show.legend = FALSE, color="grey") +
      ggplot2::geom_point(ggplot2::aes(size = n, color = zona), show.legend = FALSE) +
      ggrepel::geom_text_repel(size = 1.5, segment.alpha = 0.3, segment.size = 0.1) +
      ggplot2::coord_fixed()+
      ggplot2::theme_bw()+
      ggplot2::theme(plot.title = ggplot2::element_text(size = 8))+
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
    lg[['spats_tema']] <- gg_ico
  }
  if("spats_limit_ind" %in% stats & is.character(limit_ind)){
    if(verbose){
      print(paste0("Spatial distribution of selected engraved rocks"))
    }
    # tit
    if(!is.na(title.add)){
      if(verbose){
        print(paste0("Add new text to the plot title"))
      }
      tit <- paste("Distribución espacial de los roques gravades seleccionadas: ", toupper(title.add))
    } else {
      tit <- paste("Distribución espacial de los roques gravades seleccionadas")
    }
    # select and convert to dataframe and rename columns
    # limit_ind
    roques.limit_ind <- roques[roques$roche %in% limit_ind, ]
    roques.limit_indXY <- sf::st_coordinates(roques.limit_ind)
    roques.limit_ind <- sf::st_set_geometry(roques.limit_ind, NULL)
    roques.limit_ind.df <- cbind(roques.limit_ind, roques.limit_indXY)
    # print(colnames(roques.limit_ind.df))
    #
    roques.others <- roques[!(roques$roche %in% limit_ind), ]
    roques.othersXY <- sf::st_coordinates(roques.others)
    roques.others <- sf::st_set_geometry(roques.others, NULL)
    roques.others.df <- cbind(roques.others, roques.othersXY)
    # print(colnames(roques.others.df))
    ## /!\ cannot change correctly the column names /!\ ##
    # print(class(roques.limit_ind))
    # roques.limit_ind.df <- roques.limit_ind %>%
    #   sf::st_set_geometry(NULL) %>% 
    #   cbind(sf::st_coordinates(roques.limit_ind)) %>%
    #   dplyr::rename(Longitude = X, Latitude = Y)
    colnames(roques.limit_ind.df)[which(colnames(roques.limit_ind.df) == "X")] <- "Longitude"
    colnames(roques.limit_ind.df)[which(colnames(roques.limit_ind.df) == "Y")] <- "Latitude"
    # colnames(roques.limit_ind.df)[which(colnames(roques.limit_ind.df) == "1")] <- "Longitude"
    # colnames(roques.limit_ind.df)[which(colnames(roques.limit_ind.df) == "2")] <- "Latitude"
    # # others
    # 
    # roques.others.df <- roques.others %>%
    #   sf::st_set_geometry(NULL) %>%
    #   cbind(sf::st_coordinates(roques.others)) %>%
    #   dplyr::rename(Longitude = X, Latitude = Y)
    colnames(roques.others.df)[which(colnames(roques.others.df) == "X")] <- "Longitude"
    colnames(roques.others.df)[which(colnames(roques.others.df) == "Y")] <- "Latitude"
    # colnames(roques.others.df)[which(colnames(roques.others.df) == "1")] <- "Longitude"
    # colnames(roques.others.df)[which(colnames(roques.others.df) == "2")] <- "Latitude"
    # plot
    gg_limit_ind <- ggplot2::ggplot() +
      ggplot2::ggtitle(tit) +
      ggplot2::geom_point(data = roques.others.df, # x = roques.others$X, y = roques.others$Y,
                          ggplot2::aes(x = Longitude, y = Latitude),
                          show.legend = FALSE, color="grey") +
      ggplot2::geom_point(data = roques.limit_ind.df, # x = roques.limit_ind$X, y = roques.limit_ind$Y,
                          ggplot2::aes(x = Longitude, y = Latitude),
                          show.legend = FALSE, color="black") +
      ggrepel::geom_text_repel(data = roques.limit_ind.df,
                               ggplot2::aes(label = roche, x = Longitude, y = Latitude), max.overlaps = Inf) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 8)) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
    # fileOut <- paste0('spats_limit_ind_', title.add)
    if(verbose){
      print(paste0("Save the plot '", "spats_limit_ind", "'"))
    }
    lg[["spats_limit_ind"]] <- gg_limit_ind
  }
  return(lg)
}

# ,stat = "sf_coordinates", min.segment.length = 0
