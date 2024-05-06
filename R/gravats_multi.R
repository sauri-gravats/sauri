#' @name gravats_multi
#' 
#' @description Multifactorial statistics on engravings only, either for themes (column 'thm_xt'), technics (column 'tec'), lateralisation (column 'lat'). The `limit.coords` parameter, when not `NA`, returns a 'zoom' on a particular area of the plot with the list of individuals within this box.
#'
#' @param gravats Path to the file of engraved rocks.
#' @param stats Which stats will be performed.
#' @param limit.coords Deafult `NA`. If a vector is provided, will limit the ggplot to these coordinates, xmin, xmax, ymin and ymax.
#' @param lg If a list is provided, will output results in this list, if not, will create a list from scratch.
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
                          gravats.main.thm = c("personatge", "zoomorf", "literal", "tècnic", "geomètric", "irreconeixible"),
                          stats = c("mult_ca", "sd"),
                          lg = NA,
                          lthms = NA,
                          # patts = c("estrella", "arboriforme", "pentacle", "zigzag",
                          #           "dona", "home", "personatge",
                          #           "ballesta", "llança", "espasa", "arc",
                          #           # "fletxa",
                          #           "casc/barret/corona",
                          #           "cornamusa",
                          #           "ocell", "cavall|equid"),
                          sd.thres = 2,
                          pts.size = 1.5,
                          lbl.size = 2,
                          ind.color = "lightgrey",
                          var.color = "black",
                          ind.lbl.shw = FALSE,
                          var.lbl.shw = TRUE,
                          limit.coords = NA,
                          verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  thm_xt <- subset(gravats, select = gravats.fields)
  # library(FactoMineR)
  # library("factoextra")
  # main themes
  if(is.na(lg)){
    if(verbose){
      print(paste0("Overwrites/Creates the outut `lg` list"))
    }
    lg <- list()
  }
  if("sd" %in% stats){
    # TODO: put this 'sd' part into roques_x_gravats
    if(verbose){
      print(paste0("SD (standard deviation)"))
    }
    source("R/gravats_desc.R")
    lg_thm <- gravats_desc(gravats = gravats, stats = c("thm"))
    # TODO: deal with the main themes
    lg_thm[['grav_thm']] <- lg_thm[['grav_thm']][ , -gravats.main.thm]
      
    # thm_xt$thm_xt <- stringr::str_trim(thm_xt$thm_xt)
    # result <- thm_xt %>%
    #   tidyr::separate_rows(thm_xt, sep = "\\+") %>%
    #   dplyr::mutate(thm_xt = trimws(tolower(thm_xt))) %>%
    #   dplyr::filter(thm_xt != "") %>%
    #   dplyr::count(roche, thm_xt) %>%
    #   tidyr::pivot_wider(names_from = thm_xt, values_from = n, values_fill = list(n = 0)) %>%
    #   dplyr::select(roche, sort(names(.[-1]))) %>%
    #   dplyr::select(-all_of(gravats.main.thm)) %>%
    #   tibble::column_to_rownames('roche')
    # lg[['table']] <- result
    lg[['table']] <- lg_thm[['grav_thm']]
    ## Error ## TODO: to fix
    # sd
    # Calculate means and standard deviations for each variable
    means <- colMeans(lg[['table']])
    sds <- apply(lg[['table']], 2, sd)
    # Calculate the number of standard deviations each individual's score is from the mean
    df_sds <- sweep(lg[['table']], 2, means, "-") / sds
    # You might want to identify values that are more than a certain number of standard deviations away
    # For example, you could use a threshold of 2 standard deviations for significance
    df_significant <- df_sds > sd.thres
    
    # Now, you have a logical dataframe indicating which values are significantly higher
    # If you want a dataframe with the actual standard deviations where significant:
    df_result <- df_sds * df_significant
    View(df_result)
    # If you want to replace non-significant values with 0 or NA:
    # df_result[df_result <= threshold] <- 0  # or
  }
  if("mult_ca" %in% stats){
    if(verbose){
      print(paste0("CA on engravings main themes"))
    }
    # copy the structure
    df.patts <- thm_xt[0, ]
    # for (patt in lthms){
    #   # patt <- "estrella"
    for (i in seq(1:length(lthms))){
      # i <- 1
      nom <- names(lthms[i])
      patt <- as.character(lthms[i][[1]][2])
      if(verbose){
        print(paste0(i, ": ", nom, " | pattern = ", patt))
      }
      # find the right column
      col <- lthms[i][[1]][1]
      ico <- thm_xt[grep(patt, thm_xt[, col]),]
      
      # ico$thm_xt <- nom
      
      # 
      # if(verbose){
      #   print(paste0(" .. read: ", patt))
      # }
      # thm_xt.thm <- thm_xt[grep(patt, thm_xt[,"thm_xt"]),]
      if(nrow(ico) > 0 ){
        ico$thm_xt <-  gsub("thm_", "", nom)
        df.patts <- rbind(df.patts, ico)
      } else {
        if(verbose){
          print(paste0(" .. .... /!\ no data for: ", patt))
        }
      }
    }
    df.ac <- table(df.patts$roche, df.patts$thm_xt)
    # res.ca <- FactoMineR::CA(df.ac, graph = FALSE)
    # gg.ca <- factoextra::fviz_ca_biplot(res.ca, repel = TRUE, labelsize = 2)
    ca <- FactoMineR::CA(df.ac, graph = FALSE)            # AFC
    inertCA1 <- round(as.numeric(ca$eig[, 2][1]), 1)
    inertCA2 <- round(as.numeric(ca$eig[, 2][2]), 1)
    df.ca.perc <- data.frame(
      perCA1 = inertCA1,
      perCA2 = inertCA2
    )
    coords_ind_ca <- as.data.frame(ca$row$coord)
    coords_var_ca <- as.data.frame(ca$col$coord)
    coords_ca <- rbind(coords_ind_ca, coords_var_ca)
    colnames(coords_ca)[1] <- 'CA1'
    colnames(coords_ca)[2] <- 'CA2'
    dataset.p <- merge(df.ac, coords_ca, by = "row.names", all.y = T)
    # symbols
    var.thms <- unique(df.patts$thm_xt)
    var.symb <- data.frame(Row.names = var.thms,
                           shape = rep(17, length(var.thms)),
                           color = rep(var.color, length(var.thms))
    )
    ind.symb <- data.frame(Row.names = rownames(df.ac),
                           shape = rep(16, nrow(df.ac)),
                           color = ind.color
    )
    dfsymb <- rbind(ind.symb, var.symb)
    dataset.ps <- merge(dataset.p, dfsymb, by = "Row.names", all.x = T)
    dataset.ps$shape <- as.factor(dataset.ps$shape)
    names(dataset.ps)[names(dataset.ps) == 'Row.names'] <- "roques"
    
    # reorder to have the var over the ind
    dataset.ps$color <- factor(dataset.ps$color, levels = c(ind.color, var.color))
    dataset.ps <- dataset.ps[order(dataset.ps$color), ]
    
    
    # ff <- merge(dataset.ps, df_per_site, by = num_column, all.x = T)
    # matches <- colnames(dataset.ps) # reorder
    # ff <- ff[ ,match(matches, colnames(ff))]
    # dataset.ps <- rbind(dataset.ps,ff)
    # perCA_tsit <- perCA_tsit[-1, ] #remove line 1 = 'xxx'
    # dataset.ps<- dataset.ps[-1, ]
    # dataset.ps$shape <- as.factor(dataset.ps$shape)
    # dataset.ps$color <- as.factor(dataset.ps$color)
    # CA
    # dataset.ps <- dataset.ps
    gg.ca <- ggplot2::ggplot(dataset.ps, ggplot2::aes(CA1, CA2)) +
      # see: https://github.com/zoometh/itineRis/blob/11c8680f190fe339cc85eae6a695d82734fa140e/R/isotop_ca.R#L123
      # ggplot2::geom_text(ggplot2::aes(#x = min(CA1_interval),
      #                                 #y = max(CA2_interval),
      #                                 label = "my label"),
      #                    hjust = 0,
      #                    vjust = 1) +
      ggplot2::labs(# title = "Número de grabados por roca - media, mediana, moda", 
        # subtitle = "Plot of random data points", 
        caption = paste0("nb rocas = ", nrow(coords_ind_ca), " | nb themas = ", nrow(coords_var_ca))) +
      ggplot2::geom_point(ggplot2::aes(CA1, CA2,
                                       colour = color,
                                       fill = color,
                                       stroke = .5,
                                       pch = shape),
                          size = pts.size) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          linewidth = 0.2, alpha = 0.3) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          linewidth = 0.2, alpha = 0.3) +
      # gg.ca <- gg.ca + 
      ggplot2::geom_text(data = df.ca.perc,
                         mapping = ggplot2::aes(x = 0, y = -Inf,
                                                label = paste0(perCA1,"%")),
                         vjust = -1,
                         size = 2,
                         alpha = 0.5) +
      ggplot2::geom_text(data = df.ca.perc,
                         mapping = ggplot2::aes(x = -Inf, y = 0,
                                                label = paste0(perCA2, "%")),
                         vjust = 1,
                         angle = 90,
                         size = 2,
                         alpha = 0.5) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 5),
                     axis.title.x = ggplot2::element_text(size = 8),
                     axis.title.y = ggplot2::element_text(size = 8)) +
      ggplot2::theme(axis.ticks = ggplot2::element_line(linewidth = 0.2)) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 8),
                     strip.text.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = 'black',
                                                          fill = NA,
                                                          linewidth = 0.2)) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'transparent')) +
      ggplot2::theme(panel.spacing.y = ggplot2::unit(0, "lines")) +
      # ggplot2::scale_x_continuous(limits = CA1_interval, expand = c(0, 0)) +
      # ggplot2::scale_y_continuous(limits = CA2_interval, expand = c(0, 0)) +
      ggplot2::scale_colour_identity() +
      # ggplot2::scale_shape_identity() +
      ggplot2::scale_fill_identity()
    if(is.numeric(limit.coords)){
      if(verbose){
        print(paste0("Limit the plot to selected coordinates (box)"))
      }
      limit.indvar <- dataset.ps %>%
        dplyr::filter(
          CA1 >= limit.coords[1] & CA1 <= limit.coords[2],
          CA2 >= limit.coords[3] & CA2 <= limit.coords[4],
        )
      if(ind.lbl.shw){
        limit.indvar.ind <- limit.indvar[!(limit.indvar$roques %in% var.thms), ]
        gg.ca <- gg.ca +  
          ggrepel::geom_text_repel(data = limit.indvar.ind,
                                   ggplot2::aes(CA1, CA2, label = roques),
                                   cex = lbl.size,
                                   segment.size = 0.1,
                                   segment.alpha = 0.5,
                                   max.overlaps = Inf)
      }
      if(var.lbl.shw){
        limit.indvar.var <- limit.indvar[limit.indvar$roques %in% var.thms, ]
        gg.ca <- gg.ca +  
          ggrepel::geom_text_repel(data = limit.indvar.var,
                                   ggplot2::aes(CA1, CA2, label = roques),
                                   cex = lbl.size,
                                   segment.size = 0.1,
                                   segment.alpha = 0.5,
                                   max.overlaps = Inf)
      }
      gg.ca <- gg.ca +
        ggplot2::coord_cartesian(xlim = c(limit.coords[1], limit.coords[2]), 
                                 ylim = c(limit.coords[3], limit.coords[4])) 
      if(verbose){
        print(paste0("Creates the list of individuals within the coordinates (box)"))
      }
      # the individuals within the box
      limit.ind <- dataset.ps %>%
        dplyr::filter(
          CA1 >= limit.coords[1] & CA1 <= limit.coords[2],
          CA2 >= limit.coords[3] & CA2 <= limit.coords[4],
          color == ind.color
        )
      lg[['mult_ca_limit_ind']] <- limit.ind[, c("roques", "CA1", "CA2")]
    } else {
      # plot all labels
      if(ind.lbl.shw){
        dataset.ps.ind <- dataset.ps[!(dataset.ps$roques %in% var.thms), ]
        gg.ca <- gg.ca +  
          ggrepel::geom_text_repel(data = dataset.ps.ind,
                                   ggplot2::aes(CA1, CA2, label = roques),
                                   cex = lbl.size,
                                   segment.size = 0.1,
                                   segment.alpha = 0.5,
                                   max.overlaps = Inf)
      }
      if(var.lbl.shw){
        dataset.ps.var <- dataset.ps[dataset.ps$roques %in% var.thms, ]
        gg.ca <- gg.ca +  
          ggrepel::geom_text_repel(data = dataset.ps.var,
                                   ggplot2::aes(CA1, CA2, label = roques),
                                   cex = lbl.size,
                                   segment.size = 0.1,
                                   segment.alpha = 0.5,
                                   max.overlaps = Inf)
      }
    }
    lg[['mult_ca_plot']] <- gg.ca
    lg[['mult_ca_stat']] <- ca
  }
  return(lg)
}



