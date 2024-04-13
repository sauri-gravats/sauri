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
                          pts.size = 1.5,
                          lbl.size = 2,
                          ind.color = "blue",
                          var.color = "black",
                          ind.lbl.shw = FALSE,
                          var.lbl.shw = TRUE,
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
      ggplot2::geom_point(ggplot2::aes(CA1, CA2,
                                       colour = color,
                                       fill = color,
                                       stroke = .5,
                                       pch = shape),
                          size = pts.size) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          size = 0.2, alpha = 0.3) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          size = 0.2, alpha = 0.3)
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
    gg.ca <- gg.ca + ggplot2::geom_text(data = df.ca.perc,
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
      ggplot2::theme(axis.ticks = ggplot2::element_line(size = 0.2)) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 8),
                     strip.text.y = ggplot2::element_blank()) +
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = 'black',
                                                          fill = NA,
                                                          size = 0.2)) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'transparent')) +
      ggplot2::theme(panel.spacing.y = ggplot2::unit(0, "lines")) +
      # ggplot2::scale_x_continuous(limits = CA1_interval, expand = c(0, 0)) +
      # ggplot2::scale_y_continuous(limits = CA2_interval, expand = c(0, 0)) +
      ggplot2::scale_colour_identity() +
      # ggplot2::scale_shape_identity() +
      ggplot2::scale_fill_identity()
    lg[['mult_ca']] <- gg.ca
  }
  return(lg)
}



