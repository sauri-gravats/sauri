
outDir = "C:/Rprojects/sauri/doc/"

# read
source("R/roques_read.R")
source("R/gravats_read.R")
roques <- roques_read(roques = "C:/Rprojects/sauri/doc/data/sources/240415/Thomas_Sauri.gpkg",
                      gpkg.layer = "Roques")
gravats <- gravats_read(gravats = "C:/Rprojects/sauri/doc/data/sources/240415/llista_gravats_19-20-23_DEFINITIVA_FINAL_FINAL_1.xlsx")

############# Engraved rocks x Engravings #####################################
# Stats per roques: número de grabados por roca, altitudes, etc.
source("R/roques_x_gravats.R")
lg <- roques_x_gravats(roques = roques,
                       gravats = gravats)
# ggplot2::ggsave(paste0(outDir, "_distr_nbgravats_x_nbroques.png"), lg$ngrav, width = 16, height = 8, units = "cm")
# ggplot2::ggsave(paste0(outDir, "_distr_nbgravats_x_altura.png"), lg$altis_ngrav, width = 16, height = 10, units = "cm")
# ggplot2::ggsave(paste0(outDir, "_distr_typogravats_x_altura.png"), lg$altis_tema, width = 21, height = 18, units = "cm")


# Spatial
source("R/roques_x_gravats.R")
lthms <- list(# ico
  # thm_cavalier = c("thm", "A.?h"),
  # thm_arbaletrier = c("thm", "A.?b"),
  # thm_cheval = c("thm", "Z.?h"),
  # thm_oiseau = c("thm", "Z.?b"),
  # thm_fleche = c("thm", "T.?f"),
  # thm_arbalete = c("thm", "T.?b"),
  # thm_cornemuse = c("thm", "T.?m"),
  # thm_pentacle = c("thm", "G.?p"),
  # thm_croix_chretienne = c("thm", "G.?k"),
  # thm_ecritures = c("thm", "^L"),
  thm_genet = c("thm_xt", "personatge.*cavall"),
  thm_balleste = c("thm_xt", "personatge.*ballesta"),
  thm_cavall = c("thm_xt", "zoomorf.*èquid"),
  thm_ocell= c("thm_xt", "zoomorf.*ocell"),
  thm_fletxa = c("thm_xt", "tècnic.*fletxa"),
  thm_ballesta = c("thm_xt", "tècnic.*ballesta"),
  thm_cornamusa = c("thm_xt", "tècnic.*cornamusa"),
  thm_pentacle = c("thm_xt", "geomètric.*pentacle"),
  thm_creu_cristiana = c("thm_xt", "geomètric.*creu cristiana"),
  thm_escriptura = c("thm_xt", "literal"),
  tec_naviforme = c("tec", ".*n.*"),
  lat_dreta = c("lat", "->"),
  lat_esquerra = c("lat", "<-")
)
lg <- roques_x_gravats(roques = roques,
                       gravats = gravats,
                       lthms = lthms)
# ggplot2::ggsave(paste0(outDir, "_spat_typogravats_1.png"), lg$spats_tema, width = 21, height = length(lthms)*3, units = "cm")

############# Engraved rocks #####################################

source("R/roques_desc.R")
lg <- roques_desc(roques = roques)
# lg$map_leaflet

############# Engravings #####################################
## Descriptive
source("R/gravats_desc.R")
lg <- gravats_desc(gravats = gravats)
# ggplot2::ggsave(paste0(outDir, "_spat_typogravats.png"), lg$grav_tec, width = 14, height = 14, units = "cm")

########## 1 ###############
## Multifact
source("R/gravats_multi.R")
lthms <- list(# ico
  # anthropomorph
  thm_home = c("thm_xt", "personatge.*home"),
  thm_dona = c("thm_xt", "personatge.*dona"),
  thm_genet = c("thm_xt", "personatge.*cavall"),
  thm_ballester = c("thm_xt", "personatge.*ballesta"),
  thm_casc_barret_corona = c("thm_xt", "personatge.*casc/barret/corona"),
  # technomorph
  thm_lanca = c("thm_xt", "personatge.*llança"),
  thm_espasa = c("thm_xt", "personatge.*espasa"),
  thm_arc = c("thm_xt", "personatge.*arc"),
  thm_fletxa = c("thm_xt", "tècnic.*fletxa"),
  thm_ballesta = c("thm_xt", "tècnic.*ballesta"),
  thm_cornamusa = c("thm_xt", "tècnic.*cornamusa"),
  # zoomorph
  thm_cavall = c("thm_xt", "zoomorf.*cavall"),
  # thm_cavall_equid = c("thm_xt", "zoomorf.*(?=.*èquid)(?=.*cavall)"), # TODO
  thm_ocell= c("thm_xt", "zoomorf.*ocell"),
  # geometric
  thm_arboriforme = c("thm_xt", "geomètric.*arboriforme"),
  thm_pentacle = c("thm_xt", "geomètric.*pentacle"),
  thm_estrella = c("thm_xt", "geomètric.*estrella"),
  thm_zigzag = c("thm_xt", "geomètric.*zig-zag"),
  thm_creu_cristiana = c("thm_xt", "geomètric.*creu cristiana"),
  # writings
  thm_escriptura = c("thm_xt", "literal")
  # engraving technique
  # tec_naviforme = c("tec", ".*n.*")
  # lat_dreta = c("lat", "->"),
  # lat_esquerra = c("lat", "<-")
)
lg <- gravats_multi(gravats = gravats,
                    lthms = lthms,
                    pts.size = 2,
                    lbl.size = 3)
ggplot2::ggsave(paste0(outDir, "gthms_1_ca.png"), lg$mult_ca_plot, width = 15, height = 15, units = "cm")
## Not Run
# lg$mult_ca_stat$eig # Eigen values
# paste0(rownames(lg$mult_ca_stat$col$coord), collapse = ", ") # studied themes
## Draw boxes
source("R/dum_box.R")
lbox <- list(list("fletxa", c(-3.1, -1.7, 0, 1)), # 
             list("escriptura", c(0, 1, 3.5, 5)), # 
             list("others", c(-1.5, 1, -1, 2)) #
) # , 
gthms_1_box <- dum_box(gplot = lg$mult_ca_plot,
                       lbox = lbox)
ggplot2::ggsave(paste0(outDir, "gthms_1_ca_box.png"), gthms_1_box, width = 15, height = 15, units = "cm")

#  Export separate boxes
# source("R/gravats_multi.R")
# lg <- gravats_multi(gravats = gravats,
#                     lthms = lthms,
#                     pts.size = 2,
#                     lbl.size = 3,
#                     ind.lbl.shw = TRUE,
#                     limit.coords = lbox[[i]][[2]])
# 
# # Export separate boxes
# source("R/gravats_multi.R")
# lg <- gravats_multi(gravats = gravats,
#                     lthms = lthms,
#                     pts.size = 2,
#                     lbl.size = 3,
#                     ind.lbl.shw = TRUE,
#                     limit.coords = lbox[[i]][[2]])
# # lg$mult_ca_plot

source("R/roques_x_gravats.R")
# title.adds <- c("zigzag_and_arboriforme", "estrealla_and_creuChris_and_pentacle")
for(i in 1:(length(lbox)-1)){
  # avoid others
  # limit to some individuals to spot a particular spatial distribution
  lg <- gravats_multi(gravats = gravats,
                      lthms = lthms,
                      pts.size = 2,
                      lbl.size = 3,
                      ind.lbl.shw = TRUE,
                      limit.coords = lbox[[i]][[2]])
  ggplot2::ggsave(paste0(outDir, "gthms_1_ca_", lbox[[i]][[1]],".png"), lg$mult_ca_plot, 
                  width = 11, height = 11, units = "cm")
  lg <- roques_x_gravats(roques = roques,
                         gravats = gravats,
                         limit_ind = as.character(lg$mult_ca_limit_ind$roques),
                         stats = "spats_limit_ind",
                         title.add = lbox[[i]][[1]])
  # Spatial distribution of selected rocks
  ggplot2::ggsave(paste0(outDir, "gthms_1_spat_", lbox[[i]][[1]],".png"), lg$spats_limit_ind, 
                  width = 11, height = 11, units = "cm")
}

########## 2 ###############
# without fletxa and escrituras

source("R/gravats_multi.R")
lthms <- list(# ico
  # anthropomorph
  thm_home = c("thm_xt", "personatge.*home"),
  thm_dona = c("thm_xt", "personatge.*dona"),
  thm_genet = c("thm_xt", "personatge.*cavall"),
  thm_ballester = c("thm_xt", "personatge.*ballesta"),
  thm_casc_barret_corona = c("thm_xt", "personatge.*casc/barret/corona"),
  # technomorph
  thm_lanca = c("thm_xt", "personatge.*llança"),
  thm_espasa = c("thm_xt", "personatge.*espasa"),
  thm_arc = c("thm_xt", "personatge.*arc"),
  thm_ballesta = c("thm_xt", "tècnic.*ballesta"),
  thm_cornamusa = c("thm_xt", "tècnic.*cornamusa"),
  # zoomorph
  thm_cavall = c("thm_xt", "zoomorf.*cavall"),
  # thm_cavall_equid = c("thm_xt", "zoomorf.*(?=.*èquid)(?=.*cavall)"), # TODO
  thm_ocell= c("thm_xt", "zoomorf.*ocell"),
  # geometric
  thm_arboriforme = c("thm_xt", "geomètric.*arboriforme"),
  thm_pentacle = c("thm_xt", "geomètric.*pentacle"),
  thm_estrella = c("thm_xt", "geomètric.*estrella"),
  thm_zigzag = c("thm_xt", "geomètric.*zig-zag"),
  thm_creu_cristiana = c("thm_xt", "geomètric.*creu cristiana")
)
lg <- gravats_multi(gravats = gravats,
                    lthms = lthms,
                    pts.size = 2,
                    lbl.size = 3)
ggplot2::ggsave(paste0(outDir, "gthms_2_ca.png"), lg$mult_ca_plot, width = 15, height = 15, units = "cm")

## Multifact + boxes
source("R/dum_box.R")
# lbox <- list(c(1.2, 3, 0.5, 2.5), # zigzag and arboriforme
#              c(0, 2, -2, -.5), # creu_chritsiana estrell pentacle
#              c(-1.5, .5, -.5, 1) # other
# ) # , 
lbox <- list(list("zigzag_and_arboriforme", c(1.2, 2.8, 0.5, 2.2)), # zigzag and arboriforme
             list("creu_chritsiana_and_estrell_and_pentacle", c(0, 1.8, -2, -.5)), # 
             list("others", c(-1.2, .5, -.5, 1)) # o
) # , 

# Draw boxes
gthms_2 <- dum_box(gplot = lg$mult_ca_plot,
                   lbox = lbox)
ggplot2::ggsave(paste0(outDir, "gthms_2_ca_box.png"), gthms_2, width = 15, height = 15, units = "cm")

# Export separate boxes
source("R/gravats_multi.R")
lg <- gravats_multi(gravats = gravats,
                    lthms = lthms,
                    pts.size = 2,
                    lbl.size = 3,
                    ind.lbl.shw = TRUE,
                    limit.coords = lbox[[i]][[2]])
# lg$mult_ca_plot
# Spatial distribution of selected rocks
source("R/roques_x_gravats.R")
# title.adds <- c("zigzag_and_arboriforme", "estrealla_and_creuChris_and_pentacle")
for(i in 1:(length(lbox)-1)){
  # avoid others
  # limit to some individuals to spot a particular spatial distribution
  lg <- gravats_multi(gravats = gravats,
                      lthms = lthms,
                      pts.size = 2,
                      lbl.size = 3,
                      ind.lbl.shw = TRUE,
                      limit.coords = lbox[[i]][[2]])
  ggplot2::ggsave(paste0(outDir, "gthms_2_ca_", lbox[[i]][[1]],".png"), lg$mult_ca_plot, 
                  width = 11, height = 11, units = "cm")
  lg <- roques_x_gravats(roques = roques,
                         gravats = gravats,
                         limit_ind = as.character(lg$mult_ca_limit_ind$roques),
                         stats = "spats_limit_ind",
                         title.add = lbox[[i]][[1]])
  ggplot2::ggsave(paste0(outDir, "gthms_2_spat_", lbox[[i]][[1]],".png"), lg$spats_limit_ind, 
                  width = 11, height = 11, units = "cm")
}


########## 3 ###############
# without fletxa and escrituras and zigzag; and arboriforme and estrealla and creu cristiana and pentacle

source("R/gravats_multi.R")
lthms <- list(# ico
  # anthropomorph
  thm_home = c("thm_xt", "personatge.*home"),
  thm_dona = c("thm_xt", "personatge.*dona"),
  thm_genet = c("thm_xt", "personatge.*cavall"),
  thm_ballester = c("thm_xt", "personatge.*ballesta"),
  thm_casc_barret_corona = c("thm_xt", "personatge.*casc/barret/corona"),
  # technomorph
  thm_lanca = c("thm_xt", "personatge.*llança"),
  thm_espasa = c("thm_xt", "personatge.*espasa"),
  thm_arc = c("thm_xt", "personatge.*arc"),
  thm_ballesta = c("thm_xt", "tècnic.*ballesta"),
  thm_cornamusa = c("thm_xt", "tècnic.*cornamusa"),
  # zoomorph
  thm_cavall = c("thm_xt", "zoomorf.*cavall"),
  # thm_cavall_equid = c("thm_xt", "zoomorf.*(?=.*èquid)(?=.*cavall)"), # TODO
  thm_ocell= c("thm_xt", "zoomorf.*ocell")
)
lg <- gravats_multi(gravats = gravats,
                    lthms = lthms,
                    pts.size = 2,
                    lbl.size = 3)
ggplot2::ggsave(paste0(outDir, "gthms_3_ca.png"), lg$mult_ca_plot, width = 15, height = 15, units = "cm")

## Multifact + boxes
source("R/dum_box.R")
# lbox <- list(c(1.2, 3, 0.5, 2.5), # zigzag and arboriforme
#              c(0, 2, -2, -.5), # creu_chritsiana estrell pentacle
#              c(-1.5, .5, -.5, 1) # other
# ) # , 
lbox <- list(list("ballesta", c(.2, 1.5, -2.2, -1)), # zigzag and arboriforme
             list("ocell_ballester", c(-2, -.5, -1.5, 0.2)), # 
             list("others", c(-.5, 1, -.5, 1.5)) # o
) # , 

# Draw boxes
gthms_3 <- dum_box(gplot = lg$mult_ca_plot,
                   lbox = lbox)
ggplot2::ggsave(paste0(outDir, "gthms_3_ca_box.png"), gthms_3, width = 15, height = 15, units = "cm")

# # Export separate boxes
# source("R/gravats_multi.R")
# lg <- gravats_multi(gravats = gravats,
#                     lthms = lthms,
#                     pts.size = 2,
#                     lbl.size = 3,
#                     ind.lbl.shw = TRUE,
#                     limit.coords = lbox[[i]][[2]])

# lg$mult_ca_plot
# Spatial distribution of selected rocks
source("R/roques_x_gravats.R")
for(i in 1:(length(lbox)-1)){
  # avoid others
  # limit to some individuals to spot a particular spatial distribution
  lg <- gravats_multi(gravats = gravats,
                      lthms = lthms,
                      pts.size = 2,
                      lbl.size = 3,
                      ind.lbl.shw = TRUE,
                      limit.coords = lbox[[i]][[2]])
  ggplot2::ggsave(paste0(outDir, "gthms_3_ca_", lbox[[i]][[1]],".png"), lg$mult_ca_plot, 
                  width = 11, height = 11, units = "cm")
  lg <- roques_x_gravats(roques = roques,
                         gravats = gravats,
                         limit_ind = as.character(lg$mult_ca_limit_ind$roques),
                         stats = "spats_limit_ind",
                         title.add = lbox[[i]][[1]])
  ggplot2::ggsave(paste0(outDir, "gthms_3_spat_", lbox[[i]][[1]],".png"), lg$spats_limit_ind, 
                  width = 11, height = 11, units = "cm")
}


########## 4 ###############
# without fletxa and escrituras and zigzag; and arboriforme and estrealla and creu cristiana and pentacle; ocell, ballesta, ballester

source("R/gravats_multi.R")
lthms <- list(# ico
  # anthropomorph
  thm_home = c("thm_xt", "personatge.*home"),
  thm_dona = c("thm_xt", "personatge.*dona"),
  thm_genet = c("thm_xt", "personatge.*cavall"),
  thm_casc_barret_corona = c("thm_xt", "personatge.*casc/barret/corona"),
  # technomorph
  thm_lanca = c("thm_xt", "personatge.*llança"),
  thm_espasa = c("thm_xt", "personatge.*espasa"),
  thm_arc = c("thm_xt", "personatge.*arc"),
  thm_cornamusa = c("thm_xt", "tècnic.*cornamusa"),
  # zoomorph
  thm_cavall = c("thm_xt", "zoomorf.*cavall")
  # thm_cavall_equid = c("thm_xt", "zoomorf.*(?=.*èquid)(?=.*cavall)"), # TODO
)
lg <- gravats_multi(gravats = gravats,
                    lthms = lthms,
                    pts.size = 2,
                    lbl.size = 3)
ggplot2::ggsave(paste0(outDir, "gthms_4_ca.png"), lg$mult_ca_plot, width = 15, height = 15, units = "cm")
lg$mult_ca_stat$eig
## Multifact + boxes
source("R/dum_box.R")
lbox <- list(list("cavall", c(-2.7, -1.3, -.5, .5)), # zigzag and arboriforme
             list("arc", c(.2, 1.6, 1.4, 6.4)), # 
             list("others", c(-.7, 1, -1, .8)) # o
)
# Draw boxes
gthms_4 <- dum_box(gplot = lg$mult_ca_plot,
                   lbox = lbox)
gthms_4
ggplot2::ggsave(paste0(outDir, "gthms_4_ca_box.png"), gthms_4, width = 15, height = 15, units = "cm")


source("R/roques_x_gravats.R")
# "length(lbox)-1" to avoid other, studied in the next level
for(i in 1:(length(lbox)-1)){
  # CA by box
  lg <- gravats_multi(gravats = gravats,
                      lthms = lthms,
                      pts.size = 2,
                      lbl.size = 3,
                      ind.lbl.shw = TRUE,
                      limit.coords = lbox[[i]][[2]])
  ggplot2::ggsave(paste0(outDir, "gthms_4_ca_", lbox[[i]][[1]],".png"), lg$mult_ca_plot, 
                  width = 11, height = 11, units = "cm")
  # Spatial
  lg <- roques_x_gravats(roques = roques,
                         gravats = gravats,
                         limit_ind = as.character(lg$mult_ca_limit_ind$roques),
                         stats = "spats_limit_ind",
                         title.add = lbox[[i]][[1]])
  ggplot2::ggsave(paste0(outDir, "gthms_4_spat_", lbox[[i]][[1]],".png"), lg$spats_limit_ind, 
                  width = 11, height = 11, units = "cm")
}

########## 5 ###############
# without fletxa and escrituras and zigzag; and arboriforme and estrealla and creu cristiana and pentacle; ocell, ballesta, ballester; arc, cavall

source("R/gravats_multi.R")
lthms <- list(# ico
  # anthropomorph
  thm_home = c("thm_xt", "personatge.*home"),
  thm_dona = c("thm_xt", "personatge.*dona"),
  thm_genet = c("thm_xt", "personatge.*cavall"),
  thm_casc_barret_corona = c("thm_xt", "personatge.*casc/barret/corona"),
  # technomorph
  thm_lanca = c("thm_xt", "personatge.*llança"),
  thm_espasa = c("thm_xt", "personatge.*espasa"),
  thm_cornamusa = c("thm_xt", "tècnic.*cornamusa")
  # thm_cavall_equid = c("thm_xt", "zoomorf.*(?=.*èquid)(?=.*cavall)"), # TODO
)
lg <- gravats_multi(gravats = gravats,
                    lthms = lthms,
                    pts.size = 2,
                    lbl.size = 3)
ggplot2::ggsave(paste0(outDir, "gthms_5_ca.png"), lg$mult_ca_plot, width = 15, height = 15, units = "cm")
lg$mult_ca_stat$eig
## Multifact + boxes
source("R/dum_box.R")
lbox <- list(list("dona", c(0, 1.5, 1.5, 5)), # zigzag and arboriforme
             list("cornamusa", c(1.5, 5, -1.5, .5)), # 
             list("others", c(-1, .5, -1, 1)) # o
)
# Draw boxes
gthms_5 <- dum_box(gplot = lg$mult_ca_plot,
                   lbox = lbox)
gthms_5
ggplot2::ggsave(paste0(outDir, "gthms_5_ca_box.png"), gthms_5, width = 15, height = 15, units = "cm")


source("R/roques_x_gravats.R")
# "length(lbox)-1" to avoid other, studied in the next level
for(i in 1:(length(lbox)-1)){
  # CA by box
  lg <- gravats_multi(gravats = gravats,
                      lthms = lthms,
                      pts.size = 2,
                      lbl.size = 3,
                      ind.lbl.shw = TRUE,
                      limit.coords = lbox[[i]][[2]])
  ggplot2::ggsave(paste0(outDir, "gthms_5_ca_", lbox[[i]][[1]],".png"), lg$mult_ca_plot, 
                  width = 11, height = 11, units = "cm")
  # Spatial
  lg <- roques_x_gravats(roques = roques,
                         gravats = gravats,
                         limit_ind = as.character(lg$mult_ca_limit_ind$roques),
                         stats = "spats_limit_ind",
                         title.add = lbox[[i]][[1]])
  ggplot2::ggsave(paste0(outDir, "gthms_5_spat_", lbox[[i]][[1]],".png"), lg$spats_limit_ind, 
                  width = 11, height = 11, units = "cm")
}