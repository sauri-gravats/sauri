
outDir = "C:/Rprojects/sauri/doc/"

# read
source("R/roques_read.R")
source("R/gravats_read.R")
roques <- roques_read(roques = "C:/Rprojects/sauri/doc/data/sources/240415/Thomas_Sauri.gpkg",
                      gpkg.layer = "Roques")
gravats <- gravats_read(gravats = "C:/Rprojects/sauri/doc/data/sources/240415/llista_gravats_19-20-23_DEFINITIVA_FINAL_FINAL.xlsx")

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
  thm_cavalier = c("thm", "A.?h"),
  thm_arbaletrier = c("thm", "A.?b"),
  thm_cheval = c("thm", "Z.?h"),
  thm_oiseau = c("thm", "Z.?b"),
  thm_fleche = c("thm", "T.?f"),
  thm_arbalete = c("thm", "T.?b"),
  thm_cornemuse = c("thm", "T.?m"),
  thm_pentacle = c("thm", "G.?p"),
  thm_croix_chretienne = c("thm", "G.?k"),
  thm_ecritures = c("thm", "^L"),
  tec_naviforme = c("tec", ".?n.?"),
  lat_droite = c("lat", "->"),
  lat_gauche = c("lat", "<-")
)
lg <- roques_x_gravats(roques = roques,
                       gravats = gravats,
                       lthms = lthms)
# ggplot2::ggsave(paste0(outDir, "_spat_typogravats.png"), lg$spats_tema, width = 21, height = length(lthms)*3, units = "cm")

############# Engraved rocks #####################################

source("R/roques_desc.R")
lg <- roques_desc(roques = roques)
# lg$map_leaflet

############# Engravings #####################################
# Descriptive
source("R/gravats_desc.R")
lg <- gravats_desc(gravats = gravats)
# ggsave(paste0(outDir, "_spat_typogravats.png"), lg$grav_tec, width = 14, height = 14, units = "cm")

# Multifact
source("R/gravats_multi.R")
lg <- gravats_multi(gravats = gravats)
# ggsave(paste0(outDir, "_multi_ca_main_thm.png"), lg$mult_ca, width = 15, height = 15, units = "cm")


