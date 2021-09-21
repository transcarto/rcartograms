
getwd()

# install.packages("sf")
# install.packages("mapsf")

# install.packages("cartogram")
# install.packages("recmap")

# install.packages("countrycode")
# 
# install.packages("rmapshaper")
# install.packages("osmdata")
# install.packages("eurostat")

# install.packages("rnaturalearth")

# install.packages("dplyr")                       
# install.packages("tidyverse")



library("dplyr")
library("tidyverse")
library("sf")
library("mapsf")







################################################################################################
#                                                                                              #
#                                   CARTOGRAPHIE EUROPE                                        #  
#                                                                                              #
################################################################################################

library(eurostat) # Données Europe

# Préparation des fonds de carte Europe *******************************************************

# TEST FOND 2021 - pb de fusion sur les NUTS (Norway...)

# geo <- "eurostat_geodata_60_2016"
E2016_N3 <- get_eurostat_geospatial(output_class = "sf",
                                    resolution = "60",
                                    nuts_level = "3",
                                    year = "2016",
                                    cache = TRUE,
                                    update_cache = FALSE,
                                    cache_dir = NULL,
                                    crs = "3035")

E2016_N3 <- subset(E2016_N3,
                   !E2016_N3$NUTS_ID %in%
                     grep(pattern = "^FRY|PT200|PT300|^ES70", E2016_N3$NUTS_ID, value = TRUE, fixed=FALSE)
) #Suppression des régions éloignées


mf_map(E2016_N3)
# mf_label(x = E2016_N3, var = "NUTS_ID", halo=TRUE, cex = 0.4)
# summary(E2016_N3)



E2016_N2 <- get_eurostat_geospatial(output_class = "sf",
                                    resolution = "60",
                                    nuts_level = "2",
                                    year = "2016",
                                    cache = TRUE,
                                    update_cache = FALSE,
                                    cache_dir = NULL,
                                    crs = "3035")

E2016_N2 <- subset(E2016_N2,
                   !E2016_N2$NUTS_ID %in%
                     grep(pattern = "^FRY|PT20|PT30|ES70", E2016_N2$NUTS_ID, value = TRUE, fixed=FALSE)
) #Suppression des régions éloignées

mf_map(E2016_N2)
# mf_label(x = E2016_N2, var = "NUTS_ID", halo=TRUE, cex = 0.4)

E2016_N1 <- get_eurostat_geospatial(output_class = "sf",
                                    resolution = "60",
                                    nuts_level = "1",
                                    year = "2016",
                                    cache = TRUE,
                                    update_cache = FALSE,
                                    cache_dir = NULL,
                                    crs = "3035")

E2016_N1 <- subset(E2016_N1, !E2016_N1$NUTS_ID %in%
                     grep(pattern = "FRY|PT2|PT3|ES7", E2016_N1$NUTS_ID, value = TRUE, fixed=FALSE)
) #Suppression des régions éloignées

mf_map(E2016_N1)
# list(E2016_N1$CNTR_CODE)

E2016_N0 <- aggregate(x = E2016_N1[,], by = list(E2016_N1$CNTR_CODE), FUN = "mean")[,1]
colnames(E2016_N0) <- c("NUTS_ID" , "geometry")
mf_map(E2016_N0)


#  Données Europe ------------------------------------------------------------------------------
var <- "demo_r_pjangrp3" # Table Eurostat d'intérêt
data <- get_eurostat(var, time_format = "num") # Telecharger la table ESTAT
data <- subset(data, data$sex == "T") # Filtre des dimensions du tibble
data <- subset(data, data$age == "TOTAL") # Filtre des dimensions du tibble

data <- data %>%
  pivot_wider(names_from = "time", values_from = "values")
data <- subset(data[,4:11])
colnames(data) <- c("geo","P2020","P2019","P2018","P2017","P2016","P2015","P2014") # Renommer les colonnes du dataframe de façon explicite

# Surface des entités à partir des données eurostat utilisation du sol
var <- "reg_area3"
E_area <- get_eurostat(var, time_format = "num") # Telecharger la table ESTAT
E_area <- subset(E_area, E_area$landuse == "TOTAL") # Filtre des dimensions de la table
E_area <- E_area %>%
  pivot_wider(names_from = "time", values_from = "values")
E_area <- subset(E_area[,3:4])
colnames(E_area) <- c("geo","KM2016")

############################################
#                                          #
#       PAKAGE CARTOGRAM                   #
#                                          
############################################

# Données complètes ********************************************************************************

# Jointure fond + surface + population

E_data <- merge (E2016_N1, data, by="geo")
E_data <- merge (E_data, E_area, by="geo")

mf_map(x = E_data, var = "P2017", type = "choro")



library("cartogram")
dougenik <- cartogram_cont(E_data, "P2019")
mf_map(dougenik)


dorling <- cartogram_dorling(E_data, "P2019", k = 5, m_weight = 1, itermax = 1000)
mf_map(dorling, col = "red")


olson <- cartogram_ncont(E_data, "P2019")
mf_map(E_data)
mf_map(olson, col = "red", add= T)



############################################
#                                          #
#           SIZE ERROR                     #
#                                          
############################################

# calcul détaillé
dougenik$P2019_PC <- dougenik$P2019/sum(dougenik$P2019) # Part de la variable sur le total
dougenik$surf_theor <- dougenik$P2019_PC * sum(dougenik$KM2016) # Surface théorique
dougenik$surf_calc <- as.numeric(st_area(dougenik))/1000000 # surface calculée par l'algorithme de dougenik, chrisman et niemeyer
dougenik$size_error <- dougenik$surf_theor / dougenik$surf_calc * 100


# Cartographie des erreurs


bks <- mf_get_breaks(x = dougenik$size_error,
                     nbreaks = 7,
                     breaks = c(min(dougenik$size_error),70,80,90,100,110,120,max(dougenik$size_error)) # découpage comme dans scapetoad
        )
pal <- c("#5b5099", "#857ab3", "#b1aad0", "#dedaec", "#facfbb", "#f29979","#e95f40") #couleurs scapetoad

mf_map(x = dougenik,
       var= "size_error",
       type = "choro",
       breaks = bks,
       pal = pal)

mf_layout (title = "Cartographie des erreurs de surface",
           credits = "Source Eurostat\n
           Cartogramme continu : algorithme de Dougenik, Chrisman, Niemeyer\n
           Package Cartogram\n
           Team Transcarto, oct. 2021")

# !!!! ATTENTION calcul spé côté sardaigne sicile (essayer de comprendre)

# découpage centré autour de 95-105 (permet de ne pas séparer artificiellement les surfaces correctes)


bks <- mf_get_breaks(x = dougenik$size_error,
                     nbreaks = 7,
                     breaks = c(min(dougenik$size_error),50,75,95,105,150,200,max(dougenik$size_error)) # découpage comme dans scapetoad
)
pal <- c("#5555aa","#9f9fc9","#c3c3d9","#e8e8e8","#EEAEAE","#F47474","#ff0000")

mf_map(x = dougenik,
       var= "size_error",
       type = "choro",
       breaks = bks,
       pal = pal)

mf_layout (title = "Cartographie des erreurs de surfaces",
           credits = "Source Eurostat\n
           Cartogramme continu : algorithme de Dougenik, Chrisman, Niemeyer\n
           Package Cartogram\n
           Team Transcarto, oct. 2021")



##################################################################

#                          TEST RECMAP                           #

##################################################################

# J'avoue qu'au niveau paramétrage j'y arrive pas, mon bagage, math et trigo est trop faible

centro <- st_centroid(E_data)
centro <- st_transform(centro, crs="+proj=longlat +datum=WGS84 +ellps=WGS84")


sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}
centro <- sfc_as_cols(centro)

library(recmap)


map <- checkerboard

rec_cartogram <- data.frame (x= centro$x,
                             y=centro$y,
                             # make the rectangles overlapping by correcting lines of longitude distance
                             dx = sqrt(centro$P2019) / 50 / abs((0.65 * 60 * cos(centro$y*pi/180))),
                             dy = sqrt(centro$P2019) / 50 / (0.65 * 60),
                             z = sqrt(centro$P2019),
                             name = centro$NUTS_ID)


plot.recmap(rec_cartogram,col.text = 'black', lwd=2)
