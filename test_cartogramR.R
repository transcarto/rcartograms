library("cartogramR")
library(sf)
communes <- st_read("data/isere.geojson", quiet = TRUE ) %>% st_transform(2154)
data <- read.csv("data/popisrere.csv",  dec = ",")
communes = merge(x = communes, y = data, by = "id")


# Pour choisir la taille de la grille de déormation (calcul un peu long)
precarto <- precartogramR(communes, method = "GastnerSeguyMore")
plot(precarto)
summary(precarto)
# on choisit donc a minima la grille telle que le minimum d'intersections est superieur ou egal a un (ici un pas de grille de 256 peut faire l'affaire)

View(communes)

# pour creer le cartogramme suivant le nombre d'habitants par commune avec la methode de gastner seguy et more 2018 (par defaut)
isere_cartogram_pop <- cartogramR(communes, count="pop2018", method="GastnerSeguyMore", options=list(L=256, grid=TRUE))