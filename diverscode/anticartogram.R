library("sf")
library("mapsf")

communes <- st_read("data/isere.geojson", quiet = TRUE ) %>% st_transform(2154)
data <- read.csv("data/popisrere.csv",  dec = ",")
communes = merge(x = communes[,c("id","name","geometry")], 
                 y = data[,c("id", "pop2018","agri", "art", "cadr", "interm", "emp","ouvr","retr")],
                 by = "id")
isere = st_union(communes)

library(cartogramR)

# --------------------------------------------------------------

if(!file.exists("files/InvGastnerSeguy.gpkg")){
  communes$pop2018inv = round(1/ communes$pop2018 * 1000000)
  AntiGastnerSeguy <- cartogramR(communes, count="pop2018inv", method="gsm", options=list(L=256, grid=TRUE, maxit = 5))
  st_write(as.sf(AntiGastnerSeguy),"files/InvGastnerSeguy.gpkg", delete_layer=TRUE)
} else {
  AntiGastnerSeguy = st_read("files/InvGastnerSeguy.gpkg")
}