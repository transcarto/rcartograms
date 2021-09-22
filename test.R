library("sf")
library("mapsf")
cartogram = st_read("files/GastnerSeguy.gpkg")
mf_map(cartogram)
View(cartogram)



st_make_grid {sf}
