library("sf")
library("mapsf")

communes <- st_read("data/isere.geojson", quiet = TRUE ) %>% st_transform(2154)
data <- read.csv("data/popisrere.csv",  dec = ",")
communes = merge(x = communes[,c("id","name","geometry")], 
                 y = data[,c("id", "pop2018","agri", "art", "cadr", "interm", "emp","ouvr","retr")],
                 by = "id")



if(!file.exists("files/grid.gpkg")){
  
  grid = st_make_grid(communes, cellsize = 1000, square = TRUE)
  grid = st_sf(id = c(1:length(grid)), geometry = grid)
  ids = communes[,"id"] %>% st_drop_geometry()
  ctr = st_centroid(grid)

  for(i in 1:nrow(grid)){
  x = st_within(x = ctr[i,], y = communes, sparse = TRUE, prepared = TRUE)
  id = ids[unlist(x),][1]
  if (length(id) == 0){id = NA}
  grid[i,"id"] = id
}

grid <- grid[!is.na(grid$id),]

for(i in 1:nrow(grid)){
  id = as.character(grid[i,"id"])[1]
  popAll = as.numeric(communes[communes$id == id,"pop2018"] %>% st_drop_geometry())
  count = nrow(grid[grid$id == id,])
  grid[i,"pop"] = popAll / count
}  

st_write(grid,"files/grid.gpkg", delete_layer=TRUE)
} else {
  grid = st_read("files/grid.gpkg")
}


mf_map(communes, col = "#CCCCCC", border = "white", lwd = 0.5, add = FALSE)
mf_map(grid, var = "pop", col = "red", border = "#6b4266", type = "prop",
       inches = 0.07, leg_title_cex = 1.2, leg_val_cex	= 0.8,
       leg_title = "Nombre d'habitants, 2018")

