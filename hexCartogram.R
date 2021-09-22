library("sf")
library("mapsf")

cartogram = st_read("files/GastnerSeguy.gpkg")

grid = st_make_grid(cartogram, cellsize = 2000, square = FALSE)
grid = st_sf(id = rep("",length(grid)), geometry = grid)
ctr = st_centroid(grid)
ids = cartogram[,"id"] %>% st_drop_geometry()

for(i in 1:nrow(grid)){
  x = st_within(x = ctr[i,], y = cartogram, sparse = TRUE, prepared = TRUE)
  id = ids[unlist(x),][1]
  if (length(id) == 0){id = NA}
  grid[i,"id"] = id
}
grid <- grid[!is.na(grid$id),]
plot(st_geometry(grid))

aggregate(x = grid, by = list("id"),mean)

x <- aggregate(x = grid, 
               by = list(grid$id),
               FUN = min)

plot(st_geometry(x))
