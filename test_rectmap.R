library(sf)
library(recmap)


europe <- st_read("data/europe.geojson")

size = 0.5 # To change se size of rectangles
coords = data.frame(st_coordinates(st_centroid(st_geometry(europe))))
bb <- lapply(st_geometry(europe), function(x){st_bbox(x)})
dx <- unlist(lapply(bb, function(x){x[3]-x[1]})) * size
dy <- unlist(lapply(bb, function(x){x[4]-x[2]})) * size

df <- data.frame(x = coords$X, 
                 y = coords$Y, 
                 dx = dx, 
                 dy = dy, 
                 z = europe$pop2008,
                 name = europe$id)





plot.recmap(df, col = NA, border = "black",   col.text = 'black')

cartog <- recmap(df)
plot(cartog,  col = "black", border = "white")

