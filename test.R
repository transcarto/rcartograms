library(sf)
communes <- st_read("data/isere.geojson", quiet = TRUE ) %>% st_transform(2154)
data <- read.csv("data/popisrere.csv",  dec = ",")
communes = merge(x = communes[,c("id","name","geometry")], 
                 y = data[,c("id", "pop2018","agri", "art", "cadr", "interm", "emp","ouvr","retr")],
                 by = "id")
isere = st_union(communes)


c <- st_centroid(communes)
v <- st_voronoi(x = st_union(c))
v <- st_intersection(st_cast(c), st_union(communes))
v <- st_join(x = st_sf(v), y = c, join=st_intersects)
v <- st_cast(v, "MULTIPOLYGON")


View(v)

v <- st_voronoi(x = st_union(ctr))




v = st_voronoi(x, NULL, dTolerance = 0, bOnlyEdges = FALSE)
v <- st_intersection(st_cast(v), st_union(communes))
x <- st_join(x = st_sf(x), y = mtq_c, join=st_intersects)


x = st_centroid(st_geometry(communes))
bb = st_as_sfc(st_bbox(communes))
  
x = st_sfc(st_voronoi(x,bb))

plot(st_geometry(x))

v <- st_voronoi(d)

v = st_voronoi(do.call(ctr, bb))



bb = st_as_sfc(st_bbox(communes))
ctr = st_centroid(st_geometry(communes))
test = st_voronoi(ctr, bb)

plot(st_geometry(isere))
plot(st_geometry(test), add= T, col = NA, border = "red")

plot(st_geo)

col = "#c291bc"

library(remotes)
install_github("riatelab/fisheye")

library(fisheye)

ctr = communes[communes$name=="Grenoble",]

bb <- st_bbox(communes)
grid = st_make_grid(communes, cellsize = 5000, square = TRUE)
grid = st_sf(id = rep("",length(grid)), geometry = grid)

k = 2

communes_fisheye <- fisheye(communes, centre = ctr, method = 'log', k = 2)
grid_fisheye  <- fisheye(grid, centre = ctr, method = 'log', k = 2)
plot(st_geometry(communes_fisheye), border = "white", col = col)
plot(st_geometry(communes_fisheye[communes_fisheye$name == "Grenoble",]), border = "white", col = "#cf5da0", lwd = 0.5,  add= TRUE)
plot(st_geometry(grid_fisheye), border = "#40393d", lwd = 0.3, add = TRUE)




library(magick)
