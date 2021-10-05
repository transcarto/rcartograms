library(sf)
communes <- st_read("data/isere.geojson", quiet = TRUE ) %>% st_transform(2154)
data <- read.csv("data/popisrere.csv",  dec = ",")
communes = merge(x = communes[,c("id","name","geometry")], 
                 y = data[,c("id", "pop2018","agri", "art", "cadr", "interm", "emp","ouvr","retr")],
                 by = "id")
isere = st_union(communes)

library(recmap)

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

coords <- st_centroid(communes)
coords <- st_transform(coords, crs="+proj=longlat +datum=WGS84 +ellps=WGS84")

df_recmap <- sfc_as_cols(coords)

rec_cartogram <- data.frame (x= df_recmap$x,
                             y=df_recmap$y,
                             # make the rectangles overlapping by correcting lines of longitude distance
                             dx = sqrt(df_recmap$pop2018) / 90 / abs((0.65 * 60 * cos(df_recmap$y*pi/180))),
                             dy = sqrt(df_recmap$pop2018) / 90 / (0.65 * 60),
                             z = sqrt(df_recmap$pop2018),
                             name = df_recmap$id)

head(rec_cartogram)

plot.recmap(rec_cartogram, col = NA, border = "red", lwd=4,  col.text = col)


cartog <- recmap(df_recmap)

plot(cartog[!cartog$name %in% c("IS","MT"),],  col = col, border = "white")
