## Pour creer un cartogramme avec le package en developpement cartogramR de Pierre-Andre Cornillon et Florent Demoraes ##

# Chargement packages (doivent etre a jour)
library(cartogramR) # version 1.0-0
library(sf) # version 0.9-8
library(data.table) # version 1.14.0

# NB : FFTW doit avoir ete installe au prealable

data(france_dept) # objet au format sf

# pour choisir la taille de la grille de déformation (calcul un peu long)
precarto <- precartogramR(france_dept, method = "GastnerSeguyMore") # calcul du nombre minimal d'intersections de la grille par unite spatiale 
plot(precarto) # nombre minimal d'intersections en fonction du pas de la grille
# on choisit donc a minima la grille telle que le minimum d'intersections est superieur ou egal a un (ici un pas de grille de 256 peut faire l'affaire)

# pour creer le cartogramme suivant le nombre d'habitants par departement avec la methode de gastner seguy et more 2018 (par defaut)
france_cartogram_pop <- cartogramR(france_dept, count="pop2020", method="GastnerSeguyMore", options=list(L=256, grid=TRUE))

# pour afficher les surfaces initiales et finales dans la table attributaire issue des departements
View(cbind(france_cartogram_pop$initial_data, france_cartogram_pop$orig_area, france_cartogram_pop$final_area))

# pour calculer les residus (erreur relative : taille finale / taille theorique * 100)
summary(residuals(france_cartogram_pop, type = "relative error")*100)

# Parametrage des marges de la fenetre pour maximiser l'emprise de la carte
par(mar = c(0, 0, 0, 0))

# pour creer un objet contenant la grille de deformation
final_graticule <-make_layer(france_cartogram_pop, type = c("final_graticule"))
plot(final_graticule, border = "grey90", lwd = 0.001, lty =3) # affichage un peu long

# pour afficher le cartogramme
plot(france_cartogram_pop, border = "red", add=TRUE)

# pour creer et tracer les centroides finaux et les vecteurs de translation
final_centers<-make_layer(france_cartogram_pop, type = c("final_centers"))
plot(final_centers, add=TRUE)
centers_movement<-make_layer(france_cartogram_pop, type = c("centers_movement"))
plot(centers_movement, add=TRUE)

# pour exporter le cartogramme au format geopackage (attention deux champs ne peuvent avoir le même nom)
st_write(as.sf(france_cartogram_pop),"france_cartogram_pop.gpkg", delete_layer=TRUE)
