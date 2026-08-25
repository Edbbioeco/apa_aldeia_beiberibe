# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(ggview)

library(magick)

library(gganimate)

# Shapefile da APA Aldeiba Beiberibe ----

## Importar ----

apa <- sf::st_read("./shapefiles/apa_aldeiabeberibe.shp")

## Visualizar ----

apa

ggplot() +
  geom_sf(data = apa, color = "black")
