# Pacotes ----

library(sf)

library(tidyverse)

library(elevatr)

library(terra)

library(tidyterra)

library(ggview)

# Shapefile da APA Beiberibe ----

## Importar ----

apa <- sf::st_read("./shapefiles/apa_aldeiabeberibe.shp")

## Visualizar ----

apa

ggplot() +
  geom_sf(data = apa, color = "black")

# Altitude ----

## Baixar dados de altitude ----

  elev <- elevatr::get_aws_terrain(locations = apa,
                                 prj = apa |> sf::st_crs(),
                                 z = 14,
                                 clip = "locations") |>
  terra::mask(apa) |>
  terra::crop(apa)
