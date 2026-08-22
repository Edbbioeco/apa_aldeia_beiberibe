# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(ggview)

library(landscapemetrics)

library(gganimate)

# Shapefile da APA Aldeiba Beiberibe ----

## Importar ----

apa <- sf::st_read("./shapefiles/apa_aldeiabeberibe.shp")

## Visualizar ----

apa

ggplot() +
  geom_sf(data = apa, color = "black")

# Raster de uso e cobertura do solo ----

## Baixar ----

mirai::daemons(6)

raster_uso <- purrr::map(
  1985:2025,
  \(periodo){

    tryCatch({

      terra::rast(paste0(
        "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_",
        periodo,
        ".tif")) |>
        terra::crop(apa) |>
        terra::mask(apa)

      },
      error = \(e) {

        message("Erro no ano ", periodo, ": ", e$message) |>
          crayon::red()
        NULL

      })

    },
  .progress = TRUE) |>
  setNames(1985:2025 |> as.character())

mirai::daemons(0)
