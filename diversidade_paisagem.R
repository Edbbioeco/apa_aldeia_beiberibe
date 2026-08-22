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

## Retirar os NULL ----

raster_uso_trat <- raster_uso |>
  purrr::compact() |>
  purrr::map(~.x |> terra::as.factor())

raster_uso_trat

## Visualizar ----

purrr::imap(raster_uso_trat,
            purrr::in_parallel(

              ~ggplot() +
                tidyterra::geom_spatraster(data = .x) +
                scale_fill_viridis_d(na.translate = FALSE) +
                labs(title = .y)

             ),
           .progress = TRUE)

# Mapas ----

## Settar cores ----

source("https://raw.githubusercontent.com/Edbbioeco/mapbiomas_classes/main/cor_classes_funcao.R")

cores <- purrr::map(raster_uso_trat,
                    ~vetorizar_cores(classes = .x |>
                                         terra::values() |>
                                         unique()),
                    .progress = TRUE)

cores

## Settar classes ----

source("https://raw.githubusercontent.com/Edbbioeco/mapbiomas_classes/main/nome_classe_funcao.R")

classes <- purrr::map(raster_uso_trat,
                      ~vetorizar_classes(classes = .x |>
                                           terra::values() |>
                                           unique()),
                      .progress = TRUE)

classes

## Visualizar ----

purrr::pmap(
  list(raster_uso_trat,
       cores,
       classes,
       1985:2024),
  purrr::in_parallel(

    \(raster, cor, classe, ano){

      ggplot() +
        tidyterra::geom_spatraster(data = raster) +
        scale_fill_manual(
          values = cores,
          labels = classe,
          breaks = classe,
          na.translate = FALSE,
          guide = guide_legend(title.position = "top",
                               title.hjust = 0.5)) +
        labs(title = ano) +
        coord_sf(expand = FALSE,
                 label_graticule = "NSWE") +
        theme_bw() +
        theme(axis.text = element_text(color = "black", size = 20),
              legend.text = element_text(color = "black", size = 20),
              legend.title = element_text(color = "black", size = 20),
              legend.position = "bottom") +
        ggview::canvas(height = 10, width = 12)

      }

    ),
  .progress = TRUE)
