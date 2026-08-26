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

# Área de Mata ----

## Códigos das áreas de mata ----

codigos <- c(1:6, 10:12, 29, 32, 49:50) |> as.character()

codigos

## Filtrar ----

raster_mata <- purrr::imap(
  raster_uso_trat,
  ~.x |>
      tidyterra::mutate(
        !!{{paste0("brazil_coverage_", .y)}} := dplyr::case_when(

          .data[[paste0("brazil_coverage_", .y)]] %in%
            (codigos |> as.numeric()) ~ "Mata",
          .default = .data[[paste0("brazil_coverage_", .y)]] |> as.character()

        )
      ) |>
      tidyterra::filter(
        .data[[paste0("brazil_coverage_", .y)]] == "Mata"),
  .progress = TRUE)

raster_mata

## Visualizar ----

mapas_mata <- purrr::imap(
  raster_mata,
  \(raster, ano){

    ggplot() +
      geom_sf(data = apa,
              color = "black",
              linewidth = 1) +
    tidyterra::geom_spatraster(data = raster) +
    scale_fill_manual(values = "darkgreen",
                      na.translate = FALSE) +
    geom_sf(data = apa,
            color = "black",
            fill = "transparent",
            linewidth = 1) +
    labs(title = paste0("Área de Mata APA Aldeia Beiberibe para o ano de ", ano),
         subtitle = "Fonte: MapBiomas",
         fill = NULL) +
    coord_sf(expand = FALSE,
             label_graticule = "NSWE") +
    theme_bw() +
    theme(axis.text = element_text(color = "black", size = 20),
          legend.text = element_text(color = "black", size = 20),
          legend.title = element_text(color = "black", size = 20),
          legend.position = "bottom",
          plot.title = element_text(color = "black", size = 30,
                                    hjust = 0.5),
          plot.subtitle = element_text(color = "black", size = 30,
                                       hjust = 0.5)) +
    ggview::canvas(height = 10, width = 12)

    },
  .progress = TRUE)

mapas_mata
