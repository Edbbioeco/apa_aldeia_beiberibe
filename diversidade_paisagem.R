# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(ggview)

library(magick)

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

mapas <- purrr::pmap(
  list(raster_uso_trat,
       cores,
       classes,
       1985:2024),
  purrr::in_parallel(

    \(raster, cor, classe, ano){

      ggplot() +
        tidyterra::geom_spatraster(data = raster) +
        scale_fill_manual(
          values = cor,
          breaks = cor |> names(),
          labels = classe,
          na.translate = FALSE,
          guide = guide_legend(title.position = "top",
                               title.hjust = 0.5,
                               nrow = 3,
                               byrow = TRUE)) +
        geom_sf(data = apa,
                color = "black",
                fill = "transparent",
                linewidth = 1) +
        labs(title = paste0("Uso e Cobertura do solo para o ano de ", ano),
             subtitle = "Fonte: MapBiomas",
             fill = "Classes de uso e cobertura do solo") +
        coord_sf(expand = FALSE,
                 label_graticule = "NSWE") +
        theme_bw() +
        theme(axis.text = element_text(color = "black", size = 20),
              legend.text = element_text(color = "black", size = 20),
              legend.title = element_text(color = "black", size = 20),
              legend.position = "bottom",
              plot.title = element_text(color = "black", size = 30),
              plot.subtitle = element_text(color = "black", size = 30)) +
        ggview::canvas(height = 10, width = 12)

      }

    ),
  .progress = TRUE)

mapas

# Gif da evolução da paisagem ----

## Transformar a lista em um objeto para o pacote magick ----

imagens <- purrr::map(
  mapas,
  purrr::in_parallel(

    \(p){

      img <- magick::image_graph(height = 10 * 150,
                                 width = 12 * 150,
                                 res = 150)

      grid::grid.newpage()

      grid::grid.draw(ggplot2::ggplotGrob(p))

      dev.off()

      img

      }

    ),
  .progress = TRUE) |>
  magick::image_join()

imagens

## Criar o gif ----

gif_apa_uso <- imagens |> magick::image_animate(fps = 1)

gif_apa_uso

## Exportar gif ----

gif_apa_uso |>
  magick::image_scale("1280x1066!") |>
  magick::image_write("./apa_uso_cobertura.gif")

# Diversidade da paisagem ----

## Calcular diversidade ----

div_apa <- purrr::imap_dfr(
  raster_uso_trat,
  ~.x |>
      landscapemetrics::lsm_l_sidi() |>
      dplyr::mutate(Ano = .y |>
                      as.numeric()),
  .progress = TRUE) |>
  dplyr::select(6:7) |>
  dplyr::rename("Diversidade (D)" = 1)

div_apa

## Gráfico ----

div_apa |>
  ggplot(aes(Ano, `Diversidade (D)`)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2010, linewidth = 1, color = "darkgreen") +
  geom_label(data = tibble(Ano = 2010,
                           `Diversidade (D)` = 0.64),
             aes(Ano,
                 `Diversidade (D)`,
                 label = "Criação da APA Aldeia Beiberibe"),
           color = "black",
           fill = "green",
           size = 7.5) +
  scale_x_continuous(breaks = seq(1985, 2025, 5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        plot.title = element_text(color = "black", size = 30),
        plot.subtitle = element_text(color = "black", size = 30)) +
  ggview::canvas(height = 10, width = 12)
