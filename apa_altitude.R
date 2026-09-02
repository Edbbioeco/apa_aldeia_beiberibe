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

## Visualizar  ----

elev

ggplot() +
  tidyterra::geom_spatraster(data = elev) +
  scale_fill_viridis_c(na.value = "transparent")

## Mapa ----

ggplot() +
  tidyterra::geom_spatraster(data = elev) +
  tidyterra::scale_fill_hypso_c(
    palette = "colombia_hypso",
    na.value = "transparent",
    direction = -1,
    guide = guide_colourbar(title = "Atitude (m)",
                            title.position = "top",
                            title.hjust = 0.5,
                            barheight = 2,
                            barwidth = 30,
                            frame.colour = "black",
                            ticks.colour = "black")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "./apa_altitude.png",
       height = 10, width = 12)

## Histograma dos valores de Altitude ----

### Criar data frame ----

df_histo <- elev |>
  terra::values() |>
  na.omit() |>
  as.data.frame() |>
  dplyr::rename("Altitude (m)" = 1)

df_histo

### Gráfico ----

df_histo |>
  ggplot(aes(`Altitude (m)`)) +
  geom_histogram(color = "black") +
  labs(y = "Quantidade de pixels") +
  scale_x_continuous(breaks = seq(0, 250, 50)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "./histograma_apa_altitude.png",
       height = 10, width = 12)
