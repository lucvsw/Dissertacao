# Comentários: este arquivo produz a Figura: "Figure 2: Spatial Distribution of the Urbanized Area and Population in Brasília". A área aproximada ao lado de cada um dos mapas foi adicionada posteriormente, fora do R, para produzir a figura como está na dissertação.

# Pacotes necessários

library(httr)
library(readxl)
library(dplyr)
library(ggplot2)
library(colorspace)
library(sf)
library(geobr)
library(gridExtra)

# Dados georreferenciados de Brasília

url <- "https://raw.githubusercontent.com/lucvsw/Dissertacao/main/dados/bsb_new_data_painel.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Dados em painel
bsb_painel <- read_excel(temp_file)

# Filtrar os dados para o ano de 1975
bsb_1975 <- bsb_painel %>%
  filter(year == 1975)

# Filtrar os dados para o ano de 2015
bsb_2015 <- bsb_painel %>%
  filter(year == 2015)

# Shapefile do arranjo populacional de Brasília
## Arranjos populacionais
brasilia1_geobr <- read_municipality(code_muni = 5300108, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia2_geobr <- read_municipality(code_muni = 5215603, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia3_geobr <- read_municipality(code_muni = 5217609, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia4_geobr <- read_municipality(code_muni = 5200258, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia5_geobr <- read_municipality(code_muni = 5205497, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia6_geobr <- read_municipality(code_muni = 5212501, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia7_geobr <- read_municipality(code_muni = 5215231, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia8_geobr <- read_municipality(code_muni = 5219753, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia9_geobr <- read_municipality(code_muni = 5221858, year = 2015, simplified = TRUE, showProgress = FALSE)

# Crie uma lista com todos os objetos brasilia_geobr
bsb_geobr_list <- list(
  brasilia1_geobr, brasilia2_geobr, brasilia3_geobr,
  brasilia4_geobr, brasilia5_geobr, brasilia6_geobr,
  brasilia7_geobr, brasilia8_geobr, brasilia9_geobr
)

# Unindo shapefiles
arrange_sf_bsb <- do.call(rbind, bsb_geobr_list)

# Transformar as coordenadas para a projeção Mollweide (ESRI:54009)
new_shapefile_mollweide_bsb <- st_transform(arrange_sf_bsb, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Função para criar o mapa dos arranjos populacionais
criar_mapa <- function(data, fill_var, title = NULL, fill_name, fill_limits = NULL, show_legend = TRUE) {
  mapa <- ggplot() +
    geom_raster(aes(x = x, y = y, fill = !!sym(fill_var)), data = data) +
    geom_sf(data = new_shapefile_mollweide_bsb, fill = "transparent", color = "darkgrey", size = .7) +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    scale_fill_continuous_sequential(palette = "YlGnBu", name = fill_name, limits = fill_limits) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 1, vjust = 1),  # Título maior e à esquerda
      plot.caption = element_text(size = 12, hjust = 1.2, vjust = 14)  # Fonte maior e no canto inferior direito
    )
}

# Dados para os mapas de 1975
dens_map1975 <- criar_mapa(bsb_1975, "pop", NULL, "Pop.")
urb_map1975 <- criar_mapa(bsb_1975, "total_hec", NULL, "Ha. Ocup.")

# Dados para os mapas de 2015 sem legenda
dens_map2015 <- criar_mapa(bsb_2015, "pop", NULL, "Pop.")
urb_map2015 <- criar_mapa(bsb_2015, "total_hec", NULL, "Ha. Ocup.")

# Apresentar os plots de área urbanizada
grid.arrange(urb_map1975, urb_map2015,
             ncol=2)

# Apresentar os plots de densidade populacional
grid.arrange(dens_map1975, dens_map2015,
             ncol=2)

