library(dplyr)
library(leaflet)
library(tidygeocoder)
library(htmltools)

base <- readr::read_rds("G:/Documentos/ProjetosR/shiny-bolsas-antigo/data/cnpq_completo.rds") 

cidades <- base %>% 
  select(cidade_destino, sigla_uf_destino) %>% 
  mutate(addr = case_when(
    is.na(sigla_uf_destino) ~ cidade_destino,
    TRUE ~ paste0(cidade_destino,", ", sigla_uf_destino))
              ) %>% 
  distinct() %>% 
  tidygeocoder::geocode(addr, method = 'osm',
                       lat = latitude, long = longitude) %>% 
  readr::write_rds("data/locations.rds")

base %>% 
  left_join(cidades2, by = c("cidade_destino", "sigla_uf_destino")) %>% 
  readr::write_rds("data/cnpq_completo.rds")

amostra <- base %>% 
  #sample_n(100) %>% 
  group_by(addr, latitude, longitude) %>% 
  summarise(bolsas_concedidas = sum(bolsas_concedidas))

amostra %>% select(addr, bolsas_concedidas) %>% tibble::view()
  
amostra %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(
  popup = ~htmlEscape(addr),
  clusterOptions = markerClusterOptions(
    spiderfyOnMaxZoom = FALSE,
    singleMarkerMode = TRUE,
    
  )
)


base %>% 
  sample_n(100) %>% 
  group_by(addr, latitude, longitude) %>% 
  summarise(bolsas_concedidas = sum(bolsas_concedidas))
amostra %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(
    popup = paste0(
      '<b>Cidade:</b> ', amostra$addr, '<br>',
      '<b>Bolsas:</b> ', amostra$bolsas_concedidas, '<br>'
    ),
    clusterOptions = markerClusterOptions(
      #group = ~bolsas_concedidas
      #spiderfyOnMaxZoom = FALSE,
      #singleMarkerMode = TRUE,
      
    )
  )
  
  # addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
  #            radius = ~bolsas_concedidas, popup = ~addr
  # )
base %>% 
  filter(categoria %in% "Mestrado") %>% 
  distinct(modalidade) %>% 
  pull(modalidade)



# caixinha flutuante ------------------------------------------------------

sliderInput(
  inputId = "ano_bolsa",
  label = "Escolha um período:",
  sep = "",
  step = 1,
  min = min(base$ano_referencia),
  max = max(base$ano_referencia),
  value = c(2016,2020)
),

selectInput(inputId = "categoria_bolsa", 
            label = "Categoria da bolsa",
            choices = sort(unique(base$categoria)),
            multiple = TRUE),

selectInput(inputId = "modalidade_bolsa", 
            label = "Modalidade da bolsa",
            choices = c("Carregando..." = ""),
            selectize = TRUE,
            multiple = TRUE),

selectInput(inputId = "area_bolsa", 
            label = "Grande Área",
            choices = sort(unique(base$grande_area)),
            multiple = TRUE),
