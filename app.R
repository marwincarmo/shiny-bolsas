library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)

base <- readr::read_rds("data/cnpq_completo.rds")

ui <- fluidPage(
  
  tabPanel("Interactive map",
           div(class="outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 #includeScript("gomap.js")
               ),
               
               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leafletOutput("map", width="100%", height="100%"),
               
               # Shiny versions prior to 0.11 should use class = "modal" instead.
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h2("Mapa das bolsas"),
                             
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
                             
                             p("Este mapa mostra a distribuição das cidades de destino
                               de bolsas cedidas pelo cnpq. Utilizando os campos acima,
                               você pode filtrar os resultados selecionando um ou mais
                               critérios específicos. Caso nenhum filtro esteja selecionado, 
                               serão apresentados os resultados para todas as categorias 
                               e modalidades."),
                             p("Ao aproximar a visão do mapa, um marcador indicará cada
                               cidade para qual houve a destinação de alguma bolsa.
                               Ao clicar no marcador, você poderá ver o nome da cidade
                               e a quantidade de bolsas cedidas.")
                             
                             
                             )))

  )

server <- function(input, output, session) {
  
  all_categorias <- reactive(no_filter(input$categoria_bolsa, base$categoria))
  all_areas <- reactive(no_filter(input$area_bolsa, base$grande_area))
  
  base_mapa <- reactive(
    base %>%
    filter(ano_referencia %in% seq(min(input$ano_bolsa), max(input$ano_bolsa)),
           categoria %in% all_categorias(),
           grande_area %in% all_areas()) %>% 
    group_by(addr, latitude, longitude) %>% 
    summarise(bolsas_concedidas = sum(bolsas_concedidas))
  )
  
  observe({
    # Sys.sleep(3)
    escolha_modalidade <- base |>
      filter(categoria %in% input$categoria_bolsa) |>
      distinct(modalidade) %>% 
      pull(modalidade)
    updateSelectInput(
      session,
      "modalidade_bolsa",
      choices = escolha_modalidade
    )
  })
  
  output$map <- renderLeaflet({
    base_mapa <- base %>%
      filter(ano_referencia %in% seq(min(input$ano_bolsa), max(input$ano_bolsa)),
             categoria %in% all_categorias(),
             grande_area %in% all_areas()) %>% 
      group_by(addr, latitude, longitude) %>% 
      summarise(bolsas_concedidas = sum(bolsas_concedidas))
    
    base_mapa %>% 
      leaflet() %>% 
      addTiles() %>% 
      addMarkers(
        popup = paste0(
         '<b>Cidade:</b> ', base_mapa$addr, '<br>',
         '<b>Bolsas:</b> ', base_mapa$bolsas_concedidas, '<br>'
        ),
        clusterOptions = markerClusterOptions()
      )
  })
}

shinyApp(ui, server)