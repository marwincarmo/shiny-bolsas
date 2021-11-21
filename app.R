## pacotes ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinyjs)
  library(bs4Dash)
  #library(shinydashboard)
  library(leaflet)
  library(dplyr)
  library(ggplot2)
})

## database ----

base <- readr::read_rds("data/cnpq_completo.rds")

## Tabs ----

#source("ui/map_tab.R")
source("ui/sidebar.R")


ui <- dashboardPage(
  #fullscreen = TRUE,
  skin = 'blue',
  dashboardHeader(title = 'Bolsas CNPq'),
  sidebar = sidebar,
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map_tab",
        fluidPage(
          tabPanel("",
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
                                     
                                     p("Este mapa mostra a distribuição das cidades de destino
                               de bolsas cedidas pelo cnpq. Utilizando os campos acima,
                               você pode filtrar os resultados selecionando um ou mais
                               critérios específicos. Caso nenhum filtro esteja selecionado, 
                               serão apresentados os resultados para todas as categorias, 
                               modalidades e áreas."),
                               p("Ao aproximar a visão do mapa, um marcador indicará cada
                               cidade para qual houve a destinação de alguma bolsa.
                               Ao clicar no marcador, você poderá ver o nome da cidade
                               e a quantidade de bolsas cedidas."),
                               p("Adaptado de https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example")
                               
                       )
                   )
          )
          
        )
      ),
      tabItem(
        tabName = "area_tab"

        
      )
    )
  ),
  controlbar = dashboardControlbar(
    skin = "light",
    width = 300,
    pinned = TRUE,
    collapsed = FALSE,
    overlay = FALSE,
    controlbarMenu(
      id = "controlbarmenu",
      controlbarItem(
        title = "Area",
        ## período ----
        sliderInput(
          inputId = "date_range",
          label = "Escolha um período:",
          sep = "",
          step = 1,
          min = min(base$ano_referencia),
          max = max(base$ano_referencia),
          value = c(2016,2020)
        ),
        
        pickerInput(
          inputId = "grande_area",
          label = "Grande Area", 
          choices = sort(unique(base$grande_area)),
          options = list(
            title = "Escolha uma ou mais áreas"), 
          multiple = TRUE
        ),

        selectInput(inputId = "area_especifica", 
                    label = "Area específica",
                    choices = c("Carregando..." = ""),
                    selectize = TRUE,
                    multiple = TRUE),
        
        selectInput(inputId = "categoria_bolsa", 
                    label = "Categoria da bolsa",
                    choices = sort(unique(base$categoria)),
                    multiple = TRUE),
        
        selectInput(inputId = "modalidade_bolsa", 
                    label = "Modalidade da bolsa",
                    choices = c("Carregando..." = ""),
                    selectize = TRUE,
                    multiple = TRUE),
        actionBttn(
          inputId = "reset_area",
          label = "Limpar seleção",
          style = "simple", 
          color = "primary",
          icon = icon("trash"),
          size = "sm"
        )
      ),
      controlbarItem(
        title = "Localização",
        # selecionar país ----
        pickerInput(
          inputId = "pais_destino",
          label = "País de destino", 
          choices = sort(unique(base$pais_destino)),
          options = list(
            title = "Escolha um ou mais países"), 
          multiple = TRUE
        ),
        ## selecionar UF ----
        selectizeInput(inputId = "uf_destino", 
                    label = "Estado (apenas BR)",
                    choices = c("Carregando..." = ""),
                    multiple = TRUE),
        ## selecionar cidade ----
        selectizeInput(inputId = "cidade_destino", 
                    label = "Cidade (global)",
                    choices = c("Carregando..." = ""),
                    multiple = TRUE),
        ## selecionar instituicao ----
        selectizeInput(inputId = "inst_destino", 
                    label = "Instituição destino",
                    choices = c("Carregando..." = ""),
                    multiple = TRUE),
        
        ## limpar selecao localizacao
        actionBttn(
          inputId = "reset_local",
          label = "Limpar seleção",
          style = "simple", 
          color = "primary",
          icon = icon("trash"),
          size = "sm"
        )
      )
      
    )
    ),
  footer = dashboardFooter(
    left = a(
      href = "https://twitter.com/marwincarmo",
      target = "_blank", "@marwincarmo"
    ),
    right = "2021"
  )
)

server <- function(input, output, session) {
  
  react_categoria <- reactive(no_filter(input$categoria_bolsa, base$categoria))
  react_grande_area <- reactive(no_filter(input$grande_area, base$grande_area))
  react_area_especifica <- reactive(no_filter(input$area_especifica, base$area))
  react_modalidade <- reactive(no_filter(input$modalidade_bolsa, base$modalidade))
  
  observe({
    
    escolha_modalidade <- if(is.null(input$categoria_bolsa)) {
      
      base %>% 
        distinct(modalidade) %>% 
        pull() 
      
    } else {
      base %>% 
        filter(categoria %in% input$categoria_bolsa) %>% 
        distinct(modalidade) %>% 
        pull()
    }
    
    escolha_grande_area <- if(is.null(input$grande_area)) {
      
      base %>% 
        distinct(area) %>% 
        pull()
      
    } else {
      
      base %>% 
        filter(grande_area %in% input$grande_area) %>% 
        distinct(area) %>% 
        pull() 
      
    }
    
    escolha_uf <- if(is.null(input$pais_destino)) {
      
      base %>% 
        distinct(sigla_uf_destino) %>% 
        pull() %>% 
        sort()
      
    } else {
      base %>% 
        filter(pais_destino %in% input$pais_destino) %>% 
        distinct(sigla_uf_destino) %>% 
        pull() %>% 
        sort()
    }
    
    
    ## update modalidade bolsa ----
    updateSelectInput(
      session,
      "modalidade_bolsa",
      choices = escolha_modalidade
    )
    
    ## update area de pesquisa ----
    updateSelectInput(
      session,
      "area_especifica",
      choices = escolha_grande_area
    )
    
    ## update uf destino ----
    updateSelectizeInput(
      session,
      "uf_destino",
      choices = escolha_uf,
      server = TRUE
    )
    
    
    
  })
  
  ## update escolhas cidade ----
  observe({
    
    escolha_cidade <- if(is.null(input$uf_destino) &&
                         is.null(input$pais_destino)) {
      base %>% 
        distinct(addr) %>% 
        pull() %>% 
        sort()
      
    } else if (is.null(input$uf_destino) &&
               !is.null(input$pais_destino)) {
      base %>% 
        filter(pais_destino %in% input$pais_destino) %>% 
        distinct(addr) %>% 
        pull() %>% 
        sort()
      
      
    } else {
      base %>% 
        filter(sigla_uf_destino %in% input$uf_destino) %>% 
        distinct(addr) %>% 
        pull() %>% 
        sort()
    }
    
    ## update cidade destino ----
    updateSelectizeInput(
      session,
      "cidade_destino",
      choices = escolha_cidade,
      server = TRUE
    )
  })
  
  ## update escolhas instituição ----
  observe({
    
    escolha_inst <- if(is.null(input$uf_destino) &&
                       is.null(input$pais_destino) &&
                       is.null(input$cidade_destino)) {
      base %>% 
        distinct(instituicao_destino) %>% 
        pull() %>% 
        sort()
      
    } else if (is.null(input$uf_destino) &&
               is.null(input$cidade_destino) &&
               !is.null(input$pais_destino)) {
      base %>% 
        filter(pais_destino %in% input$pais_destino) %>% 
        distinct(instituicao_destino) %>% 
        pull() %>% 
        sort()
      
    } else if (!is.null(input$uf_destino) &&
               is.null(input$cidade_destino)) {
      base %>% 
        filter(sigla_uf_destino %in% input$uf_destino) %>% 
        distinct(instituicao_destino) %>% 
        pull() %>% 
        sort()
      
    } else {
      
      base %>% 
        filter(addr %in% input$cidade_destino) %>% 
        distinct(instituicao_destino) %>% 
        pull() %>% 
        sort()
    }
    
    ## update cidade destino ----
    updateSelectizeInput(
      session,
      "inst_destino",
      choices = escolha_inst,
      server = TRUE
    )
  })
  
  ## reset button area ----
  observeEvent(input$reset_area, {
    updateSliderInput(inputId = "date_range", value = c(2016,2020))
    updatePickerInput(session, inputId = "grande_area", selected = character(0))
    updateSelectInput(inputId = "categoria_bolsa", selected = character(0))
    updateSelectInput(inputId = "modalidade_bolsa", selected = character(0))
  })
  
  ## reset button localizacao ----
  observeEvent(input$reset_local, {
    updatePickerInput(session, inputId = "pais_destino", selected = character(0))
    updateSelectizeInput(inputId = "uf_destino", selected = character(0), server = TRUE)
    updateSelectizeInput(inputId = "cidade_destino", selected = character(0), server = TRUE)
    updateSelectizeInput(inputId = "inst_destino", selected = character(0), server = TRUE)
  })
  
  output$map <- renderLeaflet({
    base_mapa <- base %>%
      filter(ano_referencia %in% seq(min(input$date_range), max(input$date_range)),
             categoria %in% react_categoria(),
             grande_area %in% react_grande_area(),
             modalidade %in% react_modalidade(),
             area %in% react_area_especifica()
             ) %>% 
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