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
        tabName = "map_tab"
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
                    multiple = TRUE)
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
                       multiple = TRUE)
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
  
}

shinyApp(ui, server)