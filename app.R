# pacotes ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinyjs)
  library(bs4Dash)
  #library(shinydashboard)
  library(leaflet)
  library(dplyr)
  library(reactable)
  library(echarts4r)
})

# database ----

base <- readr::read_rds("data/cnpq_completo.rds")
base_tabela <- readr::read_rds("data/cnpq_tabela.rds")

# Tabs ----

#source("ui/map_tab.R")
source("ui/sidebar.R")


ui <- dashboardPage(
  #fullscreen = TRUE,
  skin = 'blue',
  dashboardHeader(title = dashboardBrand(
    title = 'Bolsas CNPq',
    image = '<i class="fas fa-atom"></i>',
    href = 'https://github.com/marwincarmo/shiny-bolsas'
    )
    ),
  sidebar = sidebar,
  dashboardBody(
    tabItems(
      tabItem(
        # Aba mapa
        tabName = "map_tab",
        fluidPage(
          tabPanel("",
                   div(class="outer",
                       # fonte: https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example
                       tags$head(
                         # Include our custom CSS
                         includeCSS("styles.css"),
                         #includeScript("gomap.js")
                       ),
                       
                       # If not using custom CSS, set height of leafletOutput to a number instead of percent
                       leafletOutput("map", width="100%", height="100%"),
                       
                       # Shiny versions prior to 0.11 should use class = "modal" instead.
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, top = 60, left = "auto", right =  "auto", bottom = "auto",
                                     width = 330, height = 330,
                                     
                                     h2("Mapa das bolsas"),
                                     
                                     p("Este mapa mostra a distribuição das cidades de destino
                               de bolsas cedidas pelo CNPq. Ao aproximar a visão do mapa, um marcador indicará cada
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
        # Aba geral ----
        tabName = "geral_tab",
        
        fluidRow(
          ## value box com resumo da frequencia de bolsas ----
          shinydashboard::valueBoxOutput("num_total", width = 2),
          shinydashboard::valueBoxOutput("num_ic", width = 2),
          shinydashboard::valueBoxOutput("num_mestrado", width = 2),
          shinydashboard::valueBoxOutput("num_doutorado", width = 2),
          shinydashboard::valueBoxOutput("num_posdoc", width = 2),
          shinydashboard::valueBoxOutput("num_outro", width = 2)
        ),
        ## linha do tempo ----
        fluidRow(
          box(
            title = "Linha do Tempo",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            radioGroupButtons(
              inputId = "escolha_grafico",
              label = "",
              choices = c("Bolsas concedidas", "Valor pago"),
              individual = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: steelblue"))
            ),
            echarts4rOutput("timeline")
            
          )
          
        ),
        ## gráficos de ranking ----
        fluidRow(
          column(width = 6,
                 box(
                   width = NULL,
                   title = "Ranking por Cidade",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   echarts4rOutput("ranking_cidade")
                   )
                 ),
          column(width = 6,
                 box(
                   width = NULL,
                   title = "Ranking por Instituição",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   echarts4rOutput("ranking_instituicao")
                 ))
        ),
        ## tabela de dados gerais ----
        fluidRow(
          box(title = "Tabela de dados",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              reactable::reactableOutput("tabela_geral"))
        )
        
      )
    )
  ),
  # control bar ----
  controlbar = dashboardControlbar(
    skin = "light",
    width = 300,
    pinned = TRUE,
    collapsed = FALSE,
    overlay = FALSE,
    controlbarMenu(
      id = "controlbarmenu",
      controlbarItem(
        ## periodo/ area
        title = "Período/ Área",
        ### período ----
        sliderInput(
          inputId = "date_range",
          label = "Escolha um período:",
          sep = "",
          step = 1,
          min = min(base$ano_referencia),
          max = max(base$ano_referencia),
          value = c(2016,2020)
        ),
        ### grande area ----
        pickerInput(
          inputId = "grande_area",
          label = "Grande Area", 
          choices = sort(unique(base$grande_area)),
          options = list(
            title = "Escolha uma ou mais áreas"), 
          multiple = TRUE
        ),
        ### area especifica ----
        selectInput(inputId = "area_especifica", 
                    label = "Area específica",
                    choices = c("Carregando..." = ""),
                    selectize = TRUE,
                    multiple = TRUE),
        ### categoria da bolsa ----
        selectInput(inputId = "categoria_bolsa", 
                    label = "Categoria da bolsa",
                    choices = sort(unique(base$categoria)),
                    multiple = TRUE),
        ### modalidade da bolsa ----
        selectInput(inputId = "modalidade_bolsa", 
                    label = "Modalidade da bolsa",
                    choices = c("Carregando..." = ""),
                    selectize = TRUE,
                    multiple = TRUE),
        ### botao limpar selecao area ----
        actionBttn(
          inputId = "reset_area",
          label = "Limpar seleção",
          style = "simple", 
          color = "primary",
          icon = icon("trash"),
          size = "sm"
        )
      ),
      ## local
      controlbarItem(
        title = "Local",
        ### selecionar país ----
        pickerInput(
          inputId = "pais_destino",
          label = "País de destino", 
          choices = sort(unique(base$pais_destino)),
          options = list(
            title = "Escolha um ou mais países"), 
          multiple = TRUE
        ),
        ### selecionar UF ----
        selectizeInput(inputId = "uf_destino", 
                    label = "Estado",
                    choices = c("UFs brasileiras" = ""),
                    multiple = TRUE),
        ### selecionar cidade ----
        selectizeInput(inputId = "cidade_destino", 
                    label = "Cidade",
                    choices = c("Todas as cidades" = ""),
                    multiple = TRUE),
        ### selecionar instituicao ----
        selectizeInput(inputId = "inst_destino", 
                    label = "Instituição destino",
                    choices = c("Todas as instituições." = ""),
                    multiple = TRUE),
        
        ### limpar selecao localizacao ----
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
  ## footer ----
  footer = dashboardFooter(
    left = a(
      href = "https://marwincarmo.github.io/",
      target = "_blank", "Marwin M I B Carmo"
    ),
    right = a(
      href = "http://dadosabertos.cnpq.br/pt_BR/organization/cnpq",
      target = "_blank", "Fonte: Portal de dados abertos CNPq"
    )
  )
)

server <- function(input, output, session) {
  
  ## funcao no_filter ----
  ## se não tiver seleção no input, retorna todos os valores
  no_filter <- function(input, val) {
    if (is.null(input)) {
      unique(val)
    } else {
      input
    }
  }
  
  base_filtrada <- reactive({
    
    base %>% 
      filter(ano_referencia %in% seq(min(input$date_range), max(input$date_range)),
             categoria %in% no_filter(input$categoria_bolsa, base$categoria),
             grande_area %in% no_filter(input$grande_area, base$grande_area),
             modalidade %in% no_filter(input$modalidade_bolsa, base$modalidade),
             area %in% no_filter(input$area_especifica, base$area),
             pais_destino %in% no_filter(input$pais_destino, base$pais_destino),
             sigla_uf_destino %in% no_filter(input$uf_destino, base$sigla_uf_destino),
             addr %in% no_filter(input$cidade_destino, base$addr),
             instituicao_destino %in% no_filter(input$inst_destino, base$instituicao_destino),
             
             
      ) %>% 
      group_by(addr, latitude, longitude) %>% 
      summarise(bolsas_concedidas = sum(bolsas_concedidas))
    
  })

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
  
  ## output mapa ----
  output$map <- renderLeaflet({

    base_filtrada() %>% 
      leaflet() %>% 
      addTiles() %>% 
      addMarkers(
        popup = paste0(
        '<b>Cidade:</b> ', pull(base_filtrada(), addr), '<br>',
        '<b>Bolsas:</b> ', pull(base_filtrada(), bolsas_concedidas), '<br>'
        ),
        clusterOptions = markerClusterOptions()
      )
  })
  
  ## value box total ----
  output$num_total <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      0, "Bolsas no total",
      color = "purple"
    )
  })
  
  ## value box ic ----
  output$num_ic <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      0, "Bolsas de IC",
      color = "purple"
    )
  })
  ## value box mestrado ----
  output$num_mestrado <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      0, "Bolsas de Mestrado",
      color = "purple"
    )
  })
  ## value box doutorado ----
  output$num_doutorado <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      0, "Bolsas de Doutorado",
      color = "purple"
    )
  })
  ## value box posdoc ----
  output$num_posdoc <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      0, "Bolsas de Pós-doc",
      color = "purple"
    )
  })
  ## value box outras ----
  output$num_outro <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      0, "Bolsas de outra categoria",
      color = "purple"
    )
  })
  
  ## output grafico de linha do tempo ----
  
  output$timeline <- renderEcharts4r({
    
    base %>% 
      sample_n(1000) %>% 
      group_by(ano_referencia, categoria) %>% 
      summarise(bolsas_concedidas = sum(bolsas_concedidas), .groups = "drop") %>% 
      group_by(ano_referencia) %>% 
      tidyr::nest() %>% 
      ungroup() %>% 
      mutate(data = purrr::map(data, janitor::adorn_totals, "row")) %>% 
      tidyr::unnest(data) %>% 
      group_by(categoria) %>% 
      mutate(ano_referencia = as.character(ano_referencia)) %>% 
      e_charts(ano_referencia) %>%
      e_line(bolsas_concedidas) %>% 
      #e_x_axis(min = 2010, formatter = e_axis_formatter(digits = 0, locale = "pt-BR")) %>% 
      e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>% 
      e_axis_labels(x = "Ano", y = "Bolsas concedidas") %>% 
      e_title("Linha do tempo", left = 'center') %>% 
      e_tooltip(trigger = "axis", 
                formatter = e_tooltip_pointer_formatter(digits = 0)) %>% 
      e_grid(right = '15%') %>% 
      e_legend(orient = 'vertical', right = '5', top = '15%',
               selector = list(
                 list(type = 'inverse', title = 'Inverter'),
                 list(type = 'all', title = 'Restaurar')
               )) %>% 
      e_toolbox_feature(c("dataZoom", "dataView", "saveAsImage")) %>% 
      e_animation(duration = 1000) %>% 
      e_theme("bee-insipired")
    
  })
  
  ## output ranking instituicoes ----
  output$ranking_instituicao <- renderEcharts4r({
    base %>% 
      sample_n(1000) %>% 
      group_by(instituicao_destino, categoria) %>% 
      summarise(bolsas_concedidas = sum(bolsas_concedidas), .groups = "drop") %>% 
      sample_n(10) %>% 
      group_by(categoria) %>% 
      # tidyr::nest() %>% 
      # ungroup() %>% 
      # mutate(data = purrr::map(data, janitor::adorn_totals, "row")) %>% 
      # tidyr::unnest(data) %>% 
      # group_by(categoria) %>% 
      # mutate(ano_referencia = as.character(ano_referencia)) %>% 
      e_charts(instituicao_destino) %>%
      e_bar(bolsas_concedidas,  stack = 'total',
            emphasis = list(
              focus = 'series', blurScope = 'coordinateSystem'
            )) %>% 
      e_flip_coords() %>% 
      e_tooltip(trigger = "shadow", 
                formatter = e_tooltip_pointer_formatter(digits = 0))
    
  })
  
  ## output ranking cidades ----
  output$ranking_cidade <- renderEcharts4r({
    
    base %>% 
      sample_n(1000) %>% 
      group_by(cidade_destino, categoria) %>% 
      summarise(bolsas_concedidas = sum(bolsas_concedidas), .groups = "drop") %>% 
      with_groups(cidade_destino, mutate, total_bolsas = sum(bolsas_concedidas)) %>% 
      arrange(desc(total_bolsas)) %>% 
      with_groups(cidade_destino, tidyr::nest) %>% 
      slice_head(n =10) %>% 
      arrange(-row_number()) %>% 
      tidyr::unnest() %>% 
      group_by(categoria) %>% 
      e_charts(cidade_destino) %>%
      e_bar(bolsas_concedidas,  stack = 'total',
            emphasis = list(
              focus = 'series', blurScope = 'coordinateSystem'
            )) %>% 
      e_flip_coords() %>% 
      e_tooltip()
    
  })
  
  ## output tabela geral -----
  output$tabela_geral <- reactable::renderReactable({
    
    base_tabela %>% 
      sample_n(1000) %>% 
      reactable::reactable(filterable = TRUE, 
                           resizable = TRUE,
                           minRows = 10,
                           columns = list(
                             ano_referencia = colDef(name = "Ano"),
                             modalidade = colDef(name = "Modalidade"),
                             area = colDef(name = "Área"),
                             cidade_destino = colDef(name = "Cidade Destino"),
                             instituicao_destino = colDef(name = "Instituição Destino"),
                             bolsas_concedidas = colDef(name = "Bolsas"),
                             valor_pago = colDef(name = "Valor pago", format = colFormat(currency = "BRL"))
                           ))
  })
}

shinyApp(ui, server)