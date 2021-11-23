# pacotes ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  #library(shinyjs)
  library(bs4Dash)
  #library(shinydashboard)
  library(leaflet)
  library(dplyr)
  library(reactable)
  library(echarts4r)
})

# database ----

base <- readr::read_rds("data/cnpq_completo.rds")
#base_tabela <- readr::read_rds("data/cnpq_tabela.rds")

# Tabs ----

source("ui/sidebar.R")


ui <- dashboardPage(
  #fullscreen = TRUE,
  skin = 'blue',
  dashboardHeader(
    title = dashboardBrand(
    title = 'Bolsas CNPq',
    href = 'https://github.com/marwincarmo/shiny-bolsas')
    ),
  sidebar = sidebar,
  dashboardBody(
    tabItems(
      # Aba info ----
      tabItem(
        tabName = "info_tab",
        fluidRow(
          column(width = 12,
                 includeMarkdown("R/info.Rmd"))
        )
      ),
      # Aba mapa ----
      tabItem(
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
            maximizable = TRUE,
            status = "info",
            width = 12,
            ### botao seletor grafico temporal ----
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
            shinycssloaders::withSpinner(echarts4rOutput("timeline"))
            
          )
          
        ),
        ## gráficos de ranking ----
        fluidRow(
          column(width = 12,
                 box(
                   width = NULL,
                   title = "Ranking por Cidade",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   maximizable = TRUE,
                   status = "info",
                   shinycssloaders::withSpinner(echarts4rOutput("ranking_cidade"))
                   )
                 )),
        fluidRow(
          column(width = 12,
                 box(
                   width = NULL,
                   title = "Ranking por Instituição",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   maximizable = TRUE,
                   status = "info",
                   shinycssloaders::withSpinner(echarts4rOutput("ranking_instituicao"))
                 ))
        ),
        ## tabela de dados gerais ----
        # fluidRow(
        #   box(title = "Tabela de dados",
        #       solidHeader = TRUE,
        #       collapsible = TRUE,
        #       maximizable = TRUE,
        #       status = "info",
        #       width = 12,
        #       reactable::reactableOutput("tabela_geral"))
        # )
        
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
          color = "danger",
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
          color = "danger",
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
             instituicao_destino %in% no_filter(input$inst_destino, base$instituicao_destino)
      ) 
    
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

    base_mapa <- base_filtrada() %>% 
      group_by(addr, latitude, longitude) %>% 
      summarise(bolsas_concedidas = sum(bolsas_concedidas))
    
    base_mapa %>% 
      leaflet() %>% 
      addTiles() %>% 
      addMarkers(
        popup = paste0(
        '<b>Cidade:</b> ', base_mapa$addr, '<br>',
        '<b>Bolsas:</b> ', format(base_mapa$bolsas_concedidas, big.mark = "."), '<br>'
        ),
        clusterOptions = markerClusterOptions()
      )
  })
  
  ## value box total ----
  output$num_total <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
      format(sum(pull(base_filtrada(), bolsas_concedidas)), big.mark = "."),
      "Bolsas no total",
      color = "purple"
    )
  })
  
  ## value box ic ----
  output$num_ic <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      format(sum(pull(dplyr::filter(base_filtrada(), 
                                    categoria == "Iniciação Científica"), bolsas_concedidas)), big.mark = "."), 
      "Bolsas de Iniciação Científica",
      color = "olive"
    )
  })
  ## value box mestrado ----
  output$num_mestrado <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      format(sum(pull(dplyr::filter(base_filtrada(), 
                                    categoria == "Mestrado"), bolsas_concedidas)), big.mark = "."),
      "Bolsas de Mestrado",
      color = "olive"
    )
  })
  ## value box doutorado ----
  output$num_doutorado <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      format(sum(pull(dplyr::filter(base_filtrada(), 
                                    categoria == "Doutorado"), bolsas_concedidas)), big.mark = "."),
      "Bolsas de Doutorado",
      color = "olive"
    )
  })
  ## value box posdoc ----
  output$num_posdoc <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      format(sum(pull(dplyr::filter(base_filtrada(), 
                                    categoria == "Pós-doutorado"), bolsas_concedidas)), big.mark = "."),
      "Bolsas de Pós-doc",
      color = "olive"
    )
  })
  ## value box outras ----
  output$num_outro <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      format(sum(pull(dplyr::filter(base_filtrada(), 
                                    categoria == "Outros"), bolsas_concedidas)), big.mark = "."),
      "Bolsas de outra categoria",
      color = "olive"
    )
  })
  
  ## output grafico de linha do tempo ----
  
  output$timeline <- renderEcharts4r({
    
    if (input$escolha_grafico == "Bolsas concedidas") {
      
      base_filtrada() %>% 
        group_by(ano_referencia, categoria) %>% 
        summarise(bolsas_concedidas = sum(bolsas_concedidas), .groups = "drop") %>% 
        group_by(ano_referencia) %>% 
        tidyr::nest() %>% 
        ungroup() %>% 
        mutate(data = purrr::map(data, janitor::adorn_totals, "row")) %>% 
        tidyr::unnest(data) %>% 
        group_by(categoria) %>% 
        #mutate(ano_referencia = as.character(ano_referencia)) %>% 
        e_charts(ano_referencia) %>%
        e_line(bolsas_concedidas) %>% 
        e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>% 
        e_x_axis(min = min(input$date_range),
                 max = max(input$date_range),
                 formatter = e_axis_formatter(locale = "pt-BR")) %>% 
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
      
    } else {
     
     base_filtrada() %>% 
       group_by(ano_referencia, categoria) %>% 
       summarise(valor_pago = sum(valor_pago), .groups = "drop") %>% 
       group_by(ano_referencia) %>% 
       tidyr::nest() %>% 
       ungroup() %>% 
       mutate(data = purrr::map(data, janitor::adorn_totals, "row")) %>% 
       tidyr::unnest(data) %>% 
       group_by(categoria) %>% 
       #mutate(ano_referencia = as.character(ano_referencia)) %>% 
       e_charts(ano_referencia) %>%
       e_line(valor_pago) %>% 
       e_y_axis(formatter = e_axis_formatter("currency", currency = "BRL", locale = "pt-BR")) %>% 
       e_x_axis(min = min(input$date_range),
                max = max(input$date_range),
                formatter = e_axis_formatter(locale = "pt-BR")) %>% 
       e_axis_labels(x = "Ano", y = "Valor pago") %>% 
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
    }
    

  })
  
  ## output ranking instituicoes ----
  output$ranking_instituicao <- renderEcharts4r({
    
    base_filtrada() %>% 
      group_by(instituicao_destino, categoria) %>% 
      summarise(bolsas_concedidas = sum(bolsas_concedidas), .groups = "drop") %>% 
      with_groups(instituicao_destino, mutate, total_bolsas = sum(bolsas_concedidas)) %>% 
      arrange(desc(total_bolsas)) %>% 
      with_groups(instituicao_destino, tidyr::nest) %>% 
      slice_head(n =10) %>% 
      arrange(-row_number()) %>% 
      tidyr::unnest() %>% 
      group_by(categoria) %>% 
      e_charts(instituicao_destino) %>%
      e_bar(bolsas_concedidas,  stack = 'total',
            emphasis = list(
              focus = 'series', blurScope = 'coordinateSystem'
            )) %>% 
      e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>% 
      e_axis_labels(y = "Número de bolsas") %>% 
      e_flip_coords() %>% 
      e_tooltip()
    
  })
  
  ## output ranking cidades ----
  output$ranking_cidade <- renderEcharts4r({
    
    base_filtrada() %>% 
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
      e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>% 
      e_axis_labels(y = "Número de bolsas") %>% 
      e_flip_coords() %>% 
      e_tooltip()
    
  })
  
  ## output tabela geral -----
  # output$tabela_geral <- reactable::renderReactable({
  #  
  #  base_tabela %>% 
  #    reactable::reactable(filterable = TRUE, 
  #                         resizable = TRUE,
  #                         minRows = 10,
  #                         columns = list(
  #                           ano_referencia = colDef(name = "Ano"),
  #                           modalidade = colDef(name = "Modalidade"),
  #                           area = colDef(name = "Área"),
  #                           cidade_destino = colDef(name = "Cidade Destino"),
  #                           instituicao_destino = colDef(name = "Instituição Destino"),
  #                           bolsas_concedidas = colDef(name = "Bolsas"),
  #                           valor_pago = colDef(name = "Valor pago", format = colFormat(currency = "BRL",
  #                                                                                       separators = TRUE))
  #                         ))
  # })
}

shinyApp(ui, server)