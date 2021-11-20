base <- readr::read_rds("../data/cnpq_completo.rds")

map_tab <- tabItem(
  tabName = "map_tab",
  fluidPage(
    
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
                               e a quantidade de bolsas cedidas."),
                               p("Adaptado de https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example")

                 )
                 )
             )
    
  )
)