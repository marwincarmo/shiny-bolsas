sidebar <- dashboardSidebar(
  collapsed = FALSE,
  skin = "dark",
  status = "info",
  # https://fontawesome.com/icons?d=gallery&m=free
  sidebarMenu(
    id = "tabs",
    menuItem("Dados gerais", 
             tabName = "geral_tab",
             icon = icon("chart-bar"),
             startExpanded = TRUE),
    menuItem("Mapa das bolsas", tabName = "map_tab",
             icon = icon("globe-americas")),
    menuItem("Info",
             tabName = "info_tab",
             icon = icon("info-circle")),
    menuItem("Codigo fonte",
             href = "https://github.com/marwincarmo/shiny-bolsas",
             icon = icon("github"))
    
  )
)