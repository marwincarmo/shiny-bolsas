sidebar <- dashboardSidebar(
  collapsed = FALSE,
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
    menuItem("Codigo no github",
             tabName = "code",
             icon = icon("github"))
    
  )
)