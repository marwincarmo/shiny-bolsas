sidebar <- dashboardSidebar(
  collapsed = FALSE,
  # https://fontawesome.com/icons?d=gallery&m=free
  sidebarMenu(
    id = "tabs",
    # menuItem("Main", tabName = "main_tab",
    #          icon = icon("home")),
    menuItem("Mapa", tabName = "map_tab",
             icon = icon("home"),
             startExpanded = TRUE),
    menuItem("Por area", tabName = "tab_area")
  )
)