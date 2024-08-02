# ui/ui_taxonomy_map.R

ui_taxonomy_map <- shiny::fluidPage(
  shiny::br(),
  shiny::actionButton(
    inputId = "display_taxonomy_map",
    label = "Display Map"
  ),
  shiny::br(),
  shiny::br(),
  leaflet::leafletOutput(
    outputId = "leaflet_taxonomy_map",
    height = "1200px"
  )
)