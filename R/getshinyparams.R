#' getshinyparams
#' 
#' @export
#' @returns Parameters to start the shiny app for determining pour points manually for a given reservoir
#' @import shiny
#' @import leaflet
#' @import shinydashboard
#' @param res An sf polygon, with an unstandardised raw reservoir

getshinyparams <- function(res){
  ui1 <- fluidPage(
    titlePanel("Pour out point (dam location)"),
    fluidRow(
      column(
        width = 8,
        leafletOutput("map")
      ),
      column(
        width = 4,
        valueBoxOutput("simple_box"))
    )
  )
  ui2 <- fluidPage(
    titlePanel("Pour in point (start of reservoir)"),
    fluidRow(
      column(
        width = 8,
        leafletOutput("map")
      ),
      column(
        width = 4,
        valueBoxOutput("simple_box"))
    )
  )
  server <- function(input, output) {
    output$map <- renderLeaflet({
      leaflet(res) %>% 
        addProviderTiles(provider = "OpenTopoMap") %>%
        addPolygons(group = "Reservoir") %>%
        addLayersControl(
          overlayGroups = c("OpenTopoMap", "Reservoir"),
          options = layersControlOptions(collapsed = FALSE)
        ) 
    })
    coordinates <- reactiveValues()
    observeEvent(input$map_click, {
      click <- input$map_click
      if (!is.null(click)) {
        coordinates$latitude <- click$lat
        coordinates$longitude <- click$lng
      }
      pour <- unlist(isolate(reactiveValuesToList(coordinates)))
      pour <<- unlist(isolate(reactiveValuesToList(coordinates)))
    })
    output$simple_box <- renderValueBox({
      valueBox(
        value = paste("Lat:", coordinates$latitude, "; Lon: ", coordinates$longitude, "."),
        subtitle = "Close map once selected.")
    })
  }
  return(list(ui1, ui2, server))
}
