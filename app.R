library(shiny)
library(leaflet)
library(request)
library(httr)
library(jsonlite)
library(sortable)
library(glue)
library(bs4Dash)


# Define UI for application that draws a histogram
ui <- fluidPage(
        column(
          leafletOutput("mymap", width = "100%", height = 1080), width = 10
          ),
        column(

          h2("Main Assignment"),

          fluidRow(
            selectInput(inputId = 'city_1', label = 'City 1',
                        choices = c('Boston', 'Copenhagen', 'Dallas', 'Houston', 'Munich', 'Oslo', 'Paris', 'Shanghai', 'Trondheim')),
            selectInput(inputId = 'city_2', label = 'City 2',
                        choices = c('Boston', 'Copenhagen', 'Dallas', 'Houston', 'Munich', 'Oslo', 'Paris', 'Shanghai', 'Trondheim'))
          ),

          actionButton(inputId = 'confirm_compare', label = 'Check Distance'),

          textOutput(outputId = 'distance', container = h3),

          h2('TSP GAME'),

          p('Add as many cities as you want. Then click "Start TSP", Reorder the cities in the correct TSP order and Submit'),

          textInput(inputId = 'city_input', label = 'City Input', placeholder = 'Enter City Name'),

          fluidRow(
            actionButton(inputId = 'confirm_city', label = 'Add', width = '49%'),
            actionButton(inputId = 'remove_city', label = 'Remove', width = '49%')
          ),

          fluidRow(
            actionButton(inputId = 'start_tsp', label = 'Start TSP', width = '49%'),
            actionButton(inputId = 'confirm_tsp', label = 'Submit', width = '49%')
          ),
          uiOutput(outputId = 'tsp_container'),
          width = 2)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(0, 0, zoom = 2)
  })

  observeEvent(input$confirm_compare, {
    req(c(input$city_1, input$city_2))

    # URL of the endpoint
    url <- "http://127.0.0.1:8000/get_distance"

    # Data to be sent in JSON format (a named list in R)
    data <- list("city_1" = input$city_1,
                 "city_2" = input$city_2)


    # Convert the data to JSON format
    json_data <- jsonlite::toJSON(data, auto_unbox = TRUE)

    # Send a GET request with JSON data
    response <- POST(url, body = json_data, content_type("application/json"))
    content <- content(response, "text")
    response_data <- jsonlite::fromJSON(content)

    if (response_data$type == 'success') {

      response_data <- jsonlite::fromJSON(content)

      leafletProxy('mymap') %>%
        addCircleMarkers(lng = response_data$city_coordinates[3],
                   lat = response_data$city_coordinates[1],
                   label = input$city_1,
                   layerId = 'city_1',
                   color = 'red') %>%
        addCircleMarkers(lng = response_data$city_coordinates[4],
                   lat = response_data$city_coordinates[2],
                   label = input$city_2,
                   layerId = 'city_2',
                   color = 'red')



        output$distance <- renderText(paste(round(response_data$distance, 1), 'km between cities'))

    } else {
      showNotification(paste("Request failed with status code:", response$status_code), type = 'error')
    }

  })

  observeEvent(input$confirm_city, {

    req(input$city_input)
    # URL of the endpoint
    url <- "http://127.0.0.1:8000/add_city"

    # Data to be sent in JSON format (a named list in R)
    data <- list("city" = input$city_input)


    # Convert the data to JSON format
    json_data <- jsonlite::toJSON(data, auto_unbox = TRUE)

    # Send a POST request with JSON data
    response <- POST(url, body = json_data, content_type("application/json"))
    content <- content(response, "text")
    response_data <- jsonlite::fromJSON(content)

    # Check the response

    if (response_data$type == 'success') {

      response_data <- jsonlite::fromJSON(content)

      leafletProxy('mymap') %>%
        addMarkers(lng = response_data$city_coordinates[2],
                   lat = response_data$city_coordinates[1],
                   label = input$city_input,
                   layerId = input$city_input)

      showNotification("City added to Map")

    } else {
      showNotification(paste("Request failed with status code:", response$status_code), type = 'error')
    }
  })

  observeEvent(input$remove_city, {
    req(input$city_input)
    # URL of the endpoint
    url <- "http://127.0.0.1:8000/remove_city"

    # Data to be sent in JSON format (a named list in R)
    data <- list("city" = input$city_input)


    # Convert the data to JSON format
    json_data <- jsonlite::toJSON(data, auto_unbox = TRUE)

    # Send a POST request with JSON data
    response <- POST(url, body = json_data, content_type("application/json"))

    # Check the response
    if (response$status_code == 200) {
      content <- content(response, "text")
      response_data <- jsonlite::fromJSON(content)

      leafletProxy('mymap') %>%
        removeMarker(layerId = input$city_input)

      showNotification("City removed from Map")

    } else {
      showNotification(paste("Request failed with status code:", response$status_code), type = 'error')
    }
  })

  observeEvent(input$start_tsp,{

    # URL of the endpoint
    url <- "http://127.0.0.1:8000/get_city_hist"

    # Send a GET request with JSON data
    response <- GET(url, content_type("application/json"))

    # Check the response
    if (response$status_code == 200) {
      content <- content(response, "text")
      response_data <- jsonlite::fromJSON(content)

      output$tsp_container <- renderUI(
        rank_list_basic <- rank_list(
          text = glue("Sort the cities in correct TSP order starting from {response_data$city_history[1]}"),
          labels = response_data$city_history,
          input_id = "rank_list_city"
        )
      )

      showNotification("City History Added to TSP")

    } else {
      showNotification(paste("Request failed with status code:", response$status_code), type = 'error')
    }
  })

  observeEvent(input$confirm_tsp, {
    req(input$rank_list_city)

    # URL of the endpoint
    url <- "http://127.0.0.1:8000/get_tsp"

    # Send a GET request with JSON data
    response <- GET(url, content_type("application/json"))

    # Check the response
    if (response$status_code == 200) {
      content <- content(response, "text")
      response_data <- jsonlite::fromJSON(content)

      if(all(diff(match(input$rank_list_city, response_data$Route_name)) > 0)){
        showNotification("Correct")
      } else {

        showNotification("Incorrect")
        showNotification(paste(response_data$Route_name, collapse = ' '))
      }

    } else {
      showNotification(paste("Request failed with status code:", response$status_code), type = 'error')
    }


  })

}

# Run the application
shinyApp(ui = ui, server = server)