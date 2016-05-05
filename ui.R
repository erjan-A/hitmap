library(shiny)
library(leaflet)

shinyUI(navbarPage(windowTitle="Hitmap" ,tags$img(src="img/logo.png", class=""), theme = "css/full.css",
  
  mainPanel(
    fluidRow(
      column(4,   wellPanel(div(class="navbar-collapse", 
                      div(class="search_form", 
                          div(class="", id="nl-form",
                              tags$span("I am travelling to"), textInput("city", label=NA, value = "San Francisco", width = "115px"), tags$br(), 
                              tags$span(" with"), selectInput("accompany", selectize = FALSE, label=NA, choices = list("family" = 1, "friends" = 2)),
                              tags$span(" of"),  selectInput("accompanynumber", selectize = FALSE, label=NA, choices = list(1, 2, 3, 4, 5)), tags$span(" person(s)"), 
                              tags$span(" for"), selectInput("purpose", label=NA, selectize = FALSE, choices = list("sightseeing" = 1, "shopping" = 2)), tags$br(), 
                              tags$span(" on"), dateRangeInput('dateRange',label = NA, separator = NULL, start = Sys.Date(), end = Sys.Date() + 2),tags$br(),
                              tags$br(), div(class="rn", tags$span("Number of hotels to display: "), selectInput("results_number",label=NA, c(5, 10, 20, "all"="all"), selected = "all")),
                              div(class="buttons", actionButton("search", "Search"), actionButton("clear", "Clear"))
                          )
                      )
                  )),
             #wellPanel(verbatimTextOutput("results")),
             uiOutput("results_count"),
             div(class = "busy", img(src="img/busy.gif")),
             uiOutput("hotels_table")
             #tableOutput("results")
      ),
      column(8,   div(class = "busy", img(src="img/busy.gif")), leafletOutput("map", height = "100%"))
    ),
    div(class="footer", tags$p(class="text-muted", "Hitmap uses data from ", tags$a(href="https://data.sfgov.org", "SF Open Data"), " /", tags$a(href="https://sandbox.amadeus.com", "Amadeus"),  " /", tags$a(href="http://developer.citygridmedia.com", "CityGrid"))),
    tags$head(tags$script(src="js/busy.js")),
    tags$head(tags$script(src="js/nlform.js")),
    HTML("<script>var nlform = new NLForm( document.getElementById( 'nl-form' ) );</script>"),
    HTML("<script type='text/javascript'>(function() {var s = document.createElement('script');s.type = 'text/javascript';s.async = true;s.src = '//api.usersnap.com/load/'+'07f0c9fc-e99a-43c8-9d4e-0d225868d253.js';var x = document.getElementsByTagName('script')[0];x.parentNode.insertBefore(s, x);})();</script>")
)))