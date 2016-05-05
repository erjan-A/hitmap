require(ggplot2)
require(rgdal)
require(scales)
require(dplyr)
require(maptools)
require(sp)
require(maps)
require(raster)
require(leaflet)
require(shiny)
require(fields)
source("API.R")

shinyServer(function(input, output, session) {

  #Load San Francisco Census Blocks data
  blocks <- shapefile("data/tl_2010_06075_tabblock10edited/tl_2010_06075_tabblock10.shp")
  # blocks <- shapefile("data/cb_2014_06_tract_500k/cb_2014_06_tract_500k.shp")
  blocks <- spTransform(x=blocks, CRSobj=CRS("+proj=longlat +datum=WGS84"))
  names(blocks@data) <- tolower(names(blocks@data))
  
  
  #Load San Francisco 2015 crime data
  crime_data <- read.csv("data/SFPD_Incidents_-_Previous_Year__2015_.csv", stringsAsFactors = FALSE, header = TRUE)
  crime_data$Category <- tolower(crime_data$Category)
  
  #Load Categroy Scoring data
  category_score <- read.csv("data/category_score.csv", stringsAsFactors = FALSE, header = TRUE)
  category_score$Category <- tolower(category_score$Category)
  
  #Load SF attractions data
  attractions <- read.csv("data/sf-attractions.csv", stringsAsFactors = FALSE, header = TRUE)
  
  
  ###########################################################################
  
  
  # Cleaning and Keeping only complete cases
  # Loading SF 2015 and 2016 crime data -> remove unnecessary columns -> merge -> load category table -> assign (match) weights to each crime based on category
  crime_data <- crime_data[complete.cases(crime_data), ]
  
  # Assign category score to each crime
  cat <- match(x=crime_data$Category, table=category_score$Category)
  crime_data$Score <- category_score$Score[cat]
  
  #Remove unncessary columns and turn crime_data in to data frame
  df.crime_data <- data.frame(category=crime_data$Category, pdid=crime_data$PdId, score=crime_data$Score, latitude=crime_data$Y, longitude=crime_data$X)
  
  
  ###########################################################################
  
  
  #Convert crime data to a spatial points object
  df.crime_data <- SpatialPointsDataFrame(coords=df.crime_data[, c("longitude", "latitude")], data=df.crime_data[, c("category", "pdid", "score")], proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  #Spatial overlay to identify census polygon in which each crime point falls
  #The Result `crime_blocks` is a dataframe with the block data for each point
  crime_blocks <- over(x=df.crime_data, y=blocks)
  
  #Add block data to each crime 
  df.crime_data@data <- data.frame(df.crime_data@data, crime_blocks)
  
  #Aggregate crimes by block 
  agg_crimeblocks <- aggregate(cbind(df.crime_data@data$score)~geoid10, data = df.crime_data, FUN = sum)
  
  #Format score into ranking
  agg_crimeblocks$V1 <- dense_rank(agg_crimeblocks$V1)
  
  #Add number of crimes to blocks object
  m <- match(x=blocks@data$geoid10, table=agg_crimeblocks$geoid10)
  blocks@data$score <- agg_crimeblocks$V1[m]
  
  maxScore <- max(agg_crimeblocks$V1)
  
  ###########################################################################
  
  ## Plotting an interactive map with leaflet ##
  
  # Copying blocks df
  df.blocks <- blocks
  
  # Cleaning blocks data: replacing NA (crime rate) with 0 
  df.blocks@data$score[is.na(df.blocks@data$score)] <- 0
  
  
  #Load SF attractions data
  attractions <- read.csv("data/sf-attractions.csv", stringsAsFactors = FALSE, header = TRUE)
  
  # Format CRS of data frame
  att <- subset(attractions, select = c(longitude, latitude))
  coordinates(att) <- c("longitude", "latitude")
  proj4string(att) <- CRS("+proj=longlat + ellps=WGS84")
  att <- spTransform(att, CRS(proj4string(df.blocks)))
  
  # Get rid of bad attractions outside SF
  att <- att[df.blocks,]
  att <- as.data.frame(att)
  
  set.seed(12345)
  
  # Using kmeans to get the centroids of attraction locations
  att.8means <- kmeans(att, 8, iter.max = 10)
  
  att_cluster_centers <- att.8means$centers
  att_cluster_sizes <- att.8means$size
  
  # Get a table of centroids and their corresponding sizes
  att_cluster_centers <- cbind(att_cluster_centers, att_cluster_sizes) 
  
  ###########################################################################
  
  ## Plotting an interactive map with leaflet ##
  
  
  # Cleaning blocks data: replacing NA (crime rate) with 0 
  df.blocks@data$score[is.na(df.blocks@data$score)] <- 0
  
  # Legend
  ATgreen <- rgb(46/255, 204/255, 113/255, 1)
  ATyellow <- rgb(241/255, 196/255, 14/255, 1)
  ATred <- rgb(231/255, 76/255, 60/255, 1)
  
  polpopup <- paste0("Hitmap score: ", paste(round(100-(df.blocks$score/maxScore)*100,digits=2),"%",sep=""))
  attractionspopup <- paste0("Attraction: ", attractions$name, "<br>", "Address: ", attractions$address.street, "<br>", "<a href=", attractions$profile, ">", "Link", "</a>" )
  pal <- colorNumeric(
    palette = c(ATgreen, ATyellow, ATred),
    domain = df.blocks$score)


  # Get input values
  inputs <- reactive({
    as.character(inputs$with)
  })
  
  # Draw map
  output$map <- renderLeaflet({
    leaflet(data = c(attractions)) %>% setView(lng = -122.447902, lat = 37.761886, zoom = 12) %>%
      addTiles() %>%
      addPolygons(data = df.blocks, 
                  fillColor = ~pal(score), 
                  color = NA,
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.5,
                  popup = polpopup) %>%
      addLegend(
        colors = c('#e74c3c', '#fdae61', '#fee08b', '#ffffbf', '#d9ef8b', '#59d285'), 
        labels = c("<span style='font-size:11px'>Less safe</span>","","","","","<span style='font-size:11px'>Safer</span>"),
        values = df.blocks$score, 
        position = "bottomright", 
        title = "Crime Risk",
        opacity = 0.6) %>%
#      addMarkers(hotels_sample$lon, hotels_sample$lat, popup = markerpopup) %>%
      addCircleMarkers(attractions$longitude, attractions$latitude, 
                       popup = attractionspopup,
                       radius = 5,
                       color = "#9b59b6",
                       stroke = FALSE, fillOpacity = 0.5,
                       group = "Attractions") %>%
      addLayersControl(
        overlayGroups = c("Attractions"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  #Search button event listener
  observeEvent(input$search, {
    
    #Clear existing data
    leafletProxy("map", session) %>% clearGroup(group="hotels")
    output$results <- renderPrint({
      invisible()
    })
    
    output$results_count <- renderPrint({
      invisible()
    })
    
    output$hotels_table <- renderPrint({
      invisible()
    })
    
    #Getting hotels using API
    hotel_r <- HitMap_hotels(input$dateRange[1],input$dateRange[2])
    hotel_r <- subset(hotel_r, select = -c(results.contacts, results.amenities, results.images, results.rooms, results.awards, results.marketing_text))
    
    #Spatial overlay to identify census polygon in which each hotel falls
    df.hotel_r <- SpatialPointsDataFrame(coords=hotel_r[, c("results.location.longitude", "results.location.latitude")], data=hotel_r[, c("results.property_name", "results.address.line1", "results.address.postal_code", "results.total_price.amount","results.min_daily_rate.amount", "results._links.more_rooms_at_this_hotel.href")], proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    #The Result `crime_blocks` is a dataframe with the block data for each point
    hotel_blocks <- over(x=df.hotel_r, y=df.blocks)
    
    #Add block data to each hotel
    df.hotel_r@data <- data.frame(df.hotel_r@data, score=round(100-(hotel_blocks$score/maxScore)*100,digits=2))
    
    hotel_r_by_scores <- df.hotel_r@data
    hotel_r_by_scores <- cbind(hotel_r_by_scores, df.hotel_r@coords[,"results.location.longitude"])
    hotel_r_by_scores <- cbind(hotel_r_by_scores, df.hotel_r@coords[,"results.location.latitude"])
    
    hotel_r_by_scores <- hotel_r_by_scores[,c(8,9, 1:7)]
    
    # Calculate the distances from hotels to each of the attraction cluster centroids
    hotel_att_distance <- 1/rdist.earth(hotel_r_by_scores, as.data.frame(att_cluster_centers), miles = FALSE)
    
    # Calculate distance * size for each hotel/centroid
    hotel_att_distance_sizes <- hotel_att_distance*att_cluster_sizes
    ranks <- dense_rank(hotel_att_distance_sizes)
    hotel_att_distance_sizes_rank <- t(as.data.frame(split(ranks, 1:as.integer(count(hotel_r_by_scores)))))
    hotel_att_distance_rank <- as.data.frame(rowSums(hotel_att_distance_sizes_rank))
    hotel_att_distance_rank <- 100*(hotel_att_distance_rank/max(hotel_att_distance_rank))
    
    hotel_r_by_scores <- cbind(hotel_r_by_scores, hotel_att_distance_rank)
    colnames(hotel_r_by_scores)[10] = "att_score"
    hotel_r_by_scores <- cbind(hotel_r_by_scores, (.6*hotel_r_by_scores$score + .4*hotel_r_by_scores$att_score))
    colnames(hotel_r_by_scores)[11] = "hitmap_score"
    hotel_r_by_scores <- hotel_r_by_scores[order(-hotel_r_by_scores$hitmap_score),]
    hotel_r_by_scores$hitmap_score_5 <- round(hotel_r_by_scores$hitmap_score*5/100, digits = 2)
    total_hotels_number <- as.integer(count(hotel_r_by_scores))
    
    if (input$results_number != "all") {
      hotel_r_by_scores <- hotel_r_by_scores[1:as.character(input$results_number),]
    }

    
    #Plotting hotel on the map
    markerpopup <- paste0("Hotel: ", hotel_r_by_scores$results.property_name, "<br>", "Total Price: $", hotel_r_by_scores$results.total_price.amount, "<br>", "Hitmap Score: ", hotel_r_by_scores$hitmap_score_5,"/ 5")
    leafletProxy("map", session) %>% addMarkers(hotel_r_by_scores[,1], hotel_r_by_scores[,2], popup = markerpopup, group="hotels")
    
    #Display number of hotels
    output$results_count <- renderUI({
      result_count <- as.character(as.integer(count(hotel_r_by_scores)))
      div(class="result_count_p", tags$strong(total_hotels_number), tags$span(" hotels found"), tags$span(class="right-span", "Sorted by Hitmap score"))
    })
    
    
    #Display a table of hotels
    output$hotels_table <- renderUI({
      lapply(1:as.integer(count(hotel_r_by_scores)), function(i) {
        div(class="thumbnail card-1",
            div(class="caption",
                div(class="left",
                  tags$h4(hotel_r_by_scores$results.property_name[i]),
                  tags$p(class="address", hotel_r_by_scores$results.address.line1[i], hotel_r_by_scores$results.address.postal_code[i]),
                  tags$p(class="reviews", "Hitmap score: ",  hotel_r_by_scores$hitmap_score_5[i] , "/ 5")
                ),
                div(class="right",
                    tags$p(class="price", "$", hotel_r_by_scores$results.total_price.amount[i]),
                    tags$p(class="link", tags$a(href=hotel_r_by_scores$results._links.more_rooms_at_this_hotel.href[i], "Book"))
                    )
            )
        )
      })
    })
    
    #output$results <- renderTable(hotel_r_by_scores)
    
  })
  
  #Clear button event listner: clear hotels on map and table
  observeEvent(input$clear, {
    leafletProxy("map", session) %>% clearGroup(group="hotels")
    output$results <- renderPrint({
      invisible()
    })
    output$results_count <- renderPrint({
      invisible()
    })
    
    output$hotels_table <- renderPrint({
      invisible()
    })
  })
  
})