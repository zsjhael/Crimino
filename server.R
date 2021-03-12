function(input, output, session) {
  theme_set(theme_classic() + theme(text=element_text(family="Raleway")))
  
  # ---- Reactive Inputs ----
  output$race.white.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Race.White_pct', label = 'Percentage "White"', min = 0, max = 1, value = dt.ward[dt.ward$Ward == input$ward.input, "Race.White_pct"])
    )
  })
  output$race.black.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Race.Black_pct', label = 'Percentage "Black"', min = 0, max = 1, value = dt.ward[dt.ward$Ward == input$ward.input, "Race.Black_pct"])
    )
  })
  output$race.asian.input <- renderUI({
    tagList(
      shinyjs::disabled(sliderInput(inputId = 'Race.Asian_pct', label = 'Percentage "Asian"', min = 0, max = 1, value = (1 - input$Race.White_pct - input$Race.Black_pct)))
    )
  })
  output$ethnicity.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Ethnicity.Hispanic_pct', label = 'Percentage "Hispanic Ethnicity"', min = 0, max = 1, value = dt.ward[dt.ward$Ward == input$ward.input, "Ethnicity.Hispanic_pct"])
    )
  })
  output$police.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Police_Stations', label = 'Number of Police Stations in a Ward', min = min(dt.ward$Police_Stations), max = max(dt.ward$Police_Stations), value = dt.ward[dt.ward$Ward == input$ward.input, "Police_Stations"])
    )
  })
  output$income.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Income', label = 'Average Income Level', min = min(dt.ward$Income), max = max(dt.ward$Income), value = dt.ward[dt.ward$Ward == input$ward.input, "Income"])
    )
  })
  output$service.calls.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Abandoned_Vehicles', label = 'Number of Abandonded Vehicle Requests', min = min(dt.ward$Abandoned_Vehicles), max = max(dt.ward$Abandoned_Vehicles), value = dt.ward[dt.ward$Ward == input$ward.input, "Abandoned_Vehicles"]),
      sliderInput(inputId = 'Garbage_Carts', label = 'Number of Garbage Cart Requests', min = min(dt.ward$Garbage_Carts), max = max(dt.ward$Garbage_Carts), value = dt.ward[dt.ward$Ward == input$ward.input, "Garbage_Carts"]),
      sliderInput(inputId = 'Rodent_Baiting', label = 'Number of Rodent Baiting Requests', min = min(dt.ward$Rodent_Baiting), max = max(dt.ward$Rodent_Baiting), value = dt.ward[dt.ward$Ward == input$ward.input, "Rodent_Baiting"]),
      sliderInput(inputId = 'Sanitation_Code_Complaints', label = 'Number of Sanitation Code Complaints', min = min(dt.ward$Sanitation_Code_Complaints), max = max(dt.ward$Sanitation_Code_Complaints), value = dt.ward[dt.ward$Ward == input$ward.input, "Sanitation_Code_Complaints"]),
      sliderInput(inputId = 'Graffiti_Removal', label = 'Number of Gaffiti Removal Requests', min = min(dt.ward$Graffiti_Removal), max = max(dt.ward$Graffiti_Removal), value = dt.ward[dt.ward$Ward == input$ward.input, "Graffiti_Removal"]),
      sliderInput(inputId = 'Abandoned_Buildings', label = 'Number of Abandonded Building Requests', min = min(dt.ward$Abandoned_Buildings), max = max(dt.ward$Abandoned_Buildings), value = dt.ward[dt.ward$Ward == input$ward.input, "Abandoned_Buildings"])
    )
  })
  
  
  # ---- Data Explorer ----
  # -- About the Crimino Dataset Tab--
  output$variables <- DT:: renderDT({
    dt.variables
  })

  output$sample <- DT:: renderDT({
    dt.crimes %>% 
      dplyr::sample_n(100)
  })
  
  
  # -- Summary Statistics Tab
  output$tb.descriptives <- renderTable({
    dt.descriptives <- data.frame(Category = c("Total nº of observations", "Unique crime types", "Unique crime groups - Primary.Type", "Total nº unique locations", "Arrests"), Statistics = c(nrow(dt.crimes), length(unique(dt.crimes$Description)), length(unique(dt.crimes$Primary.Type)), length(unique(dt.crimes$Location)), nrow(dt.crimes[Arrest == "Yes", ])))
  })
  
  output$hist.ward <- renderPlot({
    ggplot(dt.crimes, aes(x = Ward)) + geom_histogram() + ggtitle("Frequency of crimes per ward")
  })
  
  output$hist.Primary.Type <- renderPlot({
    ggplot(dt.crimes, aes(x = Primary.Type)) + geom_bar() + ggtitle("Occurences per crime type - Primary.Type") + coord_flip()
  })
  
  output$hist.Description <- renderPlot({
    ggplot(dt.crimes, aes(x = Description)) + geom_bar() + ggtitle("Occurences per crime type - Description") + coord_flip()
  })
  
  output$tb.locations <- renderTable({
    dt.locations <- data.frame(Location = c("Wards", "Districts", "Beats", "Community Areas"), Total = c(length(unique(dt.crimes$Ward)), length(unique(dt.crimes$District)), length(unique(dt.crimes$Beat)), length(unique(dt.crimes$Community.Area))))
  })
  
  # -- Crime Tab
  tabledata.ward <- function(){
    dt.crimes[Primary.Type %in% input$crime.type][Date.Time >= input$daterange.crime[1] & Date.Time <= input$daterange.crime[2]][, Sum_Crimes_Per_Ward := .N[1L], by = list(Primary.Type, Ward)][, Total_Crimes_Type := .N[1L], by = Primary.Type][, list(Ward, Sum_Crimes_Per_Ward, Total_Crimes_Type)][!duplicated(Ward), ]
  }
  histdata.ward <- function(){
    dt.crimes.hist.wards <- dt.crimes[Primary.Type %in% input$crime.type][Date.Time >= input$daterange.crime[1] & Date.Time <= input$daterange.crime[2]]
    dt.crimes.hist.wards %>% ggplot(aes(x = Ward)) + geom_bar() + coord_flip()
  }
  output$summarytable.crime <- DT::renderDT({
    tabledata.ward()
  })
  output$hist.crime <- renderPlot({
    histdata.ward()
  })
  
  # -- Ward Tab
  
  tabledata <- function(){
    dt.crimes[Ward %in% input$ward][Date.Time >= input$daterange[1] & Date.Time <= input$daterange[2]][, Sum_Crime_Type_Ward := .N[1L], by = list(Primary.Type)][, Total_Crimes_Ward := .N[1L], by = Ward][, list(Primary.Type, Sum_Crime_Type_Ward, Total_Crimes_Ward)][!duplicated(Primary.Type), ]
  }
  histdata <- function(){
    dt.crimes.hist <- dt.crimes[Ward %in% input$ward][Date.Time >= input$daterange[1] & Date.Time <= input$daterange[2]]
    dt.crimes.hist %>% ggplot(aes(x = Primary.Type)) + geom_bar() + coord_flip()
  }
  output$summarytable <- DT::renderDT({
    tabledata()
  })
  output$hist <- renderPlot({
    histdata()
  })
  
  # ---- Interactive Map ----
  
  # - Use the sampled crimes to build Choropleth spatial data frame
  # Count number of crimes per ward
  dt.crimes.map <- as.data.table(dt.crimes.map)
  dt.crimes.map.choropleth <- dt.crimes.map[, n_crimes := .N, by = Ward]
  # Delete duplicate wards and keep only ward and n_crimes variables
  dt.crimes.map.choropleth <- dt.crimes.map.choropleth[!duplicated(dt.crimes.map.choropleth$Ward), 
                                                       list(Ward, n_crimes)]
  # Merge the spatial data frame with crime count by ward data
  sp.ward.boundaries.map <- sp::merge(sp.ward.boundaries, dt.crimes.map.choropleth, 
                                      by.x = "ward", by.y = "Ward", all.x = TRUE)
  
  # - Calculate color bins for legend
  # Use quantiles function to find dynamic bin cutoffs
  bins.quantiles <- quantile(sp.ward.boundaries.map$n_crimes, names = FALSE, 
                             probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
  # Store bins
  bins <- c(0, bins.quantiles, Inf)
  # Assign different color to each bin
  pal2 <- colorBin("YlOrRd", domain = sp.ward.boundaries.map$n_crimes, bins = bins)
  
  output$text1 <- renderText({paste("Hello Wesley")})
  
  dt.crimes.map.filter <- reactive({
    dt.crimes.map[Type.Highlight %in% input$Primary.Type &
                    #Location.Description %in% input$Location.Description &
                    Arrest %in% input$Arrest &
                    Domestic %in% input$Domestic &
                    Ward >= input$Ward[1] & Ward <= input$Ward[2] &
                    date(Date.Time) >= input$Date[1] & date(Date.Time) <= input$Date[2], ]
  })
  
  sp.ward.boundaries.map.filter <- reactive({
    dt.crimes.map.filter2 <- dt.crimes.map[Type.Highlight %in% input$Primary.Type &
                                            #Location.Description %in% input$Location.Description &
                                             Arrest %in% input$Arrest &
                                             Domestic %in% input$Domestic &
                                             Ward >= input$Ward[1] & Ward <= input$Ward[2] &
                                             date(Date.Time) >= input$Date[1] & date(Date.Time) <= input$Date[2], ]
    dt.crimes.map.choropleth.filter <- dt.crimes.map.filter2[, n_crimes := .N, by = list(Ward)]
    dt.crimes.map.choropleth.filter <- dt.crimes.map.choropleth.filter[!duplicated(dt.crimes.map.choropleth.filter$Ward), 
                                                                       list(Ward, n_crimes)]
    outcome2 <- sp::merge(sp.ward.boundaries, dt.crimes.map.choropleth.filter, 
                          by.x = "ward", by.y = "Ward", all.x = TRUE)
    outcome2
  })
  
  pal2.filter <- reactive({
    dt.crimes.map.filter3 <- dt.crimes.map[Type.Highlight %in% input$Primary.Type &
                                             #Location.Description %in% input$Location.Description &
                                             Arrest %in% input$Arrest &
                                             Domestic %in% input$Domestic &
                                             Ward >= input$Ward[1] & Ward <= input$Ward[2] &
                                             date(Date.Time) >= input$Date[1] & date(Date.Time) <= input$Date[2], ]
    dt.crimes.map.choropleth.filter3 <- dt.crimes.map.filter3[, n_crimes := .N, by = list(Ward)]
    dt.crimes.map.choropleth.filter3 <- dt.crimes.map.choropleth.filter3[!duplicated(dt.crimes.map.choropleth.filter3$Ward), 
                                                                         list(Ward, n_crimes)]
    sp.ward.boundaries.map.filter3 <- sp::merge(sp.ward.boundaries, dt.crimes.map.choropleth.filter3, 
                                                by.x = "ward", by.y = "Ward", all.x = TRUE)
    bins.quantiles3 <- unique(quantile(sp.ward.boundaries.map.filter3$n_crimes, names = FALSE, 
                                       probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm = TRUE))
    bins3 <- c(0, bins.quantiles3, Inf)
    outcome3 <- colorBin("YlOrRd", domain = sp.ward.boundaries.map.filter3$n_crimes, bins = bins3)
    outcome3
  })
  
  output$m.crimes <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>% 
      addPolygons(data = sp.ward.boundaries.map, fillOpacity = 0, color = "#000000", 
                  weight = 1, opacity = 1, smoothFactor = 0, 
                  label = paste0("Ward: ", sp.ward.boundaries.map$ward),
                  labelOptions = labelOptions(interactive = TRUE, textsize = "15px")) %>% 
      addCircleMarkers(data = dt.crimes.map, 
                       lng = ~Longitude, 
                       lat = ~Latitude, 
                       popup = ~paste0("<h4>", Primary.Type,"</h4>",
                                       "<b>Date/Time: </b>", Date.Time,
                                       "<br><b>Ward: </b>", Ward,
                                       "<br><b>Primary Type: </b>", Primary.Type,
                                       "<br><b>Specific Type: </b>", Description,
                                       "<br><b>Location: </b>", Location.Description,
                                       "<br><b>Someone Arrested: </b>", Arrest,
                                       "<br><b>Domestic Incident: </b>", Domestic,
                                       "<br><b>Block: </b>", Block),
                       radius = ~ifelse(Arrest == "Yes", 5, 5),
                       color = ~pal(Type.Highlight),
                       stroke = FALSE, fillOpacity = 0.4) %>% 
      addLegend("topright", pal = pal, values = l.Type.Highlight,
                title = "Primary Type",
                opacity = 1)
  })
  
  observe({
    leafletProxy("m.crimes", data = dt.crimes.map.filter()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude, 
        popup = ~paste0("<h4>", Primary.Type, "</h4>",
                        "<b>Date/Time: </b>", Date.Time,
                        "<br><b>Ward: </b>", Ward,
                        "<br><b>Primary Type: </b>", Primary.Type,
                        "<br><b>Specific Type: </b>", Description,
                        "<br><b>Location: </b>", Location.Description,
                        "<br><b>Someone Arrested: </b>", Arrest,
                        "<br><b>Domestic Incident: </b>", Domestic,
                        "<br><b>Block: </b>", Block),
        radius = ~ifelse(Arrest == "Yes", 5, 5),
        color = ~pal(Type.Highlight),
        stroke = FALSE, fillOpacity = 0.4)
  })
  
  output$m.crimes.choropleth <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>% 
      addPolygons(data = sp.ward.boundaries.map, 
                  fillColor = ~pal2(sp.ward.boundaries.map$n_crimes),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  smoothFactor = 0, 
                  label = ~paste0("Ward ", sp.ward.boundaries.map$ward, " -- ",
                                  "# of Crimes: ", sp.ward.boundaries.map$n_crimes),
                  labelOptions = labelOptions(interactive = TRUE, textsize = "15px"),
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>% 
      addLegend(pal = pal2, values = sp.ward.boundaries.map$n_crimes, opacity = 1, title = "# of Crimes",
                position = "topright")
  })
  
  observe({
    sp.ward.boundaries.map.filter.save <- sp.ward.boundaries.map.filter()
    pal2.filter.save <- pal2.filter()
    leafletProxy("m.crimes.choropleth", data = sp.ward.boundaries.map.filter.save) %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(
        fillColor = ~pal2.filter.save(sp.ward.boundaries.map.filter.save$n_crimes),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        smoothFactor = 0, 
        label = ~paste0("Ward ", sp.ward.boundaries.map.filter.save$ward, " -- ",
                        "# of Crimes: ", sp.ward.boundaries.map.filter.save$n_crimes),
        labelOptions = labelOptions(interactive = TRUE, textsize = "15px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)) %>% 
      addLegend(pal = pal2.filter.save, values = sp.ward.boundaries.map.filter.save$n_crimes, opacity = 1, title = "# of Crimes",
                position = "topright")
  })
  
  # ---- Network Exploration ----
  
  graphInput <- reactive({
    if (input$node == 'District') {
    }
    all.vertices <- dt.crimes[!duplicated(District), list(District, type = TRUE)]
    if (input$node == 'Ward') {
      all.vertices <- dt.crimes[!duplicated(Ward), list(Ward, type = TRUE)]
    }
    all.primary.types <- dt.crimes[!duplicated(Primary.Type), list(Primary.Type, type = FALSE)]
    all.vertices <- rbind(all.vertices, all.primary.types, use.names = FALSE)
    all.vertices
  })
  
  graphObject <- reactive({
    
    if (input$node == 'District') {
      g.plot <- igraph::graph.data.frame(dt.crimes[, list(Primary.Type, District)], directed = FALSE, vertices = graphInput())
    } else if (input$node == 'Ward') {
      g.plot <- igraph::graph.data.frame(dt.crimes[, list(Primary.Type, Ward)], directed = FALSE, vertices = graphInput())
    }
    if (input$edge == 'Crime Type') {
      g.plot <- bipartite.projection(g.plot)$proj2
    } else if (input$edge == 'Location') {
      g.plot <- bipartite.projection(g.plot)$proj1
    }
    no.connections <- which(igraph::degree(g.plot) > input$degree)
    g.plot <- igraph::delete.vertices(g.plot, no.connections)
    g.plot
  })
  
  graphCentralities <- reactive({
    if (input$node == 'District') {
    }
    all.vertices <- dt.crimes[!duplicated(District), list(District, type = TRUE)]
    if (input$node == 'Ward') {
      all.vertices <- dt.crimes[!duplicated(Ward), list(Ward, type = TRUE)]
    }
    all.primary.types <- dt.crimes[!duplicated(Primary.Type), list(Primary.Type, type = FALSE)]
    all.vertices <- rbind(all.vertices, all.primary.types, use.names = FALSE)
    
    
    if (input$node == 'District') {
      g.plot <- igraph::graph.data.frame(dt.crimes[, list(Primary.Type, District)], directed = FALSE, vertices = all.vertices)
    } else if (input$node == 'Ward') {
      g.plot <- igraph::graph.data.frame(dt.crimes[, list(Primary.Type, Ward)], directed = FALSE, vertices = all.vertices)
    }
    if (input$edge == 'Crime Type') {
      g.plot <- bipartite.projection(g.plot)$proj2
    } else if (input$edge == 'Location') {
      g.plot <- bipartite.projection(g.plot)$proj1
    }
    no.connections <- which(igraph::degree(g.plot) > input$degree)
    g.plot <- igraph::delete.vertices(g.plot, no.connections)
    
    V(g.plot)$degree <- igraph::degree(g.plot)
    V(g.plot)$closeness <- igraph::closeness(g.plot)
    V(g.plot)$betweenness <- igraph::betweenness(g.plot)
    V(g.plot)$evcent <- igraph::evcent(g.plot)$vector
    
    dt.g.object <- data.table::data.table(igraph::get.data.frame(g.plot, "vertices"))
    dt.g.object.order <- data.frame(degreename = head(dt.g.object[order(-degree)], 20)$name,
                                    degree = head(dt.g.object[order(-degree)], 20)$degree,
                                    closenessname = head(dt.g.object[order(-closeness)], 20)$name,
                                    closeness = head(dt.g.object[order(-closeness)], 20)$closeness,
                                    betweennessname = head(dt.g.object[order(-betweenness)], 20)$name,
                                    betweenness = head(dt.g.object[order(-betweenness)], 20)$betweenness,
                                    evcentname = head(dt.g.object[order(-evcent)], 20)$name,
                                    evcent = head(dt.g.object[order(-evcent)], 20)$evcent)
    dt.g.object.order
  })
  
  output$graph <- threejs::renderScatterplotThree({
    #dt.crimes <- dt.crimes[Date.Time >= input$daterange[1] & Date.Time <= input$daterange[2]]
    
    threejs::graphjs(graphObject(), vertex.color = 'orange', vertex.size = 1, vertex.label = NA, edge.color = 'blue')
    #igraph::plot.igraph(g.plot, vertex.size = 3, vertex.label = NA)
  })
  
  #  output$networksummary <- DT::renderDT({
  #    dt.crimes <- dt.crimes[, list(Primary.Type, District, Ward)]
  #    
  #    if (input$node == 'District') {
  #      g.plot <- igraph::graph.data.frame(dt.crimes[, list(Primary.Type, District)], directed = FALSE, vertices = graphInput())
  #    } else if (input$node == 'Ward') {
  #      g.plot <- igraph::graph.data.frame(dt.crimes[, list(Primary.Type, Ward)], directed = FALSE, vertices = graphInput())
  #    }
  #    if (input$projection == 'Bipartite') {
  #      g.plot <- bipartite.projection(g.plot)$proj2
  #    } else if (input$projection == 'Network') {
  #      g.plot <- g.plot
  #    }
  #    
  #    no.connections <- which(igraph::degree(g.plot) == 0)
  #    igraph::delete.vertices(g.plot, no.connections)
  #    })
  
  output$centralitysummary <- renderTable({
    graphCentralities()
  })

  patch-4
  
  # ---- Advanced Analytics ----
  
  predictions <- reactive({
    dt.output.prediction <- data.table(matrix(ncol = 2, nrow = 1))
    colnames(dt.output.prediction) <- c("Primary.Type", "Prediction")
    dt.all.predictions <- data.table(matrix(ncol = 2, nrow = 0))
    colnames(dt.all.predictions) <- c("Primary.Type", "Prediction")
    
    dt.manual.predictions <- c(input$Race.White_pct, 
                               input$Race.Black_pct, 
                               input$Race.Asian_pct,
                               input$Ethnicity.Hispanic_pct,
                               input$Income,
                               input$Abandoned_Vehicles,
                               input$Garbage_Carts,
                               input$Rodent_Baiting,
                               input$Sanitation_Code_Complaints,
                               input$Graffiti_Removal,
                               input$Abandoned_Buildings,
                               input$Police_Stations,
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Race.White_pct"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Race.Black_pct"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Race.Asian_pct"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Ethnicity.Hispanic_pct"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Income"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Abandoned_Vehicles"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Garbage_Carts"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Rodent_Baiting"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Sanitation_Code_Complaints"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Graffiti_Removal"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Abandoned_Buildings"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Police_Stations"]
    )
    
    names(dt.manual.predictions) <- c(l.variables)
    
    for (ptype in l.primary.types) {
      predictor.values <- data.table(matrix(ncol = 0, nrow = 1))
      for (variables in l.variables) {
        value <- unname(dt.manual.predictions[variables])
        predictor.values <- cbind(predictor.values, value)
      }
      
      colnames(predictor.values) <- c(l.variables)
      
      prediction <- as.matrix(dt.regression.est[Primary.Type == ptype, ][, -c(1:2)]) * as.matrix(predictor.values)
      prediction <- sum(prediction) + dt.regression.est[Primary.Type == ptype, ]$Intercept
      
      if(round(prediction, 0) < 0) {
        dt.output.prediction$Primary.Type[1] <- ptype
        dt.output.prediction$Prediction[1] <- 0
        dt.all.predictions <- rbind(dt.all.predictions, dt.output.prediction)
      } else {
        dt.output.prediction$Primary.Type[1] <- ptype
        dt.output.prediction$Prediction[1] <- round(prediction, 0)
        dt.all.predictions <- rbind(dt.all.predictions, dt.output.prediction)
      }
    }
    dt.all.predictions
  })
  
  output$predictions <- renderTable({
    predictions()
  })  
  
=======
 main
}
