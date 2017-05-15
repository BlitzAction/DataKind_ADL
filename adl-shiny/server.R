shinyServer(function(input, output, session) {
  
  #### Create Reactive Values for Dashboard 1 ####
  selected_values = reactiveValues(state = "All states",
                                   city = "All cities",
                                   date = 2015,
                                   #geo_data_totals = crime_by_city_summed[crime_by_city_summed$date == 2015, ],
                                   geo_data_by_type = crime_by_city_long[crime_by_city_long$date == 2015, ],
                                   rows_selected = NULL,
                                   population = population_est[population_est$year == 2015, ],
                                   message = "")
  
  #### Observe Date Selection ####
  observeEvent(input$input_years1, {
    selected_values$date = input$input_years1
    selected_values$city = "All cities"
    #selected_values$geo_data_totals = crime_by_city_summed[crime_by_city_summed$date == input$input_years1, ]
    selected_values$geo_data_by_type = crime_by_city_long[crime_by_city_long$date == input$input_years1, ]
    selected_values$population = population_est[population_est$year == input$input_years1, ]
  })
  
  #### Observe State Selection on Left Panel ####
  observeEvent(input$input_states1, {
    selected_values$state = input$input_states1
    selected_values$city = "All cities"
  })
  
  #### Observe State Selection on Map ####
  observeEvent(input$map_shape_click$id, {
    selected_values$state = input$map_shape_click$id
    selected_values$city = "All cities"
  })
  
  #### Observe State Selection on Map ####
  observeEvent(input$map_marker_click$id, {
    selected_values$city = input$map_marker_click$id
  })
  
  #### Dashboard 2 Observe Events and Reactivity ####
  
  #### Create Reactive Values ####
  selected_values2 = reactiveValues(state = "All states",
                                    date = 2015,
                                    #geo_data_totals = crime_by_city_summed[crime_by_city_summed$date == 2015, ],
                                    geo_data_by_type = crime_by_city_long[crime_by_city_long$date == 2015, ],
                                    type = "Disability",
                                    population = population_est[population_est$year == 2015, ],
                                    message = "")
  
  data_to_plot2_3 = reactive({
    
    data = crime_by_state_long
    
    # If no rows and just 1 state was selected.
    if(selected_values2$state != "All states"){
      
      data = data[data$state == selected_values2$state, -2]
      
      # If All states were selected
    }else{
      
      data = aggregate(crime_count ~ crime_type + date,
                       data = data, na.action = "na.omit",
                       sum)
    }
    
    return(data)
    
  })
  
  
  
  #### Dashboard 1 ####
  output$map2_1 = renderLeaflet({
    drawStates2(data = data_to_plot2_1())
  })
  
  
  #### Create Reactive Values ####
  selected_values2 = reactiveValues(state = "All states",
                                    date = 2015,
                                    #geo_data_totals = crime_by_city_summed[crime_by_city_summed$date == 2015, ],
                                    geo_data_by_type = crime_by_city_long[crime_by_city_long$date == 2015, ],
                                    type = "Disability",
                                    population = population_est[population_est$year == 2015, ],
                                    message = "")
  
  #### Observe Date 2 Selection ####
  
  observeEvent(input$input_years2, {
    selected_values2$date = input$input_years2
    #selected_values2$geo_data_totals = crime_by_city_summed[crime_by_city_summed$date == input$input_years2, ]
    selected_values2$geo_data_by_type = crime_by_city_long[crime_by_city_long$date == input$input_years2, ]
    selected_values2$population = population_est[population_est$year == input$input_years2, ]
    
    Markers_Checked <- input$layers_1
    Markers_Test = c("Did Not Report", "Reported 0")
    Markers_Hide = Markers_Test[which(!Markers_Test %in% Markers_Checked)]
    
    leafletProxy("map2_1") %>% hideGroup(c(Markers_Hide))
    leafletProxy("map2_1") %>% showGroup(c(Markers_Checked))
    
  })
  
  #### Observe State 2 Selection on Left Panel ####
  observeEvent(input$input_states2, {
    selected_values2$state = input$input_states2
    
    Markers_Checked <- input$layers_1
    Markers_Test = c("Did Not Report", "Reported 0")
    Markers_Hide = Markers_Test[which(!Markers_Test %in% Markers_Checked)]
    
    leafletProxy("map2_1") %>% hideGroup(c(Markers_Hide))
    leafletProxy("map2_1") %>% showGroup(c(Markers_Checked))
    
  })
  
  #### Observe State Click 2 Selection on Map ####
  observeEvent(input$map2_shape_click$id, {
    selected_values2$state = input$map2_shape_click$id
  })
  
  #### Observe State Click 2 Selection on Map ####
  observeEvent(input$map2_1_shape_click$id, {
    selected_values2$state = input$map2_1_shape_click$id
    
    Markers_Checked <- input$layers_1
    Markers_Test = c("Did Not Report", "Reported 0")
    Markers_Hide = Markers_Test[which(!Markers_Test %in% Markers_Checked)]
    
    leafletProxy("map2_1") %>% hideGroup(c(Markers_Hide))
    leafletProxy("map2_1") %>% showGroup(c(Markers_Checked))
    
  })
  
  #### Observe Statute Type Selection on Map ####
  observeEvent(input$input_type2, {
    selected_values2$type = input$input_type2
  })
  
  data_to_plot2_1 = reactive({
    data = dnr_zero[dnr_zero$date == selected_values2$date, ]
    return(data)
  })
  
  
  #### Draw States On Map on Dashboard 1 ####  
  drawStates2 = function(data) {
    
    # Bounding box around the US
    bbox = data.frame(b1=-126.4, b2=24, b3=-64.2, b4=51.1)
    
    # Box 1 is on the top left of the page (Total crime counts)
    
    selected_date = selected_values2$date
    #str(zero_summary)
    bins = c(0, 5, 10, 15, 20)
    map_df = join(map_df,
                  zero_summary[as.character(zero_summary$date) == selected_date, -1],
                  by = "state")
    
    df = data
    
    selected_state = selected_values2$state
    
    if(selected_values2$state != "All states"){
      bbox = state_bounding_boxes[state_bounding_boxes$state == selected_state, ]
      #df = df[df$state == selected_values2$state, ]
    }
    
    map_df = join(map_df,
                  selected_values2$population[-1, -c(1, 3)])
    
    map_df$population = format(map_df$population, decimal.mark=",", big.mark=",")
    
    pal = colorBin(or_red_pal_1, domain = map_df$total_crimes, bins = bins)
    
    labels = sprintf(
      "Number of cities in %s that<br/>reported 0 hate crimes: <br/><strong>%g</strong>",
      map_df$state, 
      map_df$total_crimes
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(df) %>%
      addTiles() %>%
      fitBounds(bbox$b1, bbox$b2, bbox$b3, bbox$b4)%>%
      addPolygons(data = us_states_geo,
                  group = "State borders",
                  layerId = us_states_geo@data$name,
                  fillColor = ~pal(map_df$total_crimes),
                  weight = 2,
                  opacity = 0.5,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.3,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.4,
                    bringToFront = FALSE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 "border-color" = "rgba(0,0,0,0)",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
                  
      )  %>%
      
      addCircleMarkers(radius = 5, 
                       lng = df$longitude[df$dnr_zero == "Did Not Report"],
                       lat = df$latitude[df$dnr_zero == "Did Not Report"],
                       group = "Did Not Report",
                       layerId = as.character(df$city[df$dnr_zero == "Did Not Report"]),
                       label = as.character(paste0(df$city[df$dnr_zero == "Did Not Report"],
                                                   ", ",df$abbr[df$dnr_zero == "Did Not Report"])),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto',
                                                   style = list("font-weight" = "strong", 
                                                                "border-color" = "rgba(0,0,0,0)",
                                                                padding = "3px 8px"
                                                     ),
                                                   textsize = "12px"),
                       color = "red",
                       opacity = 1,
                       fillColor = "red",
                       fillOpacity = 1,
                       fill = TRUE) %>%
      
      addCircleMarkers(radius = 5, 
                       lng = df$longitude[df$dnr_zero == "Reported 0 in at least 1 quarter"],
                       lat = df$latitude[df$dnr_zero == "Reported 0 in at least 1 quarter"],
                       group = "Reported 0",
                       layerId = as.character(df$city[df$dnr_zero == "Reported 0 in at least 1 quarter"]),
                       label = as.character(paste0(df$city[df$dnr_zero == "Reported 0 in at least 1 quarter"],
                                                   ", ", 
                                                   df$abbr[df$dnr_zero == "Reported 0 in at least 1 quarter"])),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto',
                                                   style = list("font-weight" = "strong", 
                                                                "border-color" = "rgba(0,0,0,0)",
                                                                padding = "3px 8px"),
                                                   textsize = "12px"),
                       color = "blue",
                       opacity = 1,
                       fillColor = "blue",
                       fillOpacity = 1,
                       fill = TRUE)%>%
      
      addLegend(colors = c("blue", "red"),
                labels = c(paste0(tags$b("Reported 0"), " hate crimes"), paste0(tags$b("Did not report"), " any hate crimes")),
                opacity = 1,
                title = "Cities with population > 100,000",
                position = "bottomleft")%>%
      addLegend(pal = pal, 
                values = ~map_df$total_crimes, 
                opacity = 0.6, 
                title = "Number of cities that<br>reported 0 hate crimes",
                position = "bottomright")
  }
  
  
  observeEvent(input$layers_1, { 
    
    Markers_Checked <- input$layers_1
    Markers_Test = c("Did Not Report", "Reported 0")
    Markers_Hide = Markers_Test[which(!Markers_Test %in% Markers_Checked)]
    
    leafletProxy("map2_1") %>% hideGroup(c(Markers_Hide))
    leafletProxy("map2_1") %>% showGroup(c(Markers_Checked))
  }, ignoreNULL = FALSE)
  
  
  output$annotation1 <- renderText({
    if(is.null(input$layers_1)){
      paste("")
    } else if(input$layers_1 == "Did Not Report"){

      HTML(paste0("The ",tags$b("red markers"), " are the cities of more than 100,000 in population that ",
        tags$b("did not report"), " any hate crimes to the FBI in the selected year."))
      
    } else if(input$layers_1 == "Reported 0"){
      
      HTML(paste0("The ",tags$b("blue markers"), " represent cities with populations over 100,000 that ",
                  tags$b("reported zero (0)"), " hate crimes to the FBI in the selected year."))
      
    } 
  })
  
  output$annotation2 <- renderText({
    if(is.null(input$layers_1)){
      paste("")
    } else if(length(input$layers_1) == 2){
      
      HTML(paste0("The ",tags$b("blue markers"), " represent cities with populations over 100,000 that ",
                  tags$b("reported zero (0)"), " hate crimes to the FBI in the selected year."))
    }
  })
  
  
  #### Dashboard 2 ####
  
  output$Total <- renderText({

    if(is.null(selected_values4$marker_click)){

      if(selected_values4$state == "All states"){
        
       # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Total"]
          paste0("Total: ", Disability_count)
          
        # } else {
        #       
        # Disability_year = crime_by_state_long[crime_by_state_long$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Total: ", Disability_count)
        # }

      } else {

        Disability_year = crime_by_state_long[crime_by_state_long$date == input$input_years4, ]
        Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
        Disability_count = sum(Disability_state$crime_count, na.rm = T)
        paste0("Total: ", Disability_count)
      }

    } else {

      Disability_year = crime_by_city_long[crime_by_city_long$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Total: ", Disability_count)

    }
  })
  
  output$Ethnicity <- renderText({
    
    if(input$input_years4 == 2015){
      return("")
      
    }else if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Ethnicity"]
          paste0("Ethnicity: ", Disability_count)
          
        # } else {
        #   
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Ethnicity", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Ethnicity: ", Disability_count)
        # }
        
      } else {
        
        Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Ethnicity", ]
        Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
        Disability_count = sum(Disability_state$crime_count, na.rm = T)
        paste0("Ethnicity: ", Disability_count)
        
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Ethnicity", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Ethnicity: ", Disability_count)
      
    }
  })
  
  output$Race_Ethnicity_Ancestry <- renderText({
    
    if(input$input_years4 != 2015){
      return("")
      
    }else if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Race/Ethnicity"]
          paste0("Race/Ethnicity: ", Disability_count)
          
        # } else {
        # 
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Race/Ethnicity", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Race/Ethnicity: ", Disability_count)
        # }
        
      } else {
        
        Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Race/Ethnicity", ]
        Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
        Disability_count = sum(Disability_state$crime_count, na.rm = T)
        paste0("Race/Ethnicity: ", Disability_count)
        
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Race/Ethnicity", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Race/Ethnicity: ", Disability_count)
      
    }
  })
  
  output$Anti_Jewish <- renderText({
    
    if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Anti Jewish"]
          paste0("Anti Jewish: ", Disability_count)
          
        # } else {
        # 
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Anti Jewish", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Anti Jewish: ", Disability_count)
        # }
        
      } else {
        
        Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Anti Jewish", ]
        Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
        Disability_count = sum(Disability_state$crime_count, na.rm = T)
        paste0("Anti Jewish: ", Disability_count)
        
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Anti Jewish", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Anti Jewish: ", Disability_count)
    }
  })
  
  output$Anti_Muslim <- renderText({
    
    if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Anti Muslim"]
          paste0("Anti Muslim: ", Disability_count)
          
        # } else {
        # 
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Anti Muslim", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Anti Muslim: ", Disability_count)
        # }
        
      } else {
        
        Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Anti Muslim", ]
        Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
        Disability_count = sum(Disability_state$crime_count, na.rm = T)
        paste0("Anti Muslim: ", Disability_count)
        
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Anti Muslim", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Anti Muslim: ", Disability_count)
    }
  })
  
  
  output$Disability <- renderText({
    
    if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Disability"]
          paste0("Disability: ", Disability_count)
          
        # } else {
        # 
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Disability", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Disability: ", Disability_count)
        # }
        
      } else {
      
      Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Disability", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = T)
      paste0("Disability: ", Disability_count)
      
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Disability", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Disability: ", Disability_count)
    }
  })
  
  output$Sexual_Orientation <- renderText({
    
    if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Sexual Orientation"]
          paste0("Sexual Orientation: ", Disability_count)
          
        # } else {
        # 
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Sexual Orientation", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Sexual Orientation: ", Disability_count)
        # }
        
      } else {
      
      Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Sexual Orientation", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = T)
      paste0("Sexual Orientation: ", Disability_count)
      
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Sexual Orientation", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Sexual Orientation: ", Disability_count)
      
    }
  })
  
  output$Race <- renderText({
    
    if(input$input_years4 == 2015){
      return("")
    
      }else if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Race"]
          paste0("Race: ", Disability_count)
          
        # } else {
        # 
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Race", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Race: ", Disability_count)
        # }
        
      } else {
      
      Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Race", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = T)
      paste0("Race: ", Disability_count)
      
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Race", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Race: ", Disability_count)
      
    }
  })
  
  output$Gender_Identity <- renderText({
    
    if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Gender Identity"]
          paste0("Gender Identity: ", Disability_count)
          
        # } else {
        # 
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Gender Identity", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Gender Identity: ", Disability_count)
        # }
        
      } else {
      
      Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Gender Identity", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = T)
      paste0("Gender Identity: ", Disability_count)
      
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Gender Identity", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Gender Identity: ", Disability_count)
      
    }
  })
  
  output$Gender <- renderText({
    
    if(is.null(selected_values4$marker_click)){
      
      if(selected_values4$state == "All states"){
        
        # if(input$input_type2 == "All types"){
          
          Disability_year = crime_national_long[crime_national_long$date == input$input_years4, ]
          Disability_count = Disability_year$crime_count[Disability_year$crime_type == "Gender"]
          paste0("Gender: ", Disability_count)
          
        # } else {
        # 
        # Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Gender", ]
        # Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
        # Disability_count = sum(Disability_year$crime_count, na.rm = T)
        # paste0("Gender: ", Disability_count)
        # }
        
      } else {
      
      Disability_data = crime_by_state_long[crime_by_state_long$crime_type == "Gender", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$state == selected_values4$state, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = T)
      paste0("Gender: ", Disability_count)
      
      }
      
    } else {
      Disability_data = crime_by_city_long[crime_by_city_long$crime_type == "Gender", ]
      Disability_year = Disability_data[Disability_data$date == input$input_years4, ]
      Disability_state = Disability_year[Disability_year$city == selected_values4$marker_click, ]
      Disability_count = sum(Disability_state$crime_count, na.rm = F)
      paste0("Gender: ", Disability_count)
      
    }
  })
  
  output$Summary_label <- renderText({
    if(is.null(selected_values4$marker_click)){
      paste0(selected_values4$state, " summary")
    } else {
      paste0(selected_values4$marker_click, ", ", state_abbr$abbr[intersect(grep(selected_values4$marker_click, state_abbr$city), 
                                                                            grep(selected_values4$state, state_abbr$state))[1]], " summary")
    }
  })
  
  data_to_plot2_2 = reactive({
    
    data = crime_by_city_long[crime_by_city_long$date == selected_values4$date, ]
    
    if(selected_values2$type == "All types"){
      data = aggregate(crime_count ~ city + state + latitude + longitude, data = data, sum)
    }else{
      data = data[data$crime_type == selected_values2$type, ]
    }
    
    data = data[data$crime_count > 0, ]
    
    return(data)
  })
  
  output$map2_2 = renderLeaflet({
    drawStates4(data = data_to_plot2_2())
  })
  
  #### Create Reactive Values ####
  selected_values4 = reactiveValues(state = "All states",
                                    date = 2015,
                                    #geo_data_totals = crime_by_city_summed[crime_by_city_summed$date == 2015, ],
                                    geo_data_by_type = crime_by_city_long[crime_by_city_long$date == 2015, ],
                                    type = "Disability",
                                    population = population_est[population_est$year == 2015, ],
                                    message = "",
                                    marker_click = NULL)
  
  #### Observe Date 2 Selection ####
  observeEvent(input$input_years4, {
    selected_values4$date = input$input_years4
    #selected_values4$geo_data_totals = crime_by_city_summed[crime_by_city_summed$date == input$input_years4, ]
    selected_values4$geo_data_by_type = crime_by_city_long[crime_by_city_long$date == input$input_years4, ]
    selected_values4$population = population_est[population_est$year == input$input_years4, ]
  })
  
  #### Observe State 2 Selection on Left Panel ####
  observeEvent(input$input_states4, {
    selected_values4$state = input$input_states4
    selected_values4$marker_click = NULL
  })
  
  #### Observe State Click 2 Selection on Map ####
  observeEvent(input$map2_2_shape_click$id, {
    selected_values4$state = input$map2_2_shape_click$id
    selected_values4$marker_click = NULL
  })
  
  observeEvent(input$map2_2_marker_click$id, {
    selected_values4$marker_click = input$map2_2_marker_click$id
  })
  
  #### Draw States On Map on Dashboard 2 ####  
  drawStates4 = function(data) {
    
    # Bounding box around the US
    bbox = data.frame(b1 = -126.4, b2 = 24, b3 = -64.2, b4 = 51.1)
    
    # Box 1 is on the top left of the page (Total crime counts)
    selected_date = selected_values4$date
    
    bins = c(0, 10, 20, 30, 40, 50, 100, Inf)
    
    # test = map_df
    # map_df = test
    # selected_date = 2015
    
    map_df = join(map_df, 
                  crime_by_state_long[crime_by_state_long$date == selected_date, -1], 
                  by = "state")
    
    if(selected_values2$type == "All types"){
      temp = aggregate(crime_count ~ state, data = map_df, sum)
      diff = setdiff(map_df$state, temp$state)
      if(length(diff) > 0 ){
        temp = rbind(temp,
                     data.frame(state = diff,
                                crime_count = rep(NA, length(diff))))
      }
      map_df = temp
      map_df = map_df[match(as.character(us_states_geo@data$name), map_df$state), ]
      map_df = map_df[-52, ]
      
    }else{
      NAs = which(is.na(map_df$crime_type))
      TRUEs = which(map_df$crime_type == selected_values2$type)
      selected = sort(c(TRUEs, NAs))
      map_df = map_df[selected, ]
      map_df$crime_type[is.na(map_df$crime_type)] = selected_values2$type
      
    }
    
    map_df = join(map_df,
                  selected_values4$population[-1, -c(1, 3)], by = "state")
    
    map_df$population = format(map_df$population, decimal.mark=",", big.mark=",")
    
    
    
    df = data
    selected_state = selected_values4$state
    
    if(selected_values4$state != "All states"){
      bbox = state_bounding_boxes[state_bounding_boxes$state == selected_state, ]
    }
    
    pal = colorBin(or_red_pal_2, domain = map_df$crime_count, bins = bins)
    
    labels = sprintf(
      "Hate crimes reported in %s: <br/><strong>%g</strong><br/>Total state population: <br/><strong>%s</strong>",
      map_df$state, 
      map_df$crime_count,
      map_df$population
    ) %>% lapply(htmltools::HTML)
    
    #print(df[df$state == selected_state, ])
    
    leaflet(df) %>%
      addTiles() %>%
      fitBounds(bbox$b1, bbox$b2, bbox$b3, bbox$b4)%>%
      addPolygons(data = us_states_geo,
                  group = "State borders",
                  layerId = us_states_geo@data$name,
                  fillColor = ~pal(map_df$crime_count),
                  weight = 2,
                  opacity = 0.5,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.3,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.4,
                    bringToFront = FALSE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 "border-color" = "rgba(0,0,0,0)",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
                  
      )  %>%
      addCircleMarkers(radius = 5, 
                       group = "Cities in state",
                       lat = df$latitude[df$state == selected_state],
                       lng = df$longitude[df$state == selected_state],
                       layerId = as.character(df$city[df$state == selected_state]),
                       label = ~as.character(paste0(df$city[df$state == selected_state], ": ", df$crime_count[df$state == selected_state])),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto',
                                                   style = list("font-weight" = "strong", 
                                                                "border-color" = "rgba(0,0,0,0)",
                                                                padding = "3px 8px"),
                                                   textsize = "12px"),
                       color = "blue",
                       opacity =.3,
                       fillColor = "blue",
                       fillOpacity = .3,
                       fill = TRUE) %>%
      
      addLegend(pal = pal, 
                values = ~map_df$crime_count, 
                opacity = 0.6, 
                title = "Number of hate crimes",
                position = "bottomright")
  }
  
  ####### Dashboard 3 #######
  
  #### Map 3 ####
  output$map2_4 = renderLeaflet({
    drawStates3(data = data_to_plot2_4())
  })
  
  #### Box 3 (Tab 1): Interactive Table of Statutes by Enhancement ####
  output$table_by_enhancement = DT::renderDataTable({
    datatable(statutes_by_state_by_enhancement,
              rownames = FALSE,
              class = 'cell-border stripe',
              container = sketch,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 10,
                             autoWidth = FALSE))%>%
      formatStyle(
        c("race_religion_ethnicity", "sexual_orientation", "gender", "gender_identity", "disability", "other"),
        backgroundColor = styleEqual(
          c("YES", "NO"), c("#CCFFCC", "#FFCCCC")
        )
      )
  }, server = TRUE)
  
  #### Box 3 (Tab 2): Interactive Table of Statutes by Other Types ####
  output$table_by_other_types = DT::renderDataTable({
    datatable(statutes_by_state_other_types,
              rownames = FALSE,
              class = 'cell-border stripe',
              colnames = c("State", "Civil Action", 
                           "Data Collection", "Police Training", 
                           "Instit. Vandalism", "Cross Burning"),
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 5,
                             autoWidth = FALSE))
  }, server = TRUE)
  
  observeEvent(input$input_type3, {
    if(input$input_type3 == "Other"){
      input_value = "Other Statutes"
    } else {
      input_value = "Statutes by Enhancement Type"
    }
    updateTabsetPanel(session, inputId = "tabset3", selected = input_value)
  })
  
  #### Dashboard 2 Observe Events and Reactivity ####
  
  #### Observe Statute Type Selection on Map ####
  data_to_plot2_4 = reactive({
    
    col_id = statute_types$statute_column[statute_types$type == selected_values3$type]
    data = data.frame(state = statutes_by_state_by_enhancement_2$state[statutes_by_state_by_enhancement_2[, col_id] == "YES"],
                      color = statute_types$statute_color[statute_types$type == selected_values3$type], 
                      stringsAsFactors = FALSE)
    
    return(data)
  })
  
  #### Dashboard 2 Observe Events and Reactivity ####
  levels(as.factor(crime_by_city_long$crime_type))
  #### Create Reactive Values ####
  selected_values3 = reactiveValues(state = "All states",
                                    date = 2015,
                                    #geo_data_totals = crime_by_city_summed[crime_by_city_summed$date == 2015, ],
                                    geo_data_by_type = crime_by_city_long[crime_by_city_long$date == 2015, ],
                                    type = "Disability",
                                    population = population_est[population_est$year == 2015, ],
                                    message = "")
  
  #### Observe Statute Type Selection on Map ####
  observeEvent(input$input_type3, {
    if(input$input_type3 == "Anti Jewish" || input$input_type3 == "Anti Muslim"){ 
    selected_values3$type = input$input_type3
    
    } else{
      selected_values3$type = input$input_type3
    }
  })
  
  
  #### Draw States On Map on Dashboard 2 ####  
  drawStates3 = function(data) {
    
    # Bounding box around the US
    bbox = data.frame(b1 = -126.4, b2 = 24, b3 = -64.2, b4 = 51.1)
    
    map2_df = join(map2_df, 
                   data, 
                   by = "state")
    
    map2_df$color[is.na(map2_df$color)] = "#ff0000"
    map2_df$color[map2_df$state == "Utah"] = "#A0A0A0"
    
    map2_df = join(map2_df,
                   selected_values3$population[-1, -c(1, 3)])
    
    map2_df$population = format(map2_df$population, decimal.mark=",", big.mark=",")
    
    labels = sprintf(
      "Hate Crime Statutes in %s include: <br/><strong>%s</strong>",
      map2_df$state, 
      statues_tooltip_2$label
    ) %>% lapply(htmltools::HTML)
    
    if(input$input_type3 == "All"){
      colors_to_plot = statues_tooltip_2$color
      legend_to_plot = colors_for_map_3
      legend_title = "Number of hate </br>crime laws enacted"
      
    }else{
      colors_to_plot = map2_df$color
      legend_to_plot = data.frame(num_yes = c("Hate crime laws enacted", "Hate crime laws absent"),
                                  color = c("#00FF00", "#ff0000"),
                                  stringsAsFactors = FALSE)
      legend_title = ""
    }
    
    leaflet(map2_df) %>%
      fitBounds(bbox$b1, bbox$b2, bbox$b3, bbox$b4)%>%
      addPolygons(data = composite_usa_map,
                  group = "State borders",
                  layerId = composite_usa_map@data$name,
                  fillColor = ~colors_to_plot,
                  weight = 2,
                  opacity = 0.5,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.4,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.6,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 "border-color" = "rgba(0,0,0,0)",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addLegend(colors = legend_to_plot$color,
                labels = legend_to_plot$num_yes, 
                opacity = 0.6,
                title = legend_title,
                position = "bottomleft")
  }
  
  ##### Dynamic UI for Hate Crimes by Type Tab #####
  output$dynamic_ui = renderUI({
    if (is.null(input$input_years4))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_years4,
           
           "2004" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           "2005" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           "2006" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           "2007" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           "2008" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           "2009" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           "2010" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           "2011" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           "2012" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           
           "2013" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           
           "2014" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,6,7,3,8,4,5)]),
                                 selected = "All types",
                                 inline = FALSE),
           
           "2015" = radioButtons("input_type2",
                                 label = NULL,
                                 choices = c("All types", crime_types[c(1,2,7,3:5,9)]),
                                 selected = "All types",
                                 inline = FALSE)
    )
  })
})