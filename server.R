#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


function(input, output) {
  
  # Read and reformat uploaded psy data
  psy <- reactive({
    req(input$upload_psy)
    
    inFile <- input$upload_psy
    initial <- read_csv(inFile$datapath) # "data_examples/psy_2a.csv"
    initial %>%
      mutate(month = lubridate::month(dt))
  })
  
  # Read and reformat uploaded env data
  env <- reactive({
    req(input$upload_env)
    
    inFile <- input$upload_env
    initial <- read_csv(inFile$datapath) # "data_examples/neon_vpd30.csv"
    initial 
  })
  
  # Dynamic selectInput for month
  output$dyn_month <- renderUI({
    req(input$upload_psy)
    
    selectInput("month", "Select month:", 
                unique(psy()$month))
  })
  
  # Dynamic selectInput for x variable, env data
  output$dyn_xvar <- renderUI({
    req(input$upload_env)
    
    varSelectInput("xvar", "Select x variable:", 
                env())
  })
  
  # Dynamic selectInput for y variable, 1st axis, env data
  output$dyn_yvar1 <- renderUI({
    req(input$upload_env)
    
    varSelectInput("yvar1", "Select y variable (left axis):", 
                env())
  })
  
  # Dynamic selectInput for y variable, 2nd axis, env data
  output$dyn_yvar2 <- renderUI({
    req(input$upload_env)
    
    varSelectInput("yvar2", "Select y variable (right axis):", 
                env())
  })
  
  # Dynamically updated slider input based on month selected
  output$dyn_range <- renderUI({
    req(input$month)
    
    foo <- psy() %>%
      filter(month == input$month) %>%
      pull(date) %>%
      range()
    
    sliderInput("daterange", "Select date range:",
                min = foo[1],
                max = foo[2],
                value = foo,
                width = '93%',
                ticks = FALSE)
  })
  
  # Initial selection of no points
  selected <- reactiveVal()
  
  # Create vector of FAlSE the length of the input psychrometer timeseries
  observeEvent(input$upload_psy, {
    selected(rep(FALSE, nrow(psy())))
  })

  # Create observed points within brush area
  observeEvent(input$plot1_brush, {
               brushed <- brushedPoints(psy(), input$plot1_brush, allRows = TRUE)$selected_
               selected(brushed | selected())
  })

  # Create observed points with click
  observeEvent(input$plot1_click, {
    clicked <- nearPoints(psy(), input$plot1_click, allRows = TRUE)$selected_
    selected(clicked | selected())
  })

  # Reset point if double clicked
  observeEvent(input$plot_reset, {
    dblclicked <- nearPoints(psy(), input$plot_reset, allRow = TRUE)$selected_
    
    temp <- selected()
    ind <- which(dblclicked == TRUE)
    temp[ind] <- FALSE
    selected(temp)
  })
  
  

  # ggplot timeseries of psy data
  output$p <- renderPlot({
    req(input$upload_psy)
    req(input$daterange)
    
    # Update with selected
    psy_temp <- psy() %>%
      mutate(sel = selected())
    
    # custom integer labels
    int_breaks <- function(x, n = 5) {
      l <- pretty(x, n)
      l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
    }

    # Filter and plot
    psy_temp %>%
      filter(month == input$month) %>%
      filter(date >= input$daterange[1],
             date <= input$daterange[2]) %>%
      ggplot() +
      geom_point(aes(x = dt, y = corrected_water_potential_m_pa,
                     color = sel)) +
      scale_y_continuous("Water potential", breaks = int_breaks) +
      scale_x_datetime(limits = c(as.POSIXct(paste(input$daterange[1], "00:00")),
                                  as.POSIXct(paste(input$daterange[2], "00:00"))),
                       date_breaks = "2 days",
                       date_labels = "%d") +
      scale_colour_manual(limits = c("TRUE", "FALSE"),
                            values = c( "#CC6677", "#117733")) +
      theme_bw(base_size = 16) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold")) +
      guides(color = "none")
  })
  
  # ggplot timeseries of env data
  # pick 2 inputs
  output$env <- renderPlot({
    req(input$upload_env)
    req(input$daterange)
    req(input$xvar, input$yvar1, input$yvar2)
    
    temp <- env() %>%
      filter(!!input$xvar >= input$daterange[1],
             !!input$xvar <= input$daterange[2]) 
    
    # For tuning the relative sizes of left and right y axes
    # ratio1 <- if ( max(temp$VPD,  na.rm = TRUE) > max(temp$p34_6,  na.rm = TRUE) ) {
    #   max(max(temp$VPD,  na.rm = TRUE), 
    #       max(temp$p34_6,  na.rm = TRUE),
    #       na.rm = TRUE) / 
    #     max(temp$p34_6, na.rm = TRUE)
    # } else {
    #   max(max(temp$VPD,  na.rm = TRUE), 
    #       max(temp$p34_6,  na.rm = TRUE),
    #       na.rm = TRUE) / 
    #     max(temp$VPD, na.rm = TRUE)
    # }
    # 
    # ratio1 <- if ( max(temp$!!!input$yvar1,  na.rm = TRUE) > max(temp$!!input$yvar2,  na.rm = TRUE) ) {
    #   max(max(temp$!!input$yvar1,  na.rm = TRUE), 
    #       max(temp$!!input$yvar2,  na.rm = TRUE),
    #       na.rm = TRUE) / 
    #     max(temp$!!input$yvar2, na.rm = TRUE)
    # } else {
    #   max(max(temp$!!input$yvar1,  na.rm = TRUE), 
    #       max(temp$!!input$yvar2,  na.rm = TRUE),
    #       na.rm = TRUE) / 
    #     max(temp$!!input$yvar1, na.rm = TRUE)
    # }
      
    lab_left <- as.character(input$yvar1)
    lab_right <- as.character(input$yvar2)
    print(is.character(lab_left))

    temp %>%
      ggplot() +
      geom_point(aes(x = !!input$xvar,
                     y = !!input$yvar1,
                     color = lab_left)) +
      geom_point(aes(x = !!input$xvar,
                     y = !!input$yvar2*1,
                     color = lab_right)) +
      scale_y_continuous(lab_left,
                         sec.axis = sec_axis(~./1,
                                             name = lab_right)) +
      scale_x_datetime(date_breaks = "2 days",
                       date_labels = "%d") +
      theme_bw(base_size = 16) +
      scale_color_manual(values = c(lab_left = "#6699CC",
                                    lab_right = "#44AA99")) +
      guides(color = "none") +
      theme(axis.title.y.left = element_text(color = "#6699CC", face = "bold"),
            axis.title.y.right = element_text(color = "#44AA99", face = "bold"),
            axis.title.x = element_blank())
      
    
  })
  
  
  
  # Sidebar table of data to remove
  output$brush_info_remove <- renderPrint({
    temp <- psy() %>%
      mutate(to_remove = selected()) %>%
      filter(to_remove == TRUE) %>%
      select(date, corrected_water_potential_m_pa) %>%
      rename(corrected_WP = corrected_water_potential_m_pa)
    return(print(temp, n = 1500))
  })
  
  # report all data
  clean_all <- reactive({
    psy_all <- psy() %>%
      mutate(to_remove == selected())
    return(all)
  })
  
  # report month data
  clean_month <- reactive({
    psy_month <- psy() %>%
      mutate(to_remove == selected()) %>%
      filter(month == input$month)
    return(psy_month)
  })
  
  # Button to download cleaned month
  output$download_clean_month <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(clean_month(), file)
    }
  )
  
  # Button to download cleaned all
  output$download_clean_all <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(clean_all(), file)
    }
  )
  
}