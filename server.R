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
    
    # find date and time - create dt
    if("dt" %nin% colnames(initial)) {
     d_name <- initial %>%
        select(where(is.Date)) %>%
        colnames()
     
     t_name <- initial %>%
       select(where(hms::is_hms)) %>%
       colnames()
     
     initial %>%
       mutate( # WIP to figure out adding on-the-fly conversion of data
         # dt = as.POSIXct(paste0(!!sym(d_name), !!sym(t_name))),
              month = lubridate::month(dt))
    } else {
      initial %>%
        mutate(month = lubridate::month(dt))
    }

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
  
  # Dynamic selectInput for PSY, psy data
  output$dyn_psyvar <- renderUI({
    req(input$upload_psy)
    
    # only provide POSIXct objects
    temp <- psy() %>%
      select(where(is.numeric))
    
    varSelectInput("psyvar", "Select WP variable:", 
                   temp)
  })
  
  # Dynamic selectInput for x variable, env data
  output$dyn_xvar <- renderUI({
    req(input$upload_env)
    
    # only provide POSIXct objects
    temp <- env() %>%
      select(where(is.POSIXct))
    
    varSelectInput("xvar", "Select x variable:", 
                   temp)
  })
  
  # Dynamic selectInput for y variable, 1st axis, env data
  output$dyn_yvar1 <- renderUI({
    req(input$upload_env)
    
    # only provide numeric objects
    temp <- env() %>%
      select(where(is.numeric))
    
    varSelectInput("yvar1", "Select y variable (left axis):", 
                   temp)
  })
  
  # Dynamic selectInput for y variable, 2nd axis, env data
  output$dyn_yvar2 <- renderUI({
    req(input$upload_env)
    
    # only provide numeric objects
    temp <- env() %>%
      select(where(is.numeric))
    
    varSelectInput("yvar2", "Select y variable (right axis):", 
                   temp)
  })
  
  # Dynamically updated slider input based on month selected
  output$dyn_range <- renderUI({
    req(input$month)
    
    temp <- psy() %>%
      filter(month == input$month) %>%
      pull(date) %>%
      range()
    
    sliderInput("daterange", "Select date range:",
                min = temp[1],
                max = temp[2],
                value = temp,
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
  output$psy_plot <- renderPlot({
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
      geom_point(aes(x = dt, y = !!input$psyvar,
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
  # require 2 inputs and the x variable as POSIX object
  output$env_plot <- renderPlot({
    req(input$upload_env)
    req(input$daterange)
    req(input$xvar, input$yvar1, input$yvar2)
    
    temp <- env() %>%
      filter(!!input$xvar >= input$daterange[1],
             !!input$xvar <= input$daterange[2]) 
    
    # create character strings of selected columns
    lab_left <- as.character(input$yvar1)
    lab_right <- as.character(input$yvar2)

    # create ratio for scaling left and right axes
    ratio1 <- if ( max(temp[[lab_left]],  na.rm = TRUE) > max(temp[[lab_right]], na.rm = TRUE)) {
      max(max(temp[[lab_left]],  na.rm = TRUE),
          max(temp[[lab_right]],  na.rm = TRUE)) /
        min(max(temp[[lab_left]],  na.rm = TRUE),
            max(temp[[lab_right]],  na.rm = TRUE))
    } else {
      min(max(temp[[lab_left]],  na.rm = TRUE),
          max(temp[[lab_right]],  na.rm = TRUE))  / 
        max(max(temp[[lab_left]],  na.rm = TRUE),
            max(temp[[lab_right]],  na.rm = TRUE)) 
    }
    print(max(temp[[lab_left]],  na.rm = TRUE) > max(temp[[lab_right]], na.rm = TRUE))
    print(ratio1)

    # For tuning the relative sizes of left and right y axes
    # ratio1 <- if ( max(temp$VPD,  na.rm = TRUE) > max(temp$p34_6,  na.rm = TRUE) ) {
    #   max(max(temp$VPD,  na.rm = TRUE),
    #       max(temp$p34_6,  na.rm = TRUE),
    #       na.rm = TRUE) /
    #     max(temp$p34_6, na.rm = TRUE)
    # } else {
    #   max(temp$p34_6, na.rm = TRUE) /
    #     max(max(temp$VPD,  na.rm = TRUE),
    #         max(temp$p34_6,  na.rm = TRUE),
    #         na.rm = TRUE)
    # }

    # Name color vector
    color_vec <- c("#6699CC","#44AA99")
    names(color_vec) <- c(lab_left, lab_right)

    temp %>%
      ggplot() +
      geom_point(aes(x = !!input$xvar,
                     y = !!input$yvar1,
                     color = lab_left)) +
      geom_point(aes(x = !!input$xvar,
                     y = !!input$yvar2*ratio1,
                     color = lab_right)) +
      scale_y_continuous(lab_left,
                         sec.axis = sec_axis(~./ratio1,
                                             name = lab_right)) +
      scale_x_datetime(date_breaks = "2 days",
                       date_labels = "%d") +
      theme_bw(base_size = 16) +
      scale_color_manual(values = color_vec) +
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
      mutate(to_remove = selected())
    return(psy_all)
  })
  
  # report month data
  clean_month <- reactive({
    psy_month <- psy() %>%
      mutate(to_remove = selected()) %>%
      filter(month == input$month)
    return(psy_month)
  })
  
  # Button to download cleaned month
  output$download_clean_month <- downloadHandler(
    filename = function() {
      glue::glue("clean-{input$month}-{input$upload_psy}.csv")
    },
    content = function(file) {
      write_csv(x = clean_month(), file = file)
    }
  )
  
  # Button to download cleaned all
  output$download_clean_all <- downloadHandler(
    filename = function() {
      glue::glue("clean-{input$upload_psy}.csv")
    },
    content = function(file) {
      write_csv(clean_all(), file)
    }
  )
  
}