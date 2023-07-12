#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


function(input, output) {
  
  # Read and reformat uploaded data
  psy <- reactive({
    req(input$upload_psy)
    
    inFile <- input$upload_psy
    
    initial <- read_csv(inFile$datapath) # "data_examples/psy_2a.csv"
    print(initial)
    
    initial %>%
      mutate(month = lubridate::month(dt))
  })
  
  # Dynamic selectInput for month
  output$dyn_month <- renderUI({
    req(input$upload_psy)
    
    selectInput("month", "Select month:", 
                unique(psy()$month))
    
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
                width = '100%'
    )
    
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

    # Filter and plot
    psy_temp %>%
      filter(month == input$month) %>%
      filter(date >= input$daterange[1],
             date <= input$daterange[2]) %>%
      ggplot() +
      geom_point(aes(x = dt, y = corrected_water_potential_m_pa,
                     color = sel)) +
      scale_colour_discrete(limits = c("TRUE", "FALSE")) +
      theme_bw(base_size = 16)
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