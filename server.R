#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


function(input, output) {
  
  # Dynamically updated slider input based on month selected
  output$dyn_range <- renderUI({
    req(input$month)
    
    foo <- psy %>%
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
  selected <- reactiveVal(rep(FALSE, nrow(psy)))

  # Create observed points within brush area
  observeEvent(input$plot1_brush, {
               brushed <- brushedPoints(psy, input$plot1_brush, allRows = TRUE)$selected_
               selected(brushed | selected())
  })

  # Create observed points with click
  observeEvent(input$plot1_click, {
    clicked <- nearPoints(psy, input$plot1_click, allRows = TRUE)$selected_
    selected(clicked | selected())
  })

  # Reset point if double clicked
  observeEvent(input$plot_reset, {
    dblclicked <- nearPoints(psy, input$plot_reset, allRow = TRUE)$selected_
    
    temp <- selected()
    ind <- which(dblclicked == TRUE)
    temp[ind] <- FALSE
    selected(temp)
  })
  
  
  # Initial selection of whole dataset
  # psy_out <- reactiveValues(data = psy)
  
  # ggplot timeseries of psy data
  output$p <- renderPlot({
    req(input$daterange)
    
    # Update with selected
    psy$sel <- selected()

    # Filter and plot
    psy %>%
      filter(month == input$month) %>%
      filter(date >= input$daterange[1],
             date <= input$daterange[2]) %>%
      ggplot() +
      geom_point(aes(x = dt, y = corrected_water_potential_m_pa,
                     color = sel)) +
      scale_colour_discrete(limits = c("TRUE", "FALSE")) +
      theme_bw(base_size = 16)
  })
  
  # Visual of data to remove, temporary
  output$brush_info_remove <- renderPrint({
    psy$to_remove <- selected()
    temp <- psy %>%
      filter(to_remove == TRUE) %>%
      select(date, corrected_water_potential_m_pa) %>%
      rename(corrected_WP = corrected_water_potential_m_pa)
    return(print(temp, n = 1500))
  })
  
  # report all data
  clean_all <- reactive({
    psy$to_remove <- selected()
    return(psy)
  })
  
  # report month data
  clean_month <- reactive({
    psy$to_remove <- selected()
    return(filter(psy,
                  month == input$month))
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