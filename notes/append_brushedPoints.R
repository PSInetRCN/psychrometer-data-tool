library(shiny)

ui <- fluidPage(
  plotOutput("plot", brush = "plot_brush", dblclick = "plot_reset")
)
server <- function(input, output, session) {
  selected <- reactiveVal(rep(FALSE, nrow(mtcars)))
  
  observeEvent(input$plot_brush, {
    brushed <- brushedPoints(mtcars, input$plot_brush, allRows = TRUE)$selected_
    selected(brushed | selected())
  })
  observeEvent(input$plot_reset, {
    selected(rep(FALSE, nrow(mtcars)))
  })
  
  output$plot <- renderPlot({
    mtcars$sel <- selected()
    ggplot(mtcars, aes(wt, mpg)) + 
      geom_point(aes(colour = sel)) +
      scale_colour_discrete(limits = c("TRUE", "FALSE"))
  }, res = 96)
}

shinyApp(ui, server)