#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#



page_sidebar(
  theme = bs_theme(bootswatch = "yeti"),
  title = "Psychrometer cleaning",
  sidebar = sidebar(
    width = 300, 
    "This tool is intended for QA/QC of psychrometer timeseries by visual comparison with site-level environmental data.",
    "We advise that preliminary data cleaning (e.g., removing 0 or positive values, excising day of installation) be completed programatically prior to use of this tool. ", 
    br(),
    br(),
    fileInput("upload_psy", NULL,
              buttonLabel = "Upload psychrometer timeseries as .csv",
              multiple = FALSE, accept = ".csv"),
    fileInput("upload_env", NULL,
              buttonLabel = "Upload environmental timeseries as .csv",
              multiple = FALSE, accept = ".csv"),
    
    selectInput("month", "Select month:", 
                unique(psy$month))
  ),
  layout_columns(col_widths = c(9, 3),
                 card(
                   card_header("Timeseries"),
                   uiOutput("dyn_range"),
                   plotOutput("p",
                              brush = brushOpts(id = "plot1_brush"),
                              click = clickOpts(id = "plot1_click"),
                              dblclick = dblclickOpts(id = "plot_reset"))
                 ),
                 card(
                   card_header("Points to remove"),
                   verbatimTextOutput("brush_info_remove"),
                   downloadButton("download_clean_month", "Month"),
                   downloadButton("download_clean_all", "All")
                 ))
  
)
