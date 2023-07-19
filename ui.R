#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#



page_sidebar(
  theme = bs_theme(bootswatch = "yeti"),
  title = "Psychrometer cleaning",
  sidebar = sidebar(
    width = 300, 
    "This tool is intended for QA/QC of psychrometer timeseries by visual comparison with site-level environmental covariates. ",
    "We advise that preliminary data cleaning (e.g., removing 0 or positive values, excising day of installation) be completed programatically prior to use of this tool. ", 
    br(),
    br(),
    fileInput("upload_psy", NULL,
              buttonLabel = "Upload psychrometer",
              multiple = FALSE, accept = ".csv"),
    uiOutput("dyn_month"),
    fileInput("upload_env", NULL,
              buttonLabel = "Upload covariates",
              multiple = FALSE, accept = ".csv"),
    uiOutput("dyn_xvar"),
    uiOutput("dyn_yvar1"),
    uiOutput("dyn_yvar2")

  ),
  layout_columns(col_widths = c(9, 3),
                 card(
                   card_header("Timeseries"),
                   uiOutput("dyn_range"),
                   plotOutput("p",
                              brush = brushOpts(id = "plot1_brush"),
                              click = clickOpts(id = "plot1_click"),
                              dblclick = dblclickOpts(id = "plot_reset"),
                              width = "93%"),
                   plotOutput("env")
                 ),
                 card(
                   card_header("Points to remove"),
                   verbatimTextOutput("brush_info_remove"),
                   downloadButton("download_clean_month", "Month"),
                   downloadButton("download_clean_all", "All")
                 ))
  
)

