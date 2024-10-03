#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#



page_sidebar(
  theme = bs_theme(bootswatch = "yeti"),
  title = "Psychrometer cleaning tool",
  sidebar = sidebar(
    width = 300, 
    "This tool is intended for QA/QC of psychrometer timeseries by visual comparison with site-level environmental covariates. ",
    "We advise that preliminary data cleaning (e.g., removing 0 or positive values, excising day of installation) be completed programatically prior to use of this tool. ",
    br(),
    "Please upload .csv files of hourly or sub-hourly psychrometer and environmental covariate timeseries. One column should be named `dt` and include the date/time in standard POSIX format.",
    "We've found that including soil moisture and VPD are most helpful in determining whether psychrometer outliers should be excluded from downstream calculations.",
    br(),
    br(),
    fileInput("upload_psy", NULL,
              buttonLabel = "Upload psychrometer",
              multiple = FALSE, accept = ".csv"),
    uiOutput("dyn_month"),
    uiOutput("dyn_psyvar"),
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
                   plotOutput("psy_plot",
                              brush = brushOpts(id = "plot1_brush"),
                              click = clickOpts(id = "plot1_click"),
                              dblclick = dblclickOpts(id = "plot_reset"),
                              width = "95%"),
                   plotOutput("env_plot")
                 ),
                 card(
                   card_header("Points to remove"),
                   p("Select points by drawing a rectangle or clicking on individual points. Double-click to undo. "),
                   verbatimTextOutput("brush_info_remove"),
                   downloadButton("download_clean_month", "Month"),
                   downloadButton("download_clean_all", "All")
                 ))
  
)

