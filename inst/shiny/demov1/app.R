library(shiny)
library(normfluodbf)
reactiveConsole(TRUE)
library(shinythemes)
library(shinydashboard)
library(bslib)
library(shinyTime)
library(foreign)
library(shinytest)
library(ggplot2)
library(ggdark)
library(ggthemes)
library(plotly)
#library(magick)
#library(reactable)
source("PROCS.R")

ui <- fluidPage(
  shinythemes::themeSelector(),
  #theme = shinytheme("cerulean"),
  #theme = "mytheme.css",


  tabsetPanel(
    id = "tabset",
    #TAB1
    tabPanel("Import .dbf or .dat file",
             sidebarPanel(
               timeInput("time1", "Time:",
                         value = Sys.time()),
               fileInput("dbfordat", NULL,
                         accept = c(".dbf", ".dat"),
                         buttonLabel = "Upload...",
                         multiple = FALSE),
               numericInput("rows",
                            "Rows to preview",
                            value = 5,
                            min = 1,
                            step = 1),
               textInput("delim",
                         "Delimiter (leave blank to guess)",
                         ""),
               downloadButton("download",
                              "Download .csv"),
               width = 3),

             mainPanel(
               tableOutput("head"), width = 3)
    ),

    #TAB2
    tabPanel("Normalized Flux Assay Data",
             sidebarPanel(
               radioButtons("data_view",
                            "Raw or Summary",
                            c("None" = "none",
                              "Raw"="raw",
                              "Normalized"="norm",
                              "Summary"="summ")),
               numericInput("rows_tab2",
                            "Rows to preview",
                            value = 10,
                            min = 1,
                            step = 1),
               downloadButton("download_norm",
                              "Download Normalized .csv"),
               width = 3),

             mainPanel(
               textOutput("text"),
               tableOutput("head_norm"),
               verbatimTextOutput("summary"), width = 3)),

    #TAB3
    tabPanel("Demo Visualizations",
             sidebarPanel(
               uiOutput("y_names"),
               uiOutput("x_name"),
               actionButton("plot_button","Plot Results"),
               numericInput("rows_tab3",
                            "Rows to preview",
                            value = 30,
                            min = 1,
                            step = 1),
               sliderInput(inputId = "y_range",
                           label = "Y-axis",
                           min = 0.00,
                           max = 1,
                           value = c(0,1),
                           step = 0.05),
               sliderInput(inputId = "x_range",
                           label = "X-axis",
                           min = 1.00,
                           max = 3000,
                           value = c(1,40),
                           step = 1),
               downloadButton("download_plot", "Download Flux Assay images")
             ),

             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Active table", tableOutput("selected_cols")),
                           tabPanel("Main plot", plotOutput("single_plot")),
                           tabPanel("Plate View", plotlyOutput("multiplot_panel"))
               ),

             )
    )
  )
)

server <- function(input, output, session) {

  #TAB 1

  observe(print(strftime(input$time1, "%T")))

  csv_input <- reactive({
    req(input$dbfordat)

    ext <- tools::file_ext(input$dbfordat$name)
    switch(ext,
           dbf = normfluordbf(input$dbfordat$datapath, norm_scale = 'raw'),
           dat = normfluodat(input$dbfordat$datapath, norm_scale = 'raw'),
           validate("Invalid file; Please upload a .dbf file or .dat file"))})

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dbfordat, ".csv")},
    content = function(file){
      write.csv(csv_input(), file)})

  output$head <- renderTable({
    if(NA %in% input$rows) {
      head(c("Enter > 1 rows to preview the information"))
    }
    else {
      head(csv_input(), input$rows)
    }
    },
    options = list(pageLength = 5))


  #BACKEND-backend magic begins, using reactive
    dbf_raw <- reactive({csv_input()})
    dbf_norm <- reactive({normfluordbf(input$dbfordat$datapath)})
    dat_raw = reactive({csv_input()})
    dat_norm = reactive({
      normfluodat(input$dbfordat$datapath)
      })


  #TAB 2

  #dat_view <- reactive({input$data_view}) #this approach fails as the reactive element here
  #is a function

  output$text <- renderPrint({
    if(input$data_view == 'none'){
      suppressWarnings(print("Hello Fellow Data Nerd and Fun Wanabee,
    \n let the program get some rest as there
    is nothing to show here and you
    have no clue what a dbf file is"))
    }
  })

  output$head_norm <- renderTable({
    req(input$dbfordat)
    ext = tools::file_ext(input$dbfordat$datapath)

    if(input$data_view == 'raw' && ext == 'dbf') {
      if(NA %in% input$rows_tab2) {
        head(c("Enter >= 1 rows to preview the information"))
      }
      else{
        head(dbf_raw(), input$rows_tab2)
      }
    }
    else if(input$data_view == 'norm' && ext == 'dbf') {
      if(NA %in% input$rows_tab2) {
        head(c("Enter >= 1 rows to preview the information"))
        #head(dbf_norm(), 5)
      }
      else{
        head(dbf_norm(), input$rows_tab2)
      }
    }
    else if(input$data_view == 'raw' && ext == 'dat'){
      if(NA %in% input$rows_tab2) {
        head(c("Enter >= 1 rows to preview the information"))
        #head(dbf_norm(), 5)
      }
      else{
        head(dat_raw(), input$rows_tab2)
      }
    }
    else if(input$data_view == 'norm' && ext == 'dat'){
      if(NA %in% input$rows_tab2) {
        head(c("Enter >= 1 rows to preview the information"))
        #head(dbf_norm(), 5)
      }
      else{
        head(dat_norm(), input$rows_tab2)
      }
    }
    else {
      #do nothing
    }
  })

  output$summary <- renderPrint({
    req(input$dbfordat)
    ext = tools::file_ext(input$dbfordat$datapath)
    if(input$data_view == 'summ' && ext == 'dbf'){
        summary(dbf_norm())
        }
      else if(input$data_view == 'summ' && ext == 'dat'){
        summary(dat_norm())
      }
  })

  output$download_norm <- downloadHandler(
    filename <- function() {
      paste0("normalized_flux_data", ".csv")},
    content <- function(file_norm){
      write.csv(dbf_norm(), file_norm)})

  #TAB3-PLOTS
  #Use the renderUI option to do this nonsense

  output$y_names <- renderUI({
    req(input$dbfordat)
    ext = tools::file_ext(input$dbfordat$datapath)
    if(is.null(input$dbfordat$name)) {
      #print("Load DBF or DAT")
    }
    else if (ext == "dbf"){
      selectizeInput(inputId = "wells",
                     label = "Active Assay samples (3 Max for Demo)",
                     choices = colnames(dbf_norm()),
                     multiple = T,
                     options = list(maxItems = 3))
    }
    else {
      selectizeInput(inputId = "wells",
                     label = "Active Assay samples (3 Max for Demo)",
                     colnames(dat_norm()),
                     multiple = T,
                     options = list(maxItems = 6))
    }
    })

  output$x_name <- renderUI({
    req(input$dbfordat)
    ext = tools::file_ext(input$dbfordat$datapath)
    if(is.null(input$dbfordat$name)) {
      #print("Load DBF or DAT")
    }
    else if (ext == "dbf"){
      selectizeInput(inputId = "x",
                     label = "Time or Cycle_No (X-axis)",
                     choices = colnames(dbf_norm()[,c("Time", "Cycle_Number")]),
                     selected = colnames(dbf_norm())[which(colnames(dbf_norm()) == "Cycle_Number")],
                     multiple = T,
                     options = list(maxItems = 1))
    }
    else {
      selectizeInput(inputId = "x",
                     label = "Cycle_No (X-axis)",
                     choices = colnames(dat_norm())[which(colnames(dat_norm()) == "Cycle_Number")],
                     selected = colnames(dat_norm())[which(colnames(dat_norm()) == "Cycle_Number")],
                     multiple = F,
                     options = list(maxItems = 1))
    }

    })

  active_ys <- reactive({
    req(input$dbfordat)
    ext = tools::file_ext(input$dbfordat$datapath)
    if (ext == "dbf") {
      req(input$wells)
      dbf_norm()[,input$wells]
    }
    else {
      req(input$wells)
      dat_norm()[,input$wells]
    }

  })

  active_x <- reactive({
    req(input$dbfordat)
    ext = tools::file_ext(input$dbfordat$datapath)
    if (ext == "dbf") {
      req(input$x)
      if(input$x == "Time"){
        dbf_norm()["Time"]
      } else if(input$x == "Cycle_Number"){
        dbf_norm()["Cycle_Number"]}
    }
    else {
      req(input$x)
      dat_norm()["Cycle_Number"]
    }
  })

  output$selected_cols <- renderTable({
    if(NA %in% input$rows_tab3) {
      head(c("Enter >= 1 rows to preview the information"))
    }
    else {
      head(cbind(active_x(),
                 active_ys()),
           input$rows_tab3)
    }
  })

  active_table = reactive({
    cbind(active_x(),
          active_ys())
    })

  y_vars <- reactive({c(input$wells)})
  x_var <- reactive({c(input$x)})

  plot_container <- reactiveValues()
  observeEvent(input$plot_button,{
    req(input$dbfordat)
    ext = tools::file_ext(input$dbfordat$datapath)
    plot_container$plot = NULL
    if(length(y_vars()) !=3) {
      plot_container$plot = ggplot() +
        labs(title = "Error: Please provide exactly 3 samples.",
             x = "X Variable",
             y = "") +
        theme_void()
    } else {
      plot_container$plot = ggplot.normfluodbf.demo(active_table(),
                                              x=x_var(),
                                              y_list=y_vars(),
                                              xlim=input$x_range,
                                              ylim=input$y_range)
    }
    })

  #using the ggplot program for this
  output$single_plot <- renderPlot({
    plot_container$plot})

  output$multiplot_panel <- renderText({
    print("No Multiplots for Demo Version")})

  output$download_plot <- downloadHandler(
    filename <- function() {
      paste0("flux_assay_plot", ".png", sep = "")},
    content <- function(file_norm){
      ggsave(file_norm, plot=plot_container$plot)})

}



shinyApp(ui, server)
