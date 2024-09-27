library(shiny)
reactiveConsole(TRUE)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(shinyTime)
library(shinytest)
library(foreign)
library(ggplot2)
library(ggdark)
library(ggthemes)
library(plotly)
library(normfluodbf)
serverrr <- function(input, output, session) {

  #TAB 1

  observe(print(strftime(input$time1, "%T")))

  csv_input <- reactive({
    req(input$dbfordat)

    ext <- tools::file_ext(input$dbfordat$name)
    switch(ext,
           dbf = foreign::read.dbf(input$dbfordat$datapath),
           dat = read.table(input$dbfordat$datapath),
           validate("Invalid file; Please upload a .dbf file or .dat file"))})

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dbfordat, ".csv")},
    content = function(file){
      write.csv(csv_input(), file)})

  output$head <- renderTable({
    head(csv_input(), input$rows)}, options = list(pageLength = 5))


  #BACKEND-backend magic begins, using reactive
  dbf_norm <- reactive({dbf_wrangle(csv_input())})

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

  output$head_norm <- renderTable({ #places the output in a reactive component without messing the list
    if(input$data_view == 'raw' || input$dbfordat == '.dbf'){
      head(dbf_norm(), input$rows_tab2)
    } else if(input$data_view == 'raw' || input$dbfordat == '.dat'){
      head(normfluodat(input$dbfordat$datapath), input$rows_tab2)
    }
  })

  output$summary <- renderPrint({
    if(input$data_view == 'summ'){
      summary(dbf_norm())}
  })

  output$download_norm <- downloadHandler(
    filename <- function() {
      paste0("normalized_flux_data", ".csv")},
    content <- function(file_norm){
      write.csv(dbf_norm(), file_norm)})

  #TAB3-PLOTS
  #Use the renderUI option to do this nonsense

  output$y_names <- renderUI({
    selectizeInput(inputId = "samples", label = "Active Assay samples",
                   colnames(dbf_norm()), multiple = T, options = list(maxItems = 6))})

  output$x_name <- renderUI({
    selectizeInput(inputId = "uid", label = "Time or Cycle_No (X-axis)",
                   colnames(dbf_norm()[,c("Time", "unique_id")]), multiple = T, options = list(maxItems = 1))})

  output$test <- renderUI({selectInput("ts", "Test sample_Match Active samples", colnames(dbf_norm()))})

  output$negc <- renderUI({selectInput("negative_control", "Negative Control_Match Active samples", colnames(dbf_norm()))})

  output$posc <- renderUI({selectInput("positive_control", "Positive Control_Match Active samples", colnames(dbf_norm()))})


  active_ys <- reactive({req(input$samples)
    dbf_norm()[,input$samples]
  })

  active_x <- reactive({
    req(input$uid) #need an if statement if i keep blank but no need to complicate
    if(input$uid == "Time"){
      dbf_norm()["Time"]
    } else if(input$uid == "unique_id"){
      dbf_norm()["unique_id"]}
  })

  output$selected_cols <- renderTable({
    head(cbind(active_x(),active_ys()), input$rows_tab3)
  })

  #The journey to rome was not done in a day

  #get variables ready for plot-names are gotten from
  #tne inputs but plots are  not taking them well

  #prepare data for plotting based on advise from ChatGPT AI
  #Ref: https://zzzcode.ai/answer-question?p1=shiny&p2=Plot+graphs+in+shiny+R
  y_vars <- reactive({c(input$samples)})
  x_var <- reactive({c(input$uid)})

  plot_container <- reactiveValues()
  observeEvent(input$plot_button,{
    plot_container$plot <- GG_plot_triplets(dbf_norm(),x=x_var(),y_list=y_vars(),xlim=input$x_range,ylim=input$y_range)})


  #flux_plot <- reactive({GG_plot_triplets(dbf_norm(),x=x_var(),y_list=y_vars(),xlim=input$x_range,ylim=input$y_range)})

  #using the ggplot program for this
  output$single_plot <- renderPlot({
    plot_container$plot})

  output$download_plot <- downloadHandler(
    filename <- function() {
      paste0("flux_assay_plot", ".png", sep = "")},
    content <- function(file_norm){
      ggsave(file_norm, plot=plot_container$plot)})

}


#shinyApp(ui, server)
