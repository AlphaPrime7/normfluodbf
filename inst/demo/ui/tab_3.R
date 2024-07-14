tabPanel(
  title = "Plot Wells",
  id = "plot",
  icon = tags$i(class = "fa-solid fa-chart-bar"),
         sidebarPanel(
           #selectInput("fluor_label", "Select Fluorescence label", choices = NULL),
           #selectizeInput("x_label", "Select X label", choices = NULL, multiple = FALSE),
           #selectizeInput("tnp", "Choose Sample Type", choices = NULL, multiple = TRUE),
           actionButton("plot_button","Plot Wells"),
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
                       max = 40,
                       value = c(1,40),
                       step = 1),
           downloadButton("download_plot", "Download Flux Assay images")
         ),

         mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Active table", tableOutput("selected_cols")),
                       tabPanel("Main plot", plotOutput("single_plot")))))
