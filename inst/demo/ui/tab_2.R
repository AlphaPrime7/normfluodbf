tabPanel(
  title = "Preview Data",
  id = "preview",
  icon = icon("magnifying-glass"),
         sidebarPanel(
           radioButtons("data_view", "Preview Data",
                        c("Raw" = "raw", "Normalized" = "normalized")),
           numericInput("rows_preview", "Rows to preview",
                        value = 10,
                        min = 1,
                        step = 1),
           actionButton("preview_button","preview_data"),
           tags$div(style = "display:inline-block; width: 20px;"),
           #tags$div(style = "margin-top: 20px;"),
           #tags$br(),
           #tags$br(),
           downloadButton("download_preview", "Download Data"),
           width = 3),
         mainPanel(
           tableOutput("raw"),
           tableOutput("normalized"),
           width = 3))
