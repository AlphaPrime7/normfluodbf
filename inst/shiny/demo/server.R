library(shiny)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(zoo)
library(ggplot2)
library(ggthemes)
library(ggdark)
library(profvis)
local({source(file.path("ui", "helpers.R"))})

shinyServer(function(input, output, session) {

  Sys.sleep(1)
  observe({
    removeUI(selector = "#loading-content")
    shinyjs::show("main_nav")
  })

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = db(),
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  logout_init <- reactiveVal(FALSE)

  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      shinyjs::hide(id = "login")
    } else {
      shinyjs::show(id = "login")
    }
  })

  output$app_ui <- renderUI({
    req(credentials()$user_auth)
    tagList(

      tags$div(style = "width: 95%", navbarPage(
        title = tags$b("Normfluodbf Demo"),
        windowTitle = "Normfluodbf",
        id = "main_nav",
        inverse = FALSE,
        fluid = TRUE,
        collapsible = FALSE,
        source(file.path("ui", "tab_1.R"),  local = TRUE)$value,
        source(file.path("ui", "tab_2.R"),  local = TRUE)$value,
        source(file.path("ui", "tab_3.R"),  local = TRUE)$value,
        source(file.path("ui", "tab_about.R"),  local = TRUE)$value
      )),
      tags$button(id="logout",
                  type="button",
                  icon("person"),
                  style="color: #fff000;
                  background-color: #337ab7;
                  border-color: #2e6da4;
                  position: absolute;
                  top: 0px;
                  right: 0px",
                  class="btn action-button btn-large btn-primary",
                  HTML('<i></i>Logout'))
      #actionButton("logout", "Log out")
    )
  })

  # Source server files here
  source(file.path("server", "tab_1.R"), local = TRUE)
  source(file.path("server", "tab_2.R"), local = TRUE)
  #source(file.path("server", "tab_3.R"), local = TRUE)

  observeEvent(input$logout, {
    logout_init(TRUE)
    shinyjs::show(id = "login")
  })


})


