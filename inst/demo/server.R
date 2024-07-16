library(shiny)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(zoo)
library(ggplot2)
library(ggthemes)
library(ggdark)
library(profvis)


shinyServer(function(input, output, session) {

  Sys.sleep(1)
  observe({
    removeUI(selector = "#loading-content")
    shinyjs::show("main_nav")
  })

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
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
      navbarPage(
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
      ),
      actionButton("logout", "Log out")
    )
  })

  observeEvent(input$logout, {
    logout_init(TRUE)
    shinyjs::show(id = "login")
  })


})


