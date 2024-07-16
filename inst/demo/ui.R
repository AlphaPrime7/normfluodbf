library(shiny)
library(shinyauthr)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(shinyTime)
library(shinytest)
library(ggplot2)
library(ggdark)
library(ggthemes)
library(plotly)
library(normfluodbf)
library(foreign)
local({source(file.path("ui", "helpers.R"))})
#library(magick)
#library(reactable)

user_base <- data.frame(
  user = c("user1", "adeck@brown.edu"),
  password = c("password1", "tingwei"), #purrr::map_chr(c("pass1", "pass2"), sodium::password_store) ; sapply(c("password1", "password2"), sodium::password_store)
  permissions = c("standard", "admin"),
  name = c("User One", "GODsFriend"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  shinydisconnect::disconnectMessage2(),
  shinyjs::useShinyjs(),
  tags$head(
    shinythemes::themeSelector(),
    tags$script(src = "normfluodbf.js"),
    tags$link(href = "styles.css", rel = "stylesheet")
  ),

  div(id = "loading-content", "App Loading...",
      img(src = "waiting.gif")),

  shinyauthr::loginUI(id = "login"),

  uiOutput("app_ui")
)


