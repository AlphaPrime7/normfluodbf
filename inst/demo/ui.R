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


