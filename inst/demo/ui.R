library(shiny)
library(shinythemes)
library(bslib)
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
local({source(file.path("ui", "helpers.R"))})
#library(magick)
#library(reactable)

tagList(
  shinydisconnect::disconnectMessage2(),
  shinyjs::useShinyjs(),
  tags$head(
    shinythemes::themeSelector(),
    tags$script(src = "normfluodbf.js"),
    tags$link(href = "styles.css", rel = "stylesheet")
  ),
  div(id = "loading-content", "App Loading...",
      img(src = "waiting.gif")),

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
  )
)
