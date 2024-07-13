library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(normfluodbf, lib.loc = Sys.getenv('lib.loc'))
source(file.path("ui", "helpers.R"))

tagList(
  shinydisconnect::disconnectMessage2(), #shiny disconnect message
  useShinyjs(), #use shinyjs
  tags$head(
    shinythemes::themeSelector(), #united is my best-ubuntu vibe
    tags$script(src = "normfluodbf.js"),
    tags$link(href = "styles.css", rel = "stylesheet")

  ),
  div(id = "loading-content", "App loading...might take a few seconds",
      img(src = "waiting.gif")),

  navbarPage(
    title = tags$b('Normfluodbf'),
    windowTitle = "normfluodbf",
    id = "main_nav",
    inverse = FALSE,
    fluid = TRUE,
    collapsible = FALSE,
    source(file.path("ui", "tab-dataset.R"),  local = TRUE)$value
  )
)

