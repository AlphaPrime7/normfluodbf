library(shiny)
library(shinyjs)
library(shinycssloaders)
reactiveConsole(TRUE)
library(tidyverse, lib.loc = Sys.getenv('lib.loc'))
library(zoo)
library(ggplot2)
library(ggthemes)
library(ggdark)
library(profvis)


shinyServer(function(input, output, session) {

  Sys.sleep(3)

  observe({
    removeUI(selector = "#loading-content")
    shinyjs::show("main_nav")
  })


})


