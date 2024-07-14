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
  Sys.sleep(3)
  observe({
    removeUI(selector = "#loading-content")
    shinyjs::show("main_nav")
  })
})


