#QC for shiny apps
install.packages("shinytest")
install.packages("testApp")
install.packages('testthat')
library(shinytest)
library(testApp)
library(testthat)
recordTest('C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/Liposome_Flux_Assays_R/Shiny_App/Src/Demo/simple_app.R')
#the above test failed probably cause the app is a little more complex

#mytest.R file using in debugging
app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")
app$setInputs(name = "Adeck")
app$setInputs(greet = "click")
app$snapshot()
app$snapshot(list(output = "greeting"))

#Once test recorder is exited then the test script is ran automatically, the code below is behind the scenes
testApp("myshinyapp", "mytest")

#subsequent test to check current vs previous states
testApp("path/to/app") 

#The tutorial ends here as the current state of my project is far away from the QC stages yet





