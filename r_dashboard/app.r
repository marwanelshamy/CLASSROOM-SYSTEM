# app.R
library(shiny)
library(shinydashboard)
library(shinyjs)
library(base64enc)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(dplyr)

source("api_calls.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server, options = list(port = 6863, host = "localhost", launch.browser = TRUE))