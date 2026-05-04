# app.R
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(dplyr)

source("api_calls.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)