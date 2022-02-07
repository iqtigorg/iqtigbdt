library(shiny)
library(dplyr)
library(ggplot2)
library(iqtigbdt)

source("server_include.R", chdir = TRUE, encoding = "UTF-8")
source("ui_include.R", chdir = TRUE, encoding = "UTF-8")

shiny::shinyApp(ui, server)
