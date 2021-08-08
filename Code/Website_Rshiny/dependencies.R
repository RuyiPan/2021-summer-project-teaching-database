# LIST OF REQUIRED PACKAGES -----------------------------------------------

# required_packages <- c(
#   "checkpoint"
# )

# install missing packages
# 
# new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# 
# if (length(new.packages)) {
#   install.packages(new.packages)
# }
# 
# rm(new.packages)
library(ggridges)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinycustomloader)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

library(reshape2)
library(countrycode)
library(tidyverse)

library(viridis) 
library(PNWColors)
library(readxl)
library(DT)

# loading the package
library(shinydashboardPlus)
library(shinythemes)
library(ggthemes)
library(hrbrthemes)

require(highcharter)
library(plotly)

library(ctmm)
library(data.table)

library(webshot)
library("htmlwidgets")
library(wordcloud2)

library(wordcloud2)


source('global.R')

###############################

# Create a custom theme for the plots. 
custom_theme <- hc_theme(
  colors = c('#5CACEE', 'green', 'red'),
  chart = list(
    backgroundColor = '#FAFAFA', 
    plotBorderColor = "black"),
  xAxis = list(
    gridLineColor = "C9C9C9", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#C9C9C9", 
    minorGridLineColor = "#C9C9C9", 
    tickColor = "#C9C9C9", 
    title = list(style = list(color = "#333333"))), 
  yAxis = list(
    gridLineColor = "#C9C9C9", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#C9C9C9", 
    minorGridLineColor = "#C9C9C9", 
    tickColor = "#C9C9C9", 
    tickWidth = 1, 
    title = list(style = list(color = "#333333"))),   
  title = list(style = list(color = '#333333', fontFamily = "Lato")),
  subtitle = list(style = list(color = '#666666', fontFamily = "Lato")),
  legend = list(
    itemStyle = list(color = "#333333"), 
    itemHoverStyle = list(color = "#FFF"), 
    itemHiddenStyle = list(color = "#606063")), 
  credits = list(style = list(color = "#666")),
  itemHoverStyle = list(color = 'gray'))


onInitialize <- "
function(){
  var select = this.$input[0];
  $('#reset').on('click', function(){
    select.selectize.setValue([]);
  });
}
"


# Function to call in place of dropdownMenu --------------
customSentence <- function(numItems, type) {
  paste("Feedback & suggestions")
}

customSentence_share <- function(numItems, type) {
  paste("Helpful? Share it!")
}

dropdownMenuCustom <-  function (..., type = c("messages", "notifications", "tasks"), 
                                 badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        div(customSentence(numItems, type),style = 'color: white'),
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}

