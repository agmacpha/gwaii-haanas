library(shiny)
library(shinythemes)
library(magick)
library(plotly)
library(tidyverse)

#List of species on Haida Gwaii
hgspp <- read_csv("/users/alexandremacphail/GH/GH/hgspp/avibasehg2.csv")
# Yes means confirmed breeder, migrant or resident
# Rare / Accidental
# Yes_d was changed from Rare / Accdidental -> Yes for TOWA, CHSP, WETA
# Rare / Accidental_m is for NOFU
# COSEWIC and BC Provincial statuses are included

ui <- fluidPage(
    theme = shinytheme("united"),
    titlePanel("List of avifauna on Haida Gwaii"),
    helpText(
        "This app allows you to dynamically explore species on Haida Gwaii. Use the search bar to specify a particular species."
    ),
    fluidRow(
        column(12,dataTableOutput('table'))))

server <-  function(input, output) {
    output$table <- renderDataTable(hgspp)
}

shinyApp(ui = ui, server = server)
