#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme("united"),
    headerPanel("Estimate costs for an environmental sensor data program"),
    sidebarPanel(titlePanel("Acoustic data"),
                 numericInput("variable1", "Number of days surveyed per year:", min = 0, max = 365, value = 1),
                 numericInput("variable2", "Audio minutes recorded per day:", min = 0, max = 1440, value = 10),
                 selectInput("variable3", "Sample rate:", choices = c("16000", "24000", "44100", "48000","96000","192000"), multiple = F),
                 numericInput("variable4", "Locations surveyed:", min = 0, max = 300, value = 1),
                 numericInput("variable5", "Years replicated:", min = 0, max = 10, value = 1),
                 sliderInput("variable6", "Processing method (length of recording processed; minutes):", min = 1, max = 10, value = 1),
                 numericInput("variable7", "Replicates (number of unique recordings processed per location):", min = 1, max = 8, value = 1)),
    sidebarPanel(titlePanel("Camera data"),
                 numericInput("variable8", "Number of days surveyed per year:", min = 0, max = 365, value = 1),
                 numericInput("variable9", "Time-lapse images per day:", min = 0, max = 24, value = 1),
                 numericInput("variable10", "Images per trigger:", min = 1, max = 10, value = 1),
                 sliderInput("variable14", "Number of trigger events per location:", min = 0, max = 2000, value = 100),
                 numericInput("variable11", "Locations surveyed:", min = 0, max = 500, value = 1),
                 numericInput("variable12","Years replicated:",min = 0, max = 10, value = 1),
                 selectInput("variable13", "Using auto-taggers and context taggers?:", choices = c("Yes","No"), multiple = F)),
    sidebarPanel(
        tableOutput("values"),
        tableOutput("reccount"),
        tableOutput("sampling"),
        tableOutput("subset"),
        tags$head(tags$style('#values{color: #C7461F; font-size: 20px; font-style: bold;}'))),
    sidebarPanel(
        tableOutput("camvalues"),
        tableOutput("camcount"),
        tags$head(tags$style('#camvalues{color: #C7461F; font-size: 20px; font-style: bold;}'))),
    sidebarPanel(
        titlePanel("Estimate costs"),
        tableOutput("subcost"),
        tableOutput("fullcost"),
        tags$head(tags$style('#fullcost{color: #A19E99; font-size: 28px; font-style: bold;'))
    )
)

server <- function(input, output) {

    ### This is for ARUs
    #Data volume
    volumeValues <-
        reactive({
            round((
                input$variable1 * input$variable4 * input$variable5 * (
                    as.numeric(input$variable3) * 16 * 2 * input$variable2 * 60
                ) / 8
            ) * 0.000000001,
            2)
        })
    output$values <- renderText({
        paste0("Acoustic data accumulated: ", round(volumeValues(), 2), " GB")
    })
    #Count of audio files
    recValues <- reactive({(input$variable1 * input$variable2 * input$variable4 * input$variable5)})
    output$reccount <-
        renderText({
            paste0(recValues(), " 1-minute segments recorded")
        })
    #Audio minutes per location
    samplingValues <-
        reactive({
            (input$variable1 * input$variable2 * input$variable5)
        })
    output$sampling <-
        renderText({
            paste0("Audio hours collected per location: ",
                   round(samplingValues() / 60, 2))
        })
    #Subsampling
    subsamplingValues <-
        reactive({
            (
                as.numeric(input$variable6) * input$variable7 * input$variable4 * input$variable5
            )
        })
    output$subset <-
        renderText({
            paste0("Audio hours to process (4 days x 3 minutes at dawn): ",
                   round(subsamplingValues() / 60, 2))
        })

    ###This is for cameras###
    #Data volume in KB
    camvolumeValues <-
        reactive({
            round((input$variable8 * input$variable9 * input$variable10 * input$variable11 * input$variable12) * 400,
                  2)
        })
    output$camvalues <-
        renderText({
            paste0("Image data accumulated: ", round(camvolumeValues()/10000, 2), " GB")
        })
    camimagecount <-
        reactive({input$variable8 * input$variable9 * input$variable10 * input$variable14 * input$variable11 * input$variable12})
    output$camcount <-
        renderText({
            paste0("Images collected: ", camimagecount(), " images")
        })
    camtriggers <-
        reactive({input$variable14 / input$variable11})
    output$triggers <- renderText({paste0("Number of trigger events per location:",camtriggers())})

    #Project cost
    subcostValues <- reactive({scales::dollar(
        round(((input$variable1 * input$variable4 * input$variable5 * (as.numeric(input$variable3) * 16 * 2 * input$variable2 * 60) / 8) * 0.000000000001 * 125) +
                  ((as.numeric(input$variable6) * input$variable7 * input$variable4 * input$variable5) / 60 * 132.50), 2) +
            (((input$variable8 * input$variable9 * input$variable10 * input$variable14 * input$variable11 * input$variable12) * 400) / 1000000) * 1.2)})

    output$subcost <- renderText({paste0("Subtotal: ", subcostValues())})

    fullcostValues <- reactive({scales::dollar(
        (round(((input$variable1 * input$variable4 * input$variable5 * (as.numeric(input$variable3) * 16 * 2 * input$variable2 * 60) / 8) * 0.000000000001 * 125) +
                   ((as.numeric(input$variable6) * input$variable7 * input$variable4 * input$variable5) / 60 * 132.50), 2) +
             ((((input$variable8 * input$variable9 * input$variable10 * input$variable14 * input$variable11 * input$variable12) * 400) / 1000000) * 1.2)) * 1.2)})

    output$fullcost <- renderText({paste0("Total: ", fullcostValues())})
}

shinyApp(ui, server)
