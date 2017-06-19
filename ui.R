library(shiny)
library(shinythemes)
library(colourpicker)
library(dplyr)
library(DT)


# Define UI for application that draws a histogram
shinyUI(navbarPage("Cezanne", id="nav", theme = shinytheme("slate"),
tabPanel("Single Element Map",
titlePanel("Single Element Plot"),
sidebarLayout(
sidebarPanel(
actionButton("actionprocess", label = "Process Data"),
tags$hr(),


fileInput('file1', 'Choose file to upload',
accept = c('.csv'), multiple=TRUE
),

tags$hr(),

textInput("project", label="Project Name", value="My Project"),

tags$hr(),

uiOutput('inElements'),
uiOutput('inLines'),

tags$hr(),

selectInput(
"colorramp", "Color Ramp",
c("Terrain" = "terrain.colors(",
"Rainbow" = "rainbow(",
"Heat" = "heat.colors(",
"Topo" = "topo.colors(",
"CM" = "cm.colors("),

selected="Terrain"),

sliderInput("colorrampvalues", label = "Steps", value=15, min=2, max=30),

tags$hr(),

sliderInput("resolution", label = "Interpolation Resolution", value=400, min=10, max=1000),

tags$hr(),

downloadButton(outputId="downloadmap", label="Download")

),

mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("simpleMap", height = 800,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))))
)),

tabPanel("Multiple Element Map",
titlePanel("Multiple Element Plot"),

sidebarLayout(
sidebarPanel(

uiOutput('inElement1'),
uiOutput('inLine1'),
colourInput('elementcolor1', label="Color", value="red"),

tags$hr(),


uiOutput('inElement2'),
uiOutput('inLine2'),
colourInput('elementcolor2', label="Color", value="green"),

tags$hr(),


uiOutput('inElement3'),
uiOutput('inLine3'),
colourInput('elementcolor3', label="Color", value="blue"),


tags$hr(),

sliderInput("resolutionmulti", label = "Interpolation Resolution", value=400, min=10, max=1000),


tags$hr(),

downloadButton(outputId="downloadmultimap", label="Download")


),

mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("multiMap", height = 800,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))


)))


)


))

)


