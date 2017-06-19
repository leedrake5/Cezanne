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

sliderInput('threshhold', label="Threshold", value=0.1, min=0, max=1),


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

tabPanel("Three Element Map",
titlePanel("Three Element Plot"),

sidebarLayout(
sidebarPanel(

uiOutput('in3Element1'),
uiOutput('in3Line1'),
colourInput('element3color1', label="Color", value="red"),
sliderInput('thresh3hold1', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in3Element2'),
uiOutput('in3Line2'),
colourInput('element3color2', label="Color", value="green"),
sliderInput('thresh3hold2', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in3Element3'),
uiOutput('in3Line3'),
colourInput('element3color3', label="Color", value="blue"),
sliderInput('thresh3hold3', label="Threshold", value=0.1, min=0, max=1),



tags$hr(),

sliderInput("resolutionmulti", label = "Interpolation Resolution", value=400, min=10, max=1000),


tags$hr(),

downloadButton(outputId="downloadthreemap", label="Download")


),

mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("threeMap", height = 800,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))


)))


)


),

tabPanel("Five Element Map",
titlePanel("Five Element Plot"),

sidebarLayout(
sidebarPanel(

uiOutput('in5Element1'),
uiOutput('in5Line1'),
colourInput('element5color1', label="Color", value="red"),
sliderInput('thresh5hold1', label="Threshold", value=0.1, min=0, max=1),

tags$hr(),


uiOutput('in5Element2'),
uiOutput('in5Line2'),
colourInput('element5color2', label="Color", value="yellow"),
sliderInput('thresh5hold2', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in5Element3'),
uiOutput('in5Line3'),
colourInput('element5color3', label="Color", value="green"),
sliderInput('thresh5hold3', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in5Element4'),
uiOutput('in5Line4'),
colourInput('element5color4', label="Color", value="blue"),
sliderInput('thresh5hold4', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in5Element5'),
uiOutput('in5Line5'),
colourInput('element5color5', label="Color", value="violet"),
sliderInput('thresh5hold5', label="Threshold", value=0.1, min=0, max=1),



tags$hr(),

sliderInput("resolutionmulti", label = "Interpolation Resolution", value=400, min=10, max=1000),



tags$hr(),

downloadButton(outputId="downloadfivemap", label="Download")


),

mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("fiveMap", height = 800,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))


)))


)


)


)

)


