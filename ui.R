library(shiny)
library(shinythemes)
library(colourpicker)
library(dplyr)
library(DT)


# Define UI for application that draws a histogram
shinyUI(navbarPage("Cezanne", id="nav", theme = shinytheme("sandstone"),
tabPanel("Single Element Map",
titlePanel("Single Element Plot"),
sidebarLayout(
sidebarPanel(
actionButton("actionprocess1", label = "Plot"),
tags$hr(),

fileInput('file1', 'Choose file to upload',
accept = c('.csv'), multiple=TRUE
),

radioButtons('filetype', "File Type", choices=c("Net", "Sheet"), selected="Net"),

tags$hr(),

fileInput('calfileinput', 'Load Cal File', accept='.quant', multiple=FALSE),
checkboxInput('usecalfile', "Use Cal File"),
radioButtons('rhscale', "Scale", choices=c("Rh.L1", "Rh.K12"), selected="Rh.L1"),
numericInput('scalefactor', "Scale", min=0, max=10, value=6),


tags$hr(),

textInput("project", label="Project Name", value="My Project"),

tags$hr(),

uiOutput('inElements'),
uiOutput('inLines'),

tags$hr(),

selectInput(
"colorramp", "Color Ramp",
c("Terrain" = "terrain.colors(", "Rainbow" = "rainbow(", "Heat" = "heat.colors(", "Topo" = "topo.colors(", "CM" = "cm.colors("),

selected="Terrain"),

sliderInput("colorrampvalues", label = "Steps", value=15, min=2, max=30),

tags$hr(),

sliderInput('threshhold', label="Threshold", value=0.1, min=0, max=1),

tags$hr(),

checkboxInput("useinterp", label="Interpolation", value=TRUE),
sliderInput("resolution", label = "Interpolation Resolution", value=400, min=10, max=10000),
downloadButton(outputId="downloadnorm", label="normTable"),


tags$hr(),
downloadButton(outputId="downloadtable", label="Table"),
downloadButton(outputId="downloadmap", label="Tiff"),
downloadButton(outputId="downloadmapjpg", label="JPG")
),

mainPanel(
fluidRow(
div(
style = "position:relative",
plotOutput("simpleMap", height = 800,
dblclick = "plot_1_dblclick",
brush = brushOpts( id = "plot_1_brush", resetOnNew = TRUE),
hover = hoverOpts("plot_hoversimp", delay = 100, delayType = "debounce")),
uiOutput("hover_infosimp")))
))
),

tabPanel("Three Element Map",
titlePanel("Three Element Plot"),

sidebarLayout(
sidebarPanel(

actionButton("actionprocess3", label = "Plot"),

uiOutput('in3Element1'),
uiOutput('in3Line1'),
colourInput('element3color1', label="Color", value="#ACE8BB"),
sliderInput('thresh3hold1', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in3Element2'),
uiOutput('in3Line2'),
colourInput('element3color2', label="Color", value="#94D1E0"),
sliderInput('thresh3hold2', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in3Element3'),
uiOutput('in3Line3'),
colourInput('element3color3', label="Color", value="#703F4A"),
sliderInput('thresh3hold3', label="Threshold", value=0.1, min=0, max=1),



tags$hr(),

sliderInput("resolutionmulti", label = "Interpolation Resolution", value=400, min=10, max=1000),


tags$hr(),

downloadButton(outputId="downloadthreemap", label="Download Tiff"),
downloadButton(outputId="downloadthreemapjpg", label="Download JPG")



),

mainPanel(
fluidRow(
div(
style = "position:relative",
plotOutput("threeMap", height = 800,
dblclick = "plot_3_dblclick",
brush = brushOpts(id = "plot_3_brush", resetOnNew = TRUE),
hover = hoverOpts("plot_hover3", delay = 100, delayType = "debounce")
),
uiOutput("hover_info3"))

))


)


),

tabPanel("Five Element Map",
titlePanel("Five Element Plot"),

sidebarLayout(
sidebarPanel(
actionButton("actionprocess5", label = "Plot"),

uiOutput('in5Element1'),
uiOutput('in5Line1'),
colourInput('element5color1', label="Color", value="#ACE8BB"),
sliderInput('thresh5hold1', label="Threshold", value=0.1, min=0, max=1),

tags$hr(),


uiOutput('in5Element2'),
uiOutput('in5Line2'),
colourInput('element5color2', label="Color", value="#94D1E0"),
sliderInput('thresh5hold2', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in5Element3'),
uiOutput('in5Line3'),
colourInput('element5color3', label="Color", value="#703F4A"),
sliderInput('thresh5hold3', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in5Element4'),
uiOutput('in5Line4'),
colourInput('element5color4', label="Color", value="#E0D29C"),
sliderInput('thresh5hold4', label="Threshold", value=0.1, min=0, max=1),


tags$hr(),


uiOutput('in5Element5'),
uiOutput('in5Line5'),
colourInput('element5color5', label="Color", value="#F0C3F0"),
sliderInput('thresh5hold5', label="Threshold", value=0.1, min=0, max=1),



tags$hr(),

sliderInput("resolutionmulti", label = "Interpolation Resolution", value=400, min=10, max=1000),



tags$hr(),

downloadButton(outputId="downloadfivemap", label="Download Tiff"),
downloadButton(outputId="downloadfivemapjpg", label="Download JPG")


),

mainPanel(
fluidRow(
div(
style = "position:relative",
plotOutput("fiveMap", height = 800,
dblclick = "plot_5_dblclick",
brush = brushOpts(id = "plot_5_brush", resetOnNew = TRUE),
hover = hoverOpts("plot_hover5", delay = 100, delayType = "debounce")),
uiOutput("hover_info5"))



))


)


)


)

)


