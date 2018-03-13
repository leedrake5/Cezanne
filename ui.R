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
accept = c('.csv', '.xls', '.xlsx', '.dat'), multiple=TRUE
),

selectInput('filetype', "File Type", choices=c("Excel", ".dat", "Combined", "Net"), selected="Excel"),
numericInput('adjust', "Adjust",  value=0),

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
c("Black and White", "White and Black", "Terrain", "Rainbow", "Heat", "Topo", "CM"),

selected="Black and White"),

sliderInput("colorrampvalues", label = "Steps", value=15, min=2, max=30),

tags$hr(),

sliderInput('threshhold', label="Threshold", value=0, min=0, max=1),

tags$hr(),

checkboxInput('default1', label="Default to Native Resolution", value=FALSE),
uiOutput('inresolution1'),
downloadButton(outputId="downloadnorm", label="normTable"),
checkboxInput('rotate', label="Rotate", value=FALSE),
checkboxInput('flip', label="Flip", value=FALSE),




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




tabPanel("PCA",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(
numericInput("knum", label = "K-Means", value=3),

uiOutput('defaultlines'),


uiOutput('pcaFocusVariable'),
uiOutput('pcaFocusUI'),
uiOutput('pcaFocusLabel'),


sliderInput("spotsize", label = "Point Size", value=2, min=2, max=15),

checkboxInput('elipseplot1', "Elipse"),
checkboxInput('logtrans', "Log Transform"),


uiOutput('inxlimrangepca'),
uiOutput('inylimrangepca'),


tags$hr(),


downloadButton('downloadPlot2', "Plot"),
downloadButton('xrfpcatablefulldownload', "Results")

),



mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('PCA Plot',

# this is an extra div used ONLY to create positioned ancestor for tooltip
# we don't change its position
div(
style = "position:relative",
plotOutput("xrfpcaplot", height = 650,
hover = hoverOpts("plot_hoverpca", delay = 100, delayType = "debounce")),
uiOutput("hover_infopca")
)


),
tabPanel("Table", DT::dataTableOutput('xrfpcatable'))


))

))

)),

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
checkboxInput('default3', label="Default to Native Resolution", value=FALSE),
uiOutput('inresolution3'),
checkboxInput('scale3', label="Scale", value=FALSE),

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
checkboxInput('default5', label="Default to Native Resolution", value=FALSE),
uiOutput('inresolution5'),

checkboxInput('scale5', label="Scale", value=FALSE),



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


),


tabPanel("Elemental Ratios",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(




uiOutput('inelementratioa'),
uiOutput('inlineratioa'),

uiOutput('inelementratiob'),
uiOutput('inlineratiob'),

uiOutput('inelementratioc'),
uiOutput('inlineratioc'),

uiOutput('inelementratiod'),
uiOutput('inlineratiod'),

tags$hr(),

sliderInput("spotsize2", label = "Point Size", value=2, min=2, max=15),





checkboxInput('elipseplot2', "Elipse"),



tags$hr(),


downloadButton('downloadPlot4', "Plot")



),

mainPanel(
tabPanel('Element Ratios',
div(
style = "position:relative",
plotOutput("elementratiotimeseries", height = 650,
hover = hoverOpts("plot_hoverratio", delay = 100, delayType = "debounce")),
uiOutput("hover_inforatio")
)

)

))

))


)


)

)


