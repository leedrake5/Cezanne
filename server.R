library(shiny)
library(colourpicker)
library(ggplot2)
library(reshape2)
library(pbapply)
library(TTR)
library(DT)
library(pvclust)
library(data.table)
library(pbapply)
library(R.utils)
library(R.oo)
library(Biobase)
library(plyr)
library(dplyr)

library(ggplot2)
library(reshape2)
library(pbapply)
library(akima)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    observeEvent(input$actionprocess, {
        
        myData <- reactive({
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$file1
                if (is.null(inFile)) return(NULL)
                
                #inName <- inFile$name
                #inPath <- inFile$datapath
                
                #inList <- list(inName, inPath)
                #names(inList) <- c("inName", "inPath")
               
                
                n <- length(inFile$name)
                
                my.x <- pblapply(inFile$name, function(x) read_csv_x(x))
                my.y <- pblapply(inFile$name, function(x) read_csv_y(x))


                myfiles = pblapply(inFile$datapath, function(x) read_csv_net(x))
                
                myfiles.1 <- mapply(cbind, myfiles, "x" <- my.x, SIMPLIFY=F)
                myfiles.2 <- mapply(cbind, myfiles.1, "y" <- my.y, SIMPLIFY=F)
                
                all.col.names <- c( "Element", "Line", "Net", "Background", "x", "y")
                
                myfiles.list <- pblapply(myfiles.2, setNames, all.col.names)


                
                
                myfiles.frame <- do.call(rbind, lapply(myfiles.list, data.frame, stringsAsFactors=FALSE))
                

                
                
                
                
                data <- myfiles.frame
                
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            data
            
        })
        
        
      

    


#data.m <- metadataTableRe()

#if (is.null(data.m)){ data.m <- ceramics}

output$testtable <- renderDataTable({
    test <- datatable(myData())
    test
    
})


outElements <- reactive({
    metadata.dat <- myData()
    
    element.names <- unique(metadata.dat$Element)
    
    element.names
    
    
})

outLines <- reactive({
    metadata.dat <- myData()
    
    line.names <- unique(metadata.dat$Line)
    
    line.names
    
    
})



####Single Plot
output$inElements <- renderUI({
    selectInput(inputId = "elements", label = h4("Element"), choices =  outElements())
})

output$inLines <- renderUI({
    selectInput(inputId = "lines", label = h4("Fluorescence Line"), choices =  outLines())
})


####Three Element Plot
output$in3Element1 <- renderUI({
    selectInput(inputId = "threeelement1", label = h4("Element"), choices =  outElements(), selected="Fe")
})

output$in3Line1 <- renderUI({
    selectInput(inputId = "threeline1", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in3Element2 <- renderUI({
    selectInput(inputId = "threeelement2", label = h4("Element"), choices =  outElements(), selected="Co")
})

output$in3Line2 <- renderUI({
    selectInput(inputId = "threeline2", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in3Element3 <- renderUI({
    selectInput(inputId = "threeelement3", label = h4("Element"), choices =  outElements(), selected="Pb")
})

output$in3Line3 <- renderUI({
    selectInput(inputId = "threeline3", label = h4("Fluorescence Line"), choices =  outLines(), selected="L1")
})



####Five Element Plot
output$in5Element1 <- renderUI({
    selectInput(inputId = "fiveelement1", label = h4("Element"), choices =  outElements(), selected="Fe")
})

output$in5Line1 <- renderUI({
    selectInput(inputId = "fiveline1", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in5Element2 <- renderUI({
    selectInput(inputId = "fiveelement2", label = h4("Element"), choices =  outElements(), selected="Zn")
})

output$in5Line2 <- renderUI({
    selectInput(inputId = "fiveline2", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in5Element3 <- renderUI({
    selectInput(inputId = "fiveelement3", label = h4("Element"), choices =  outElements(), selected="Pb")
})

output$in5Line3 <- renderUI({
    selectInput(inputId = "fiveline3", label = h4("Fluorescence Line"), choices =  outLines(), selected="L1")
})

output$in5Element4 <- renderUI({
    selectInput(inputId = "fiveelement4", label = h4("Element"), choices =  outElements(), selected="Co")
})

output$in5Line4 <- renderUI({
    selectInput(inputId = "fiveline4", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in5Element5 <- renderUI({
    selectInput(inputId = "fiveelement5", label = h4("Element"), choices =  outElements(), selected="Hg")
})

output$in5Line5 <- renderUI({
    selectInput(inputId = "fiveline5", label = h4("Fluorescence Line"), choices =  outLines(), selected="L1")
})





plotInputSingle <- reactive({
    
    colvals = as.character(paste(input$colorramp, input$colorrampvalues, ")", sep="", collapse=""))
    
    fishImport <- myData()
    
    fishSubset <- fishImport %>% filter(Line==input$lines & Element==input$elements)
    
    
    
    
    ###Europe
    xmin <- min(fishSubset$x)
    xmax <- max(fishSubset$x)
    ymin <- min(fishSubset$y)
    ymax <- max(fishSubset$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    fish.int <- with(fishSubset, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolution, ny=input$resolution*y.ratio))
    fish.int.melt <- melt(fish.int$z, na.rm=TRUE)
    colnames(fish.int.melt) <- c("x", "y", "z")
    
    fish.int.melt$x <- fish.int$x[fish.int.melt$x]
    fish.int.melt$y <- fish.int$y[fish.int.melt$y]
    
    fish.int.melt$z <- transform_0_1(fish.int.melt$z)
    
    
    fish.int.melt[is.na(fish.int.melt)] <- 0
    
    fish.int.melt <- subset(fish.int.melt, z > input$threshhold)

    
    
    spectral.int.map <- ggplot(fish.int.melt) +
    geom_tile(aes(x, y,  fill=z, alpha=z)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    coord_equal() +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    
        spectral.int.map
    
})


output$simpleMap <- renderPlot({
    print(plotInputSingle())
})


output$downloadmap <- downloadHandler(
filename = function() { paste(input$project, "_", input$elements, "_", input$lines, '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInputSingle(), width=7, height=7, dpi=300, device="tiff")
}
)


dataSplit3 <- reactive({
    
    fishImport <- myData()
    
    fishSubset1 <- fishImport %>% filter(Line==input$threeline1 & Element==input$threeelement1)
    fishSubset2 <- fishImport %>% filter(Line==input$threeline2 & Element==input$threeelement2)
    fishSubset3 <- fishImport %>% filter(Line==input$threeline3 & Element==input$threeelement3)
    
    ###Europe
    xmin <- min(fishSubset1$x)
    xmax <- max(fishSubset1$x)
    ymin <- min(fishSubset1$y)
    ymax <- max(fishSubset1$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    fish.int.1 <- with(fishSubset1, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.1 <- melt(fish.int.1$z, na.rm=TRUE)
    colnames(fish.int.melt.1) <- c("x", "y", "z")
    
    fish.int.melt.1$x <- fish.int.1$x[fish.int.melt.1$x]
    fish.int.melt.1$y <- fish.int.1$y[fish.int.melt.1$y]
    
    fish.int.melt.1$z <- transform_0_1(fish.int.melt.1$z)
    
    
    fish.int.melt.1[is.na(fish.int.melt.1)] <- 0
    
    fish.int.melt.1 <- subset(fish.int.melt.1, fish.int.melt.1$z > input$thresh3hold1)
    
    
    #   fish.int.melt.1$z <- fish.int.melt.1$z[ fish.int.melt.1$z<0.1 ] <- 0
    
    
    fish.int.2 <- with(fishSubset2, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.2 <- melt(fish.int.2$z, na.rm=TRUE)
    colnames(fish.int.melt.2) <- c("x", "y", "z")
    
    fish.int.melt.2$x <- fish.int.2$x[fish.int.melt.2$x]
    fish.int.melt.2$y <- fish.int.2$y[fish.int.melt.2$y]
    
    fish.int.melt.2$z <- transform_0_1(fish.int.melt.2$z)
    
    
    fish.int.melt.2[is.na(fish.int.melt.2)] <- 0
    
    fish.int.melt.2 <- subset(fish.int.melt.2, fish.int.melt.2$z > input$thresh3hold2)
    
    #fish.int.melt.2$z <- fish.int.melt.2$z[ fish.int.melt.2$z<0.1 ] <- 0
    
    
    fish.int.3 <- with(fishSubset3, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.3 <- melt(fish.int.3$z, na.rm=TRUE)
    colnames(fish.int.melt.3) <- c("x", "y", "z")
    
    fish.int.melt.3$x <- fish.int.3$x[fish.int.melt.3$x]
    fish.int.melt.3$y <- fish.int.3$y[fish.int.melt.3$y]
    
    fish.int.melt.3$z <- transform_0_1(fish.int.melt.3$z)
    
    
    fish.int.melt.3[is.na(fish.int.melt.3)] <- 0
    
    fish.int.melt.3 <- subset(fish.int.melt.3, fish.int.melt.3$z > input$thresh3hold3)
    
    #fish.int.melt.3$z <- fish.int.melt.3$z[ fish.int.melt.3$z<0.1 ] <- 0
    
    #fish.merge <- data.frame(fish.int.melt.1$x, fish.int.melt.1$y, fish.int.melt.1$z, fish.int.melt.2$z, fish.int.melt.3$z)
    #colnames(fish.merge) <- c("x", "y", "z1", "z2", "z3")
    
    fish.x <- c(fish.int.melt.1$x, fish.int.melt.2$x, fish.int.melt.3$x)
    fish.y <- c(fish.int.melt.1$y, fish.int.melt.2$y, fish.int.melt.3$y)
    fish.z <- c(fish.int.melt.1$z, fish.int.melt.2$z, fish.int.melt.3$z)
    fish.element <- c(rep(paste("1. ", input$threeelement1, sep="", collapse=""), length(fish.int.melt.1$z)), rep(paste("2. ", input$threeelement2, sep="", collapse=""), length(fish.int.melt.2$z)), rep(paste("3. ", input$threeelement3, sep="", collapse=""), length(fish.int.melt.3$z)))
    
    fish.merge <- data.frame(fish.x, fish.y, fish.z, fish.element)
    colnames(fish.merge) <- c("x", "y", "z", "Element")
    fish.merge

    
})




plotInputThree <- reactive({
    
    fish.merge <- dataSplit3()
    
    

    spectral.int.map <- ggplot(fish.merge, aes(x, y)) +
    geom_tile(aes(fill=Element, alpha=z)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    scale_fill_manual("Net Counts",
    breaks=c(paste("1. ", input$threeelement1, sep="", collapse=""), paste("2. ", input$threeelement2, sep="", collapse=""), paste("3. ", input$threeelement3, sep="", collapse="")),
    values=c(input$element3color1, input$element3color2, input$element3color3)) +
    coord_equal() +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    
    spectral.int.map
    
})


output$threeMap <- renderPlot({
    print(plotInputThree())
})


output$downloadmultimap <- downloadHandler(
filename = function() { paste(input$project, "_", input$threeelement1, "_", input$threeelement2, "_", input$threeelement3, '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInputThree(), width=7, height=7, dpi=300, device="tiff")
}
)





dataSplit5 <- reactive({
    
    fishImport <- myData()
    
    fishSubset1 <- fishImport %>% filter(Line==input$fiveline1 & Element==input$fiveelement1)
    fishSubset2 <- fishImport %>% filter(Line==input$fiveline2 & Element==input$fiveelement2)
    fishSubset3 <- fishImport %>% filter(Line==input$fiveline3 & Element==input$fiveelement3)
    fishSubset4 <- fishImport %>% filter(Line==input$fiveline4 & Element==input$fiveelement4)
    fishSubset5 <- fishImport %>% filter(Line==input$fiveline5 & Element==input$fiveelement5)


    ###Europe
    xmin <- min(fishSubset1$x)
    xmax <- max(fishSubset1$x)
    ymin <- min(fishSubset1$y)
    ymax <- max(fishSubset1$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    fish.int.1 <- with(fishSubset1, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.1 <- melt(fish.int.1$z, na.rm=TRUE)
    colnames(fish.int.melt.1) <- c("x", "y", "z")
    
    fish.int.melt.1$x <- fish.int.1$x[fish.int.melt.1$x]
    fish.int.melt.1$y <- fish.int.1$y[fish.int.melt.1$y]
    
    fish.int.melt.1$z <- transform_0_1(fish.int.melt.1$z)
    
    
    fish.int.melt.1[is.na(fish.int.melt.1)] <- 0
    
    fish.int.melt.1 <- subset(fish.int.melt.1, fish.int.melt.1$z > input$thresh5hold1)
    
    
    #   fish.int.melt.1$z <- fish.int.melt.1$z[ fish.int.melt.1$z<0.1 ] <- 0
    
    
    fish.int.2 <- with(fishSubset2, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.2 <- melt(fish.int.2$z, na.rm=TRUE)
    colnames(fish.int.melt.2) <- c("x", "y", "z")
    
    fish.int.melt.2$x <- fish.int.2$x[fish.int.melt.2$x]
    fish.int.melt.2$y <- fish.int.2$y[fish.int.melt.2$y]
    
    fish.int.melt.2$z <- transform_0_1(fish.int.melt.2$z)
    
    
    fish.int.melt.2[is.na(fish.int.melt.2)] <- 0
    
    fish.int.melt.2 <- subset(fish.int.melt.2, fish.int.melt.2$z > input$thresh5hold2)
    
    #fish.int.melt.2$z <- fish.int.melt.2$z[ fish.int.melt.2$z<0.1 ] <- 0
    
    
    fish.int.3 <- with(fishSubset3, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.3 <- melt(fish.int.3$z, na.rm=TRUE)
    colnames(fish.int.melt.3) <- c("x", "y", "z")
    
    fish.int.melt.3$x <- fish.int.3$x[fish.int.melt.3$x]
    fish.int.melt.3$y <- fish.int.3$y[fish.int.melt.3$y]
    
    fish.int.melt.3$z <- transform_0_1(fish.int.melt.3$z)
    
    
    fish.int.melt.3[is.na(fish.int.melt.3)] <- 0
    
    fish.int.melt.3 <- subset(fish.int.melt.3, fish.int.melt.3$z > input$thresh5hold3)
    
    #fish.int.melt.3$z <- fish.int.melt.3$z[ fish.int.melt.3$z<0.1 ] <- 0

    
    fish.int.4 <- with(fishSubset4, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.4 <- melt(fish.int.4$z, na.rm=TRUE)
    colnames(fish.int.melt.4) <- c("x", "y", "z")
    
    fish.int.melt.4$x <- fish.int.4$x[fish.int.melt.4$x]
    fish.int.melt.4$y <- fish.int.4$y[fish.int.melt.4$y]
    
    fish.int.melt.4$z <- transform_0_1(fish.int.melt.4$z)
    
    
    fish.int.melt.4[is.na(fish.int.melt.4)] <- 0
    
    fish.int.melt.4 <- subset(fish.int.melt.4, fish.int.melt.4$z > input$thresh5hold4)
    
    #fish.int.melt.4$z <- fish.int.melt.4$z[ fish.int.melt.4$z<0.1 ] <- 0
    
    
    fish.int.5 <- with(fishSubset5, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.5 <- melt(fish.int.5$z, na.rm=TRUE)
    colnames(fish.int.melt.5) <- c("x", "y", "z")
    
    fish.int.melt.5$x <- fish.int.5$x[fish.int.melt.5$x]
    fish.int.melt.5$y <- fish.int.5$y[fish.int.melt.5$y]
    
    fish.int.melt.5$z <- transform_0_1(fish.int.melt.5$z)
    
    
    fish.int.melt.5[is.na(fish.int.melt.5)] <- 0
    
    fish.int.melt.5 <- subset(fish.int.melt.5, fish.int.melt.5$z > input$thresh5hold5)
    
    #fish.int.melt.4$z <- fish.int.melt.4$z[ fish.int.melt.4$z<0.1 ] <- 0
    

    
    fish.x <- c(fish.int.melt.1$x, fish.int.melt.2$x, fish.int.melt.3$x, fish.int.melt.4$x, fish.int.melt.5$x)
    fish.y <- c(fish.int.melt.1$y, fish.int.melt.2$y, fish.int.melt.3$y, fish.int.melt.4$y, fish.int.melt.5$y)
    fish.z <- c(fish.int.melt.1$z, fish.int.melt.2$z, fish.int.melt.3$z, fish.int.melt.4$z, fish.int.melt.5$z)
    fish.element <- c(rep(paste("1. ", input$fiveelement1, sep="", collapse=""), length(fish.int.melt.1$z)),
    rep(paste("2. ", input$fiveelement2, sep="", collapse=""), length(fish.int.melt.2$z)),
    rep(paste("3. ", input$fiveelement3, sep="", collapse=""), length(fish.int.melt.3$z)),
    rep(paste("4. ", input$fiveelement4, sep="", collapse=""), length(fish.int.melt.4$z)),
    rep(paste("5. ", input$fiveelement5, sep="", collapse=""), length(fish.int.melt.5$z))
    )
    
    fish.merge <- data.frame(fish.x, fish.y, fish.z, fish.element)
    colnames(fish.merge) <- c("x", "y", "z", "Element")
    fish.merge
    
    
})




plotInputFive <- reactive({
    
    fish.merge <- dataSplit5()
    
    
    
    spectral.int.map <- ggplot(fish.merge, aes(x, y)) +
    geom_tile(aes(fill=Element, alpha=z)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    scale_fill_manual("Net Counts",
    breaks=c(paste("1. ", input$fiveelement1, sep="", collapse=""), paste("2. ", input$fiveelement2, sep="", collapse=""), paste("3. ", input$fiveelement3, sep="", collapse=""), paste("4. ", input$fiveelement4, sep="", collapse=""), paste("5. ", input$fiveelement5, sep="", collapse="")),
    values=c(input$element5color1, input$element5color2, input$element5color3, input$element5color4, input$element5color5)) +
    coord_equal() +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    
    spectral.int.map
    
})


output$fiveMap <- renderPlot({
    print(plotInputFive())
})


output$downloadfivemap <- downloadHandler(
filename = function() { paste(input$project, "_", input$fiveelement1, "_", input$fiveelement2, "_", input$fiveelement3, "_", input$fiveelement4, "_", input$fiveelement5,  '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInputFive(), width=7, height=7, dpi=300, device="tiff")
}
)


 


})



})


