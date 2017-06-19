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
    selectInput(inputId = "elements", label = h4("Choose Element Line"), choices =  outElements())
})

output$inLines <- renderUI({
    selectInput(inputId = "lines", label = h4("Choose Element Line"), choices =  outLines())
})


####Multi element Plot
output$inElement1 <- renderUI({
    selectInput(inputId = "element1", label = h4("Choose Element Line"), choices =  outElements(), selected="Fe")
})

output$inLine1 <- renderUI({
    selectInput(inputId = "line1", label = h4("Choose Element Line"), choices =  outLines())
})

output$inElement2 <- renderUI({
    selectInput(inputId = "element2", label = h4("Choose Element Line"), choices =  outElements(), selected="Co")
})

output$inLine2 <- renderUI({
    selectInput(inputId = "line2", label = h4("Choose Element Line"), choices =  outLines())
})

output$inElement3 <- renderUI({
    selectInput(inputId = "element3", label = h4("Choose Element Line"), choices =  outElements(), selected="Pb")
})

output$inLine3 <- renderUI({
    selectInput(inputId = "line3", label = h4("Choose Element Line"), choices =  outLines(), selected="L1")
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
    
    fish.int.melt <- subset(fish.int.melt, z > 0.1)

    
    
    spectral.int.map <- ggplot(fish.int.melt) +
    geom_tile(aes(x, y,  fill=z, alpha=z)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    coord_equal() +
    guides(alpha=FALSE) +
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


dataSplit <- reactive({
    
    fishImport <- myData()
    
    fishSubset1 <- fishImport %>% filter(Line==input$line1 & Element==input$element1)
    fishSubset2 <- fishImport %>% filter(Line==input$line2 & Element==input$element2)
    fishSubset3 <- fishImport %>% filter(Line==input$line3 & Element==input$element3)
    
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
    
    fish.int.melt.1 <- subset(fish.int.melt.1, fish.int.melt.1$z > 0.1)
    
    
    #   fish.int.melt.1$z <- fish.int.melt.1$z[ fish.int.melt.1$z<0.1 ] <- 0
    
    
    fish.int.2 <- with(fishSubset2, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.2 <- melt(fish.int.2$z, na.rm=TRUE)
    colnames(fish.int.melt.2) <- c("x", "y", "z")
    
    fish.int.melt.2$x <- fish.int.2$x[fish.int.melt.2$x]
    fish.int.melt.2$y <- fish.int.2$y[fish.int.melt.2$y]
    
    fish.int.melt.2$z <- transform_0_1(fish.int.melt.2$z)
    
    
    fish.int.melt.2[is.na(fish.int.melt.2)] <- 0
    
    fish.int.melt.2 <- subset(fish.int.melt.2, fish.int.melt.2$z > 0.1)
    
    #fish.int.melt.2$z <- fish.int.melt.2$z[ fish.int.melt.2$z<0.1 ] <- 0
    
    
    fish.int.3 <- with(fishSubset3, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.3 <- melt(fish.int.3$z, na.rm=TRUE)
    colnames(fish.int.melt.3) <- c("x", "y", "z")
    
    fish.int.melt.3$x <- fish.int.3$x[fish.int.melt.3$x]
    fish.int.melt.3$y <- fish.int.3$y[fish.int.melt.3$y]
    
    fish.int.melt.3$z <- transform_0_1(fish.int.melt.3$z)
    
    
    fish.int.melt.3[is.na(fish.int.melt.3)] <- 0
    
    fish.int.melt.3 <- subset(fish.int.melt.3, fish.int.melt.3$z > 0.1)
    
    #fish.int.melt.3$z <- fish.int.melt.3$z[ fish.int.melt.3$z<0.1 ] <- 0
    
    #fish.merge <- data.frame(fish.int.melt.1$x, fish.int.melt.1$y, fish.int.melt.1$z, fish.int.melt.2$z, fish.int.melt.3$z)
    #colnames(fish.merge) <- c("x", "y", "z1", "z2", "z3")
    
    fish.x <- c(fish.int.melt.1$x, fish.int.melt.2$x, fish.int.melt.3$x)
    fish.y <- c(fish.int.melt.1$y, fish.int.melt.2$y, fish.int.melt.3$y)
    fish.z <- c(fish.int.melt.1$z, fish.int.melt.2$z, fish.int.melt.3$z)
    fish.element <- c(rep(paste("1. ", input$element1, sep="", collapse=""), length(fish.int.melt.1$z)), rep(paste("2. ", input$element2, sep="", collapse=""), length(fish.int.melt.2$z)), rep(paste("3. ", input$element3, sep="", collapse=""), length(fish.int.melt.3$z)))
    
    fish.merge <- data.frame(fish.x, fish.y, fish.z, fish.element)
    colnames(fish.merge) <- c("x", "y", "z", "Element")
    fish.merge

    
})




plotInputMultiple <- reactive({
    
    fish.merge <- dataSplit()
    
    colvals = as.character(paste(input$colorramp, input$colorrampvalues, ")", sep="", collapse=""))
    

    spectral.int.map <- ggplot(fish.merge, aes(x, y)) +
    geom_tile(aes(fill=Element, alpha=z)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_alpha_continuous("Net Counts", range=c(0.1, 1)) +
    scale_fill_manual("Net Counts",
    breaks=c(paste("1. ", input$element1, sep="", collapse=""), paste("2. ", input$element2, sep="", collapse=""), paste("3. ", input$element3, sep="", collapse="")),
    values=c(input$elementcolor1, input$elementcolor2, input$elementcolor3)) +
    coord_equal() +
    guides(alpha=FALSE) +
    theme_classic()
    
    
    spectral.int.map
    
})


output$multiMap <- renderPlot({
    print(plotInputMultiple())
})


output$downloadmultimap <- downloadHandler(
filename = function() { paste(input$project, "_", input$element1, "_", input$element2, "_", input$element3, "_", '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInputMultiple(), width=7, height=7, dpi=300, device="tiff")
}
)


 


})



})


