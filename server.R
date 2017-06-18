library(shiny)
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




output$inElements <- renderUI({
    selectInput(inputId = "elements", label = h4("Choose Element Line"), choices =  outElements())
})

output$inLines <- renderUI({
    selectInput(inputId = "lines", label = h4("Choose Element Line"), choices =  outLines())
})





plotInput <- reactive({
    
    colvals = as.character(paste(input$colorramp, input$colorrampvalues, ")", sep="", collapse=""))
    
    fishImport <- myData()
    
    fishSubset <- fishImport %>% filter(Line==input$lines & Element==input$elements)
    
    spectral.map <- ggplot(fishSubset) +
    geom_tile(aes(x, y, colour=Net, fill=Net)) +
    scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    coord_equal() +
    theme_classic()
    
    
    
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
    
    fish.int.melt[is.na(fish.int.melt)] <- 0
    
    
    spectral.int.map <- ggplot(fish.int.melt) +
    geom_tile(aes(x, y, colour=z, fill=z)) +
    scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    coord_equal() +
    theme_classic()
    
    
    
    if (input$interpolate == FALSE) {
        spectral.map
    } else if (input$interpolate == TRUE) {
        spectral.int.map
    }






})


output$simpleMap <- renderPlot({
    print(plotInput())
})


output$downloadmap <- downloadHandler(
filename = function() { paste(input$dataset, '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInput(), width=7, height=7, dpi=300, device="tiff")
}


)




})



})


