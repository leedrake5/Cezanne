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
library(plyr)
library(dplyr)

library(ggplot2)
library(reshape2)
library(pbapply)
library(akima)

options(shiny.maxRequestSize=9000000*1024^2)

options(warn=-1)
assign("last.warning", NULL, envir = baseenv())


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    observeEvent(is.null(input$file1)==FALSE, {
        
        myDataHold <- reactive({
            
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
        
        
      


calFileContents <- reactive({
    
    existingCalFile <- input$calfileinput
    
    if (is.null(existingCalFile)) return(NULL)
    
    
    Calibration <- readRDS(existingCalFile$datapath)
    
    Calibration
    
})





calValHold <- reactive({
    
    
    calFileContents()[[6]]
    
    
    
    
    
})

calVariables <- reactive({
    
    
    calFileContents()$Intensities
    
    
    
})




calValElements <- reactive({
    calList <- calValHold()
    valelements <- ls(calList)
    valelements
})

calVariableElements <- reactive({
    variables <- calVariables()
    variableelements <- ls(variables)
    variableelements
})



fullInputValCounts <- reactive({
    valelements <- calValElements()
    variableelements <- calVariableElements()
    val.data <- myDataHold()
    
    
    
    all.col.names <- c( "Element", "Line", "Net", "Background", "x", "y")
    merge.label <- paste(val.data$Element, val.data$Line, sep=".")
    merge.coord <- paste(val.data$x, val.data$y, sep="-")
    
    simple.val.frame <- data.frame(merge.coord, merge.label, val.data$Net)
    colnames(simple.val.frame) <- c("Spectrum", "Element", "Net")
    
    rh.vals <- subset(simple.val.frame$Net, simple.val.frame$Element==input$rhscale)
   
    
    norm.val <- rh.vals/mean(calFileContents()$Spectra[,input$rhscale])*input$scalefactor
    
    norm.val.frame <- data.frame(merge.coord, merge.label, simple.val.frame$Net/norm.val)
    colnames(norm.val.frame) <- c("Spectrum", "Element", "Net")

    
    val.line.table <- dcast(data=norm.val.frame, formula=Spectrum ~ Element,  fun.aggregate=mean)

    val.line.table
})







tableInputValQuant <- reactive({
    
    
    count.table <- data.frame(fullInputValCounts())
    the.cal <- calValHold()
    elements.cal <- calValElements()
    elements <- elements.cal[!is.na(match(elements.cal, ls(count.table)))]
    variables <- calVariableElements()
    valdata <- myDataHold()
    
    

    
    predicted.list <- pblapply(elements, function (x)
    if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
        predict(
        object=the.cal[[x]][[2]],
        newdata=general.prep.net(
        spectra.line.table=as.data.frame(
        count.table
        ),
        element.line=x)
        )
    } else if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
        predict(
        object=the.cal[[x]][[2]],
        newdata=simple.tc.prep.net(
        data=valdata,
        spectra.line.table=as.data.frame(
        count.table
        ),
        element.line=x
        )
        )
    } else if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
        predict(
        object=the.cal[[x]][[2]],
        newdata=simple.comp.prep.net(
        data=valdata,
        spectra.line.table=as.data.frame(
        count.table
        ),
        element.line=x,
        norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
        norm.max=the.cal[[x]][[1]][1]$CalTable$Max
        )
        )
    } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
        predict(
        object=the.cal[[x]][[2]],
        newdata=lucas.simp.prep.net(
        spectra.line.table=as.data.frame(
        count.table
        ),
        element.line=x,
        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
        )
        )
    } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
        predict(
        object=the.cal[[x]][[2]],
        newdata=lucas.tc.prep.net(
        data=valdata,
        spectra.line.table=as.data.frame(
        count.table
        ),
        element.line=x,
        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
        )
        )
    } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
        predict(
        object=the.cal[[x]][[2]],
        newdata=lucas.comp.prep.net(
        data=valdata,
        spectra.line.table=as.data.frame(
        count.table
        ),
        element.line=x,
        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
        norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
        norm.max=the.cal[[x]][[1]][1]$CalTable$Max
        )
        )
    }
    
    
    
    )
    
    predicted.vector <- unlist(predicted.list)
    
    dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))
    
    predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)
    
    colnames(predicted.frame) <- c("Spectrum", elements)
    
    melt.frame <- melt(predicted.frame, id="Spectrum")
    
    colnames(melt.frame) <- c("Spectrum", "Element", "CPS")
    
    
    x <- as.numeric(gsub("[-][\\s\\S]*$", "", melt.frame$Spectrum, perl=T))
    y <- as.numeric(sub('.*\\-', '', melt.frame$Spectrum))

    
    element.char <- gsub("[.][\\s\\S]*$", "", melt.frame$Element, perl=T)
    line.char <- sub('.*\\.', '', melt.frame$Element)
    
    reform.table <- data.frame(element.char, line.char, melt.frame$CPS, x, y)
    colnames(reform.table) <- c( "Element", "Line", "Net", "x", "y")

    reform.table
    #predicted.values <- t(predicted.values)
    
    
})

myData <- reactive({
    
    if(input$usecalfile==FALSE){
        myDataHold()
    } else if(input$usecalfile==TRUE){
        tableInputValQuant()
    }
    
})


output$downloadtable <- downloadHandler(
filename = function() { paste(input$project, '.csv', sep='', collapse='') },
content = function(file
) {
    write.csv(myData(), file)
}
)

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
    selectInput(inputId = "threeelement1", label = h4("Element"), choices =  outElements(), selected=outElements()[1])
})

output$in3Line1 <- renderUI({
    selectInput(inputId = "threeline1", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in3Element2 <- renderUI({
    selectInput(inputId = "threeelement2", label = h4("Element"), choices =  outElements(), selected=outElements()[2])
})

output$in3Line2 <- renderUI({
    selectInput(inputId = "threeline2", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in3Element3 <- renderUI({
    selectInput(inputId = "threeelement3", label = h4("Element"), choices =  outElements(), selected=outElements()[3])
})

output$in3Line3 <- renderUI({
    selectInput(inputId = "threeline3", label = h4("Fluorescence Line"), choices =  outLines())
})



####Five Element Plot
output$in5Element1 <- renderUI({
    selectInput(inputId = "fiveelement1", label = h4("Element"), choices =  outElements(), selected=outElements()[1])
})

output$in5Line1 <- renderUI({
    selectInput(inputId = "fiveline1", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in5Element2 <- renderUI({
    selectInput(inputId = "fiveelement2", label = h4("Element"), choices =  outElements(), selected=outElements()[2])
})

output$in5Line2 <- renderUI({
    selectInput(inputId = "fiveline2", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in5Element3 <- renderUI({
    selectInput(inputId = "fiveelement3", label = h4("Element"), choices =  outElements(), selected=outElements()[3])
})

output$in5Line3 <- renderUI({
    selectInput(inputId = "fiveline3", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in5Element4 <- renderUI({
    selectInput(inputId = "fiveelement4", label = h4("Element"), choices =  outElements(), selected=outElements()[4])
})

output$in5Line4 <- renderUI({
    selectInput(inputId = "fiveline4", label = h4("Fluorescence Line"), choices =  outLines())
})

output$in5Element5 <- renderUI({
    selectInput(inputId = "fiveelement5", label = h4("Element"), choices =  outElements(), selected=outElements()[5])
})

output$in5Line5 <- renderUI({
    selectInput(inputId = "fiveline5", label = h4("Fluorescence Line"), choices =  outLines())
})

ranges1 <- reactiveValues(x = NULL, y = NULL)


interpSinglePrep <- reactive({
    
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
    
    fish.int.melt$altz <- transform_0_1(fish.int.melt$z)
    
    
    fish.int.melt$z <- ifelse(fish.int.melt$z < 0, 0, fish.int.melt$z)
    fish.int.melt <- subset(fish.int.melt, altz > input$threshhold)
    
    fish.int.melt
    
})

normSinglePrep <- reactive({
    fishImport <- myData()
    
    fishSubset <- fishImport %>% filter(Line==input$lines & Element==input$elements)
    
    
    
    fish.norm <- data.frame(fishSubset$x, fishSubset$y, fishSubset$Net)
    colnames(fish.norm) <- c("x", "y", "z")
    
    fish.norm$altz <- transform_0_1(fish.norm$z)
    
    fish.norm[is.na(fish.norm)] <- 0
    
    
    fish.norm
   
    
})


plotSinglePrep <- reactive({
    
  fish <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSinglePrep()
    }
    
    fish <- subset(fish, altz > input$threshhold)


    
})

output$downloadnorm <- downloadHandler(
filename = function() { paste(input$project, '.csv', sep='', collapse='') },
content = function(file
) {
    write.csv(plotSinglePrep(), file)
}
)




plotInputSingle <- reactive({
    input$actionprocess1

    
    colvals = as.character(paste(input$colorramp, input$colorrampvalues, ")", sep="", collapse=""))
    
   isolate(fish <- plotSinglePrep())
   
   fish <- subset(fish, altz > input$threshhold)

   
   

    
    
    ggplot(fish) +
    geom_tile(aes(x, y,  fill=z, alpha=altz)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradientn("Net Counts", colours=eval(parse(text=paste(colvals))), na.value = "white") +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    coord_equal(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE) +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    
  
    
})


output$simpleMap <- renderPlot({
    plotInputSingle()
})


# Float over info
output$hover_infosimp <- renderUI({
    
    point.table <- plotSinglePrep()
    

    
    hover <- input$plot_hoversimp
    point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
    style = style,
    p(HTML(paste0(input$elements, ": ", round(point$z, 4)
    
    )))
    )
})

observeEvent(input$plot_1_dblclick, {
    brush <- input$plot_1_brush
    if (!is.null(brush)) {
        ranges1$x <- c(brush$xmin, brush$xmax)
        ranges1$y <- c(brush$ymin, brush$ymax)
        
    } else {
        ranges1$x <- NULL
        ranges1$y <- NULL
    }
})


output$downloadmap <- downloadHandler(
filename = function() { paste(input$project, "_", input$elements, "_", input$lines, '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInputSingle(), width=7, height=7, dpi=300, device="tiff")
}
)


output$downloadmapjpg <- downloadHandler(
filename = function() { paste(input$project, "_", input$elements, "_", input$lines, '.jpg', sep='') },
content = function(file) {
    ggsave(file,plotInputSingle(), width=7, height=7, dpi=300, device="jpg")
}
)



interpSplit3one <- reactive({
    
    fishImport <- myData()
    
    fishSubset1 <- fishImport %>% filter(Line==input$threeline1 & Element==input$threeelement1)

    
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
    
    fish.int.melt.1$altz <- transform_0_1(fish.int.melt.1$z)
    
    fish.int.melt.1$z <- ifelse(fish.int.melt.1$z < 0, 0, fish.int.melt.1$z)

    fish.int.melt.1[is.na(fish.int.melt.1)] <- 0
    
    
    fish.int.melt.1
    
    
})


interpSplit3two <- reactive({
    
    fishImport <- myData()
    
    fishSubset2 <- fishImport %>% filter(Line==input$threeline2 & Element==input$threeelement2)
    
    ###Europe
    xmin <- min(fishSubset2$x)
    xmax <- max(fishSubset2$x)
    ymin <- min(fishSubset2$y)
    ymax <- max(fishSubset2$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    
    fish.int.2 <- with(fishSubset2, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.2 <- melt(fish.int.2$z, na.rm=TRUE)
    colnames(fish.int.melt.2) <- c("x", "y", "z")
    
    fish.int.melt.2$x <- fish.int.2$x[fish.int.melt.2$x]
    fish.int.melt.2$y <- fish.int.2$y[fish.int.melt.2$y]
    
    fish.int.melt.2$altz <- transform_0_1(fish.int.melt.2$z)
    
    fish.int.melt.2$z <- ifelse(fish.int.melt.2$z < 0, 0, fish.int.melt.2$z)
    
    fish.int.melt.2[is.na(fish.int.melt.2)] <- 0
    
    
    fish.int.melt.2
    
})

interpSplit3three <- reactive({
    
    fishImport <- myData()
    
    fishSubset3 <- fishImport %>% filter(Line==input$threeline3 & Element==input$threeelement3)
    
    ###Europe
    xmin <- min(fishSubset3$x)
    xmax <- max(fishSubset3$x)
    ymin <- min(fishSubset3$y)
    ymax <- max(fishSubset3$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    
    fish.int.3 <- with(fishSubset3, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.3 <- melt(fish.int.3$z, na.rm=TRUE)
    colnames(fish.int.melt.3) <- c("x", "y", "z")
    
    fish.int.melt.3$x <- fish.int.3$x[fish.int.melt.3$x]
    fish.int.melt.3$y <- fish.int.3$y[fish.int.melt.3$y]
    
    fish.int.melt.3$altz <- transform_0_1(fish.int.melt.3$z)
    
    fish.int.melt.3$z <- ifelse(fish.int.melt.3$z < 0, 0, fish.int.melt.3$z)
    
    fish.int.melt.3[is.na(fish.int.melt.3)] <- 0
    
    
     fish.int.melt.3
    

})

ranges3 <- reactiveValues(x = NULL, y = NULL)


dataSplit3 <- reactive({
    
    fish.int.melt.1 <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSplit3one()
    }
    
    fish.int.melt.2 <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSplit3two()
    }
    
    fish.int.melt.3 <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSplit3three()
    }
    
    
    fish.int.melt.1 <- subset(fish.int.melt.1, fish.int.melt.1$altz > input$thresh3hold1)
    fish.int.melt.2 <- subset(fish.int.melt.2, fish.int.melt.2$altz > input$thresh3hold2)
    fish.int.melt.3 <- subset(fish.int.melt.3, fish.int.melt.3$altz > input$thresh3hold3)

    
    fish.x <- c(fish.int.melt.1$x, fish.int.melt.2$x, fish.int.melt.3$x)
    fish.y <- c(fish.int.melt.1$y, fish.int.melt.2$y, fish.int.melt.3$y)
    fish.z <- c(fish.int.melt.1$z, fish.int.melt.2$z, fish.int.melt.3$z)
    fish.altz <- c(fish.int.melt.1$altz, fish.int.melt.2$altz, fish.int.melt.3$altz)
    fish.element <- c(rep(paste("1. ", input$threeelement1, sep="", collapse=""), length(fish.int.melt.1$z)), rep(paste("2. ", input$threeelement2, sep="", collapse=""), length(fish.int.melt.2$z)), rep(paste("3. ", input$threeelement3, sep="", collapse=""), length(fish.int.melt.3$z)))
    
    fish.merge <- data.frame(fish.x, fish.y, fish.z, fish.altz, fish.element)
    colnames(fish.merge) <- c("x", "y", "z", "altz", "Element")
    
    
    fish.merge


})


plotInputThree <- reactive({
    input$actionprocess3

    isolate(fish.merge <- dataSplit3())
    
    

    spectral.int.map <- ggplot(fish.merge, aes(x, y)) +
    geom_tile(aes(fill=Element, alpha=altz)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    scale_fill_manual("Net Counts",
    breaks=c(paste("1. ", input$threeelement1, sep="", collapse=""), paste("2. ", input$threeelement2, sep="", collapse=""), paste("3. ", input$threeelement3, sep="", collapse="")),
    values=c(input$element3color1, input$element3color2, input$element3color3)) +
    coord_equal(xlim = ranges3$x, ylim = ranges3$y, expand = FALSE) +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    
    spectral.int.map
    
})


output$threeMap <- renderPlot({
    plotInputThree()
})

# Float over info
output$hover_info3 <- renderUI({
    
    
    point.table <- dataSplit3()
    
    
    
    
    
    hover <- input$plot_hover3
    point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 3, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    

    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
    style = style,
    p(HTML(paste0(input$threeelement1, ": ", round(as.numeric(point$z[1]), 4), "<br/>",
    input$threeelement2, ": ", round(as.numeric(point$z[2]), 4), "<br/>",
    input$threeelement3, ": ", round(as.numeric(point$z[3]), 4)
    
    )))
    )
})


output$downloadthreemap <- downloadHandler(
filename = function() { paste(input$project, "_", input$threeelement1, "_", input$threeelement2, "_", input$threeelement3, '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInputThree(), width=7, height=7, dpi=300, device="tiff")
}
)

output$downloadthreemapjpg <- downloadHandler(
filename = function() { paste(input$project, "_", input$threeelement1, "_", input$threeelement2, "_", input$threeelement3, '.jpg', sep='') },
content = function(file) {
    ggsave(file,plotInputThree(), width=7, height=7, dpi=300, device="jpg")
}
)

observeEvent(input$plot_3_dblclick, {
    brush <- input$plot_3_brush
    if (!is.null(brush)) {
        ranges3$x <- c(brush$xmin, brush$xmax)
        ranges3$y <- c(brush$ymin, brush$ymax)
        
    } else {
        ranges3$x <- NULL
        ranges3$y <- NULL
    }
})



ranges5 <- reactiveValues(x = NULL, y = NULL)


interpSplit5one <- reactive({
    
    fishImport <- myData()
    
    fishSubset1 <- fishImport %>% filter(Line==input$fiveline1 & Element==input$fiveelement1)


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
    
    fish.int.melt.1$altz <- transform_0_1(fish.int.melt.1$z)
    
    fish.int.melt.1$z <- ifelse(fish.int.melt.1$z < 0, 0, fish.int.melt.1$z)

    fish.int.melt.1[is.na(fish.int.melt.1)] <- 0
    
    
    fish.int.melt.1

    
})


interpSplit5two <- reactive({
    
    fishImport <- myData()
    
    
fishSubset2 <- fishImport %>% filter(Line==input$fiveline2 & Element==input$fiveelement2)

    
    ###Europe
    xmin <- min(fishSubset2$x)
    xmax <- max(fishSubset2$x)
    ymin <- min(fishSubset2$y)
    ymax <- max(fishSubset2$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    
    fish.int.2 <- with(fishSubset2, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.2 <- melt(fish.int.2$z, na.rm=TRUE)
    colnames(fish.int.melt.2) <- c("x", "y", "z")
    
    fish.int.melt.2$x <- fish.int.2$x[fish.int.melt.2$x]
    fish.int.melt.2$y <- fish.int.2$y[fish.int.melt.2$y]
    
    fish.int.melt.2$altz <- transform_0_1(fish.int.melt.2$z)
    
    fish.int.melt.2$z <- ifelse(fish.int.melt.2$z < 0, 0, fish.int.melt.2$z)
    
    fish.int.melt.2[is.na(fish.int.melt.2)] <- 0
    
    fish.int.melt.2 <- subset(fish.int.melt.2, fish.int.melt.2$altz > input$thresh5hold2)
    
    fish.int.melt.2
    

    
})


interpSplit5three <- reactive({
    
    fishImport <- myData()
    
    fishSubset3 <- fishImport %>% filter(Line==input$fiveline3 & Element==input$fiveelement3)
    
    
    ###Europe
    xmin <- min(fishSubset3$x)
    xmax <- max(fishSubset3$x)
    ymin <- min(fishSubset3$y)
    ymax <- max(fishSubset3$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    fish.int.3 <- with(fishSubset3, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.3 <- melt(fish.int.3$z, na.rm=TRUE)
    colnames(fish.int.melt.3) <- c("x", "y", "z")
    
    fish.int.melt.3$x <- fish.int.3$x[fish.int.melt.3$x]
    fish.int.melt.3$y <- fish.int.3$y[fish.int.melt.3$y]
    
    fish.int.melt.3$altz <- transform_0_1(fish.int.melt.3$z)
    
    fish.int.melt.3$z <- ifelse(fish.int.melt.3$z < 0, 0, fish.int.melt.3$z)
    
    fish.int.melt.3[is.na(fish.int.melt.3)] <- 0
    
    
    fish.int.melt.3
    

    
    
})


interpSplit5four <- reactive({
    
    fishImport <- myData()
    
    fishSubset4 <- fishImport %>% filter(Line==input$fiveline4 & Element==input$fiveelement4)
    
    
    ###Europe
    xmin <- min(fishSubset4$x)
    xmax <- max(fishSubset4$x)
    ymin <- min(fishSubset4$y)
    ymax <- max(fishSubset4$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    

    fish.int.4 <- with(fishSubset4, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.4 <- melt(fish.int.4$z, na.rm=TRUE)
    colnames(fish.int.melt.4) <- c("x", "y", "z")
    
    fish.int.melt.4$x <- fish.int.4$x[fish.int.melt.4$x]
    fish.int.melt.4$y <- fish.int.4$y[fish.int.melt.4$y]
    
    fish.int.melt.4$altz <- transform_0_1(fish.int.melt.4$z)
    
    fish.int.melt.4$z <- ifelse(fish.int.melt.4$z < 0, 0, fish.int.melt.4$z)
    
    fish.int.melt.4[is.na(fish.int.melt.4)] <- 0
    
    
    fish.int.melt.4
    

    
})

interpSplit5five <- reactive({
    
    fishImport <- myData()
    
   
    fishSubset5 <- fishImport %>% filter(Line==input$fiveline5 & Element==input$fiveelement5)
    
    
    ###Europe
    xmin <- min(fishSubset5$x)
    xmax <- max(fishSubset5$x)
    ymin <- min(fishSubset5$y)
    ymax <- max(fishSubset5$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    

    fish.int.5 <- with(fishSubset5, interp(x=x, y=y, z=Net, duplicate="user", dupfun="min", nx=input$resolutionmulti, ny=input$resolutionmulti*y.ratio))
    fish.int.melt.5 <- melt(fish.int.5$z, na.rm=TRUE)
    colnames(fish.int.melt.5) <- c("x", "y", "z")
    
    fish.int.melt.5$x <- fish.int.5$x[fish.int.melt.5$x]
    fish.int.melt.5$y <- fish.int.5$y[fish.int.melt.5$y]
    
    fish.int.melt.5$altz <- transform_0_1(fish.int.melt.5$z)
    
    fish.int.melt.5$z <- ifelse(fish.int.melt.5$z < 0, 0, fish.int.melt.5$z)
    
    fish.int.melt.5[is.na(fish.int.melt.5)] <- 0
    
    
    fish.int.melt.5

    
})

dataSplit5 <- reactive({
    
    fish.int.melt.1 <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSplit5one()
    }
    
    fish.int.melt.2 <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSplit5two()
    }
    
    fish.int.melt.3 <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSplit5three()
    }
    
    fish.int.melt.4 <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSplit5four()
    }
    
    fish.int.melt.5 <- if(input$useinterp==FALSE){
        normSinglePrep()
    } else if(input$useinterp==TRUE){
        interpSplit5five()
    }
    
    
    fish.int.melt.1 <- subset(fish.int.melt.1, fish.int.melt.1$altz > input$thresh5hold1)
    fish.int.melt.2 <- subset(fish.int.melt.2, fish.int.melt.2$altz > input$thresh5hold2)
    fish.int.melt.3 <- subset(fish.int.melt.3, fish.int.melt.3$altz > input$thresh5hold3)
    fish.int.melt.4 <- subset(fish.int.melt.4, fish.int.melt.4$altz > input$thresh5hold4)
    fish.int.melt.5 <- subset(fish.int.melt.5, fish.int.melt.5$altz > input$thresh5hold5)

    
    
    
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
    input$actionprocess5

    isolate(fish.merge <- dataSplit5())
    
    
    
    spectral.int.map <- ggplot(fish.merge, aes(x, y)) +
    geom_tile(aes(fill=Element, alpha=z)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    scale_fill_manual("Net Counts",
    breaks=c(paste("1. ", input$fiveelement1, sep="", collapse=""), paste("2. ", input$fiveelement2, sep="", collapse=""), paste("3. ", input$fiveelement3, sep="", collapse=""), paste("4. ", input$fiveelement4, sep="", collapse=""), paste("5. ", input$fiveelement5, sep="", collapse="")),
    values=c(input$element5color1, input$element5color2, input$element5color3, input$element5color4, input$element5color5)) +
    coord_equal(xlim = ranges5$x, ylim = ranges5$y, expand = FALSE) +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    
    spectral.int.map
    
})


output$fiveMap <- renderPlot({
    plotInputFive()
})

# Float over info
output$hover_info5 <- renderUI({
    
    point.table <- dataSplit5()
    
    
    hover <- input$plot_hover5
    point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 5, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
    style = style,
    p(HTML(paste0(input$fiveelement1, ": ", round(point$z[1], 4), "<br/>",
    input$fiveelement2, ": ", round(point$z[2], 4), "<br/>",
    input$fiveelement3, ": ", round(point$z[3], 4), "<br/>",
    input$fiveelement4, ": ", round(point$z[4], 4), "<br/>",
    input$fiveelement5, ": ", round(point$z[5], 4)

    )))
    )
})


observeEvent(input$plot_5_dblclick, {
    brush <- input$plot_5_brush
    if (!is.null(brush)) {
        ranges5$x <- c(brush$xmin, brush$xmax)
        ranges5$y <- c(brush$ymin, brush$ymax)
        
    } else {
        ranges5$x <- NULL
        ranges5$y <- NULL
    }
})


output$downloadfivemap <- downloadHandler(
filename = function() { paste(input$project, "_", input$fiveelement1, "_", input$fiveelement2, "_", input$fiveelement3, "_", input$fiveelement4, "_", input$fiveelement5,  '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInputFive(), width=7, height=7, dpi=300, device="tiff")
}
)

output$downloadfivemapjpg <- downloadHandler(
filename = function() { paste(input$project, "_", input$fiveelement1, "_", input$fiveelement2, "_", input$fiveelement3, "_", input$fiveelement4, "_", input$fiveelement5,  '.jpg', sep='') },
content = function(file) {
    ggsave(file,plotInputFive(), width=7, height=7, dpi=300, device="jpg")
}
)



 


})



})


