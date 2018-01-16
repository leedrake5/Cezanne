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
library(openxlsx)

library(ggplot2)
library(reshape2)
library(pbapply)
library(akima)
library(parallel)
library(stringi)


options(shiny.maxRequestSize=9000000*1024^2)

options(warn=-1)
assign("last.warning", NULL, envir = baseenv())


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    netCounts <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            core.n <- detectCores()-2
            
            n <- length(inFile$name)
            
            my.x <- mclapply(inFile$name, function(x) read_csv_x(x), mc.cores=6L)
            my.y <- mclapply(inFile$name, function(x) read_csv_y(x), mc.cores=6L)
            
            
            myfiles = pblapply(inFile$datapath, function(x) read_csv_net(x), cl=6L)
            
            myfiles.1 <- mcmapply(cbind, myfiles, "x" <- my.x, SIMPLIFY=F, mc.cores=6L)
            myfiles.2 <- mcmapply(cbind, myfiles.1, "y" <- my.y, SIMPLIFY=F, mc.cores=6L)
            
            all.col.names <- c( "Element", "Line", "Net", "Background", "x", "y")
            
            myfiles.list <- pblapply(myfiles.2, setNames, all.col.names, cl=6L)
            
            
            
            
            myfiles.frame <- do.call(rbind, pblapply(myfiles.list, data.frame, stringsAsFactors=FALSE, cl=6L))
            
            myfiles.frame$ElementLine <- paste0(myfiles.frame$Element, ".", myfiles.frame$Line)
            
            
            data <- reshape2::dcast(myfiles.frame, x+y~ElementLine, value.var="Net", fun.aggregate=mean)
            
            
            
            data
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
            data
        })
        
        
        
    })
    
    
    sheetData <- reactive({
        
        inFile <- input$file1
        
        withProgress(message = 'Processing Data', value = 0, {

        data <- read.csv(file=inFile$datapath)
        data$ElementLine <- paste0(data$Element, ".", data$Line)
        data <- reshape2::dcast(data, x+y~ElementLine, value.var="Net", fun.aggregate=mean)

        
        n <- length(inFile$name)
        
        incProgress(1/n)
        Sys.sleep(0.1)
        data
        })
        
        
        
    })
    
    ExcelData <- reactive({
        
        inFile <- input$file1
        
        #if (is.null(inFile)) {return(NULL)}
        
        
        
        proto.fish <- loadWorkbook(file=inFile$datapath)
        just.fish <- readWorkbook(proto.fish, sheet=2)
        new.fish <- just.fish[,-1]
        new.fish$x <- sapply(just.fish[,1], read_csv_x)
        new.fish$y <- sapply(just.fish[,1], read_csv_y)
        
        final.fish <- new.fish[,c("x", "y", names(just.fish[,-1]))]
        
        final.fish


        
    })
    
   
    
    
    
    observeEvent(is.null(input$file1)==FALSE, {
        
        myDataHold <- reactive({
            
            if(input$filetype=="Net"){
                netCounts()
            } else if(input$filetype=="Sheet"){
                sheetData()
            } else if(input$filetype=="Excel"){
                ExcelData()
            }
            
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
    
    
    
    #all.col.names <- c( "Element", "Line", "Net", "Background", "x", "y")
    #merge.label <- paste(val.data$Element, val.data$Line, sep=".")
    #merge.coord <- paste(val.data$x, val.data$y, sep="-")
    
    #simple.val.frame <- data.frame(merge.coord, merge.label, val.data$Net)
    #colnames(simple.val.frame) <- c("Spectrum", "Element", "Net")
    
    #rh.vals <- subset(simple.val.frame$Net, simple.val.frame$Element==input$rhscale)
    
    #norm.val <- rh.vals/mean(calFileContents()$Spectra[,input$rhscale])*input$scalefactor
    norm.val <- val.data[,input$rhscale]/mean(calFileContents()$Spectra[,input$rhscale])*input$scalefactor
    
    #norm.val.frame <- data.frame(merge.coord, merge.label, simple.val.frame$Net/norm.val)
    #colnames(norm.val.frame) <- c("Spectrum", "Element", "Net")

    
    #val.line.table <- dcast(data=norm.val.frame, formula=Spectrum ~ Element,  fun.aggregate=mean, drop=FALSE)

    #val.line.table[complete.cases(val.line.table),]
    val.line.data <- val.data[,3:length(val.data)]/norm.val
    final.frame <- data.frame(val.data[,1:2], val.line.data)
    final.frame
})







tableInputValQuant <- reactive({
    
    
    count.table <- data.frame(fullInputValCounts())
    the.cal <- calValHold()
    elements.cal <- calValElements()
    elements <- elements.cal[!is.na(match(elements.cal, ls(count.table)))]
    variables <- calVariableElements()
    valdata <- myDataHold()
    
    
    core.n <- detectCores()-2

    
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
    }, cl=6L
    )
    
    predicted.vector <- unlist(predicted.list)
    
    dim(predicted.vector) <- c(length(count.table[,1]), length(elements))
    
    predicted.frame <- data.frame(count.table[,1:2], predicted.vector)
    
    colnames(predicted.frame) <- c("x", "y", elements)
    
    #predicted.data.table <- data.table(predicted.frame)
    #predicted.values <- t(predicted.values)
    
    #predicted.data.table
    predicted.frame
    
})

dataHold <- reactive({
    
    results <- if(input$usecalfile==FALSE){
        myDataHold()
    } else if(input$usecalfile==TRUE){
        tableInputValQuant()
    }
    
    results
    
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
    metadata.dat <- dataHold()
    
    element.names <- unique(t(as.data.frame(strsplit(colnames(metadata.dat[,3:length(metadata.dat)]), split="[.]")))[,1])
    
    element.names
    
    
})





####Single Plot
output$inElements <- renderUI({
    selectInput(inputId = "elements", label = h4("Element"), choices =  outElements())
})


outLines <- reactive({
    
    metadata.dat <- dataHold()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$elements
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})

output$inLines <- renderUI({
    selectInput(inputId = "lines", label=NULL, choices =  outLines())
})



ranges1 <- reactiveValues(x = NULL, y = NULL)






myData <- reactive({
    
    results <- dataHold()
    
    y.unique <- unique(results$y)
    y.choose <- y.unique[seq(1, length(y.unique), 2)]
    y.check <- sapply(results$y, function(x) match(x, y.choose))
    y.is <- y.check==1
    y.is[is.na(y.is)] <- TRUE
    
    results$maintain <- y.is
    
    keep.frame <- subset(results, maintain==TRUE)
    alter.frame <- subset(results, maintain==FALSE)
    
    alter.frame$x <- alter.frame$x+input$adjust
    
    results.prelim <- rbind(keep.frame, alter.frame)
    
    results.final <- results.prelim[,1:length(results.prelim)-1]
    
    #results$Net <- ifelse(results$Net < 0, 0, results$Net)
    
    results.final
    
    
    
})









#####Single Element Map

xLength1 <- reactive({
    
    if(input$default1==TRUE){
        length(unique(myData()$x))/length(unique(myData()$y))
    } else {
        as.numeric("100")
    }
    
})


output$inresolution1 <- renderUI({
    
    sliderInput("resolution1", label = "Interpolation Resolution", value=xLength1(), min=10, max=10000)
    
    
})


interpSinglePrep <- reactive({
    
    fishImport <- myData()
    
    #fishSubset <- fishImport %>% filter(Line==input$lines & Element==input$elements)
    fishSubset <- fishImport[,c("x", "y", paste0(input$elements, ".", input$lines))]
    colnames(fishSubset)[3] <- "Net"
    
    
    ###Europe
    xmin <- min(fishSubset$x)
    xmax <- max(fishSubset$x)
    ymin <- min(fishSubset$y)
    ymax <- max(fishSubset$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    fish.int <- with(fishSubset, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution1, ny=input$resolution1*y.ratio))
    fish.int.melt <- melt(fish.int$z, na.rm=TRUE)
    colnames(fish.int.melt) <- c("x", "y", "z")
    
    fish.int.melt$x <- fish.int$x[fish.int.melt$x]
    fish.int.melt$y <- fish.int$y[fish.int.melt$y]
    
    fish.int.melt$altz <- transform_0_1(fish.int.melt$z)
    
    
    fish.int.melt$z <- as.numeric(ifelse(fish.int.melt$z < 0, 0, fish.int.melt$z))
    
    fish.int.melt[complete.cases(fish.int.melt), ]
    
})

normSinglePrep <- reactive({
    fishImport <- myData()
    
    #fishSubset <- fishImport %>% filter(Line==input$lines & Element==input$elements)
    fishSubset <- fishImport[,c("x", "y", paste0(input$elements, ".", input$lines))]
    colnames(fishSubset)[3] <- "Net"
    
    
    
    fish.norm <- data.frame(fishSubset$x, fishSubset$y, fishSubset$Net)
    colnames(fish.norm) <- c("x", "y", "z")
    
    fish.norm$altz <- transform_0_1(fish.norm$z)
    
    fish.norm[is.na(fish.norm)] <- 0
    
    
    fish.norm
    
    
})


plotSinglePrep <- reactive({
    
    fish <- interpSinglePrep()
    
    
    fish <- subset(fish, altz > input$threshhold)
    
    fish
    
    
    
})

output$downloadnorm <- downloadHandler(
filename = function() { paste(input$project, '.csv', sep='', collapse='') },
content = function(file
) {
    write.csv(plotSinglePrep(), file)
}
)


singlePlotType <- reactive({
    
    if(input$colorramp=="Terrain"){
        "ColorRamp"
    } else if(input$colorramp=="Rainbow"){
        "ColorRamp"
    } else if(input$colorramp=="Heat"){
        "ColorRamp"
    } else if(input$colorramp=="Topo"){
        "ColorRamp"
    } else if(input$colorramp=="CM"){
        "ColorRamp"
    } else if (input$colorramp=="Black and White"){
        "BW"
    } else if (input$colorramp=="White and Black"){
        "WB"
    }
    
    
})

plotInputSingle <- reactive({
    
    
    colvals <- if(singlePlotType()=="ColorRamp"){
        as.character(paste(input$colorramp, input$colorrampvalues, ")", sep="", collapse=""))
    } else {
        as.character(paste("terrain.colors(", input$colorrampvalues, ")", sep="", collapse=""))
    }
    
    fish <-  plotSinglePrep()
  
    
    colorramp.plot <- ggplot(fish) +
    geom_tile(aes(x, y,  fill=z, alpha=altz)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradientn("Net Counts", colours=eval(parse(text=paste(colvals))), na.value = "white") +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    coord_equal(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE) +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    bw.plot <- ggplot(fish) +
    geom_tile(aes(x, y,  fill=z, alpha=altz)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradient2("Net Counts", low="white", high="black", na.value = "white") +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    coord_equal(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE) +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    wb.plot <- ggplot(fish) +
    geom_tile(aes(x, y,  fill=z, alpha=altz)) +
    #scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradient("Net Counts", low="black", high="white", na.value = "black") +
    scale_alpha_continuous("Net Counts", range=c(0, 1)) +
    coord_equal(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE) +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    
    
    
    
    if(singlePlotType()=="ColorRamp"){
        colorramp.plot
    } else if(singlePlotType()=="BW"){
        bw.plot
    } else if(singlePlotType()=="WB"){
        wb.plot
    }
    
    
})


output$simpleMap <- renderPlot({
    
    input$actionprocess1
    
    isolate(plotInputSingle())
})


# Float over info
output$hover_infosimp <- renderUI({
    input$actionprocess1
    
    isolate(point.table <- plotSinglePrep())
    
    
    
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


#####PCA Analysis


lineOptions <- reactive({
    
    spectra.line.table <- myData()
    
    element.names <- colnames(spectra.line.table[,3:length(spectra.line.table)])
    
    element.names
    
})

defaultLines <- reactive({
    
    spectra.line.table <- myData()
    
    element.names <- colnames(spectra.line.table[,3:length(spectra.line.table)])
    
    element.names[1:3]
    
})






output$defaultlines <- renderUI({
    
    
    checkboxGroupInput('show_vars', 'Elemental lines to show:',
    choices=lineOptions(), selected = defaultLines())
})

xrfKReactive <- reactive({
    
    spectra.line.table <- myData()
    
    xrf.pca.frame <- spectra.line.table[,input$show_vars]
    xrf.pca.frame <- xrf.pca.frame[complete.cases(xrf.pca.frame),]
    
    
    
    xrf.k <- kmeans(xrf.pca.frame, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
    xrf.pca <- prcomp(xrf.pca.frame, scale.=FALSE)
    
    xrf.scores <- as.data.frame(xrf.pca$x)
    
    score.names <- colnames(xrf.scores)
    
    stri_sub(score.names, 1, 2) <- "PC."
    colnames(xrf.scores) <- score.names
    
    cluster.frame <- data.frame(spectra.line.table, xrf.k$cluster, xrf.scores)
    
    colnames(cluster.frame) <- c(names(spectra.line.table), "Cluster.Cluster", names(xrf.scores) )
    
    cluster.frame
    
    
    
})


choiceLinesPCA <- reactive({
    
    spectra.line.table <- xrfKReactive()
    
    element.names <- colnames(spectra.line.table[,3:length(spectra.line.table)])
    
    element.names
    
    
})


outElementsPCA <- reactive({
    metadata.dat <- xrfKReactive()
    
    element.names <- unique(t(as.data.frame(strsplit(colnames(metadata.dat[,3:length(metadata.dat)]), split="[.]")))[,1])
    
    element.names
    
    
})


###Three Element Map

output$in3Element1 <- renderUI({
    selectInput(inputId = "threeelement1", label = h4("Element"), choices =  outElementsPCA(), selected=outElementsPCA()[1])
})

outLinesPCA_3p1 <- reactive({
    
    metadata.dat <- xrfKReactive()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$threeelement1
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})



output$in3Line1 <- renderUI({
    selectInput(inputId = "threeline1", label=NULL, choices =  outLinesPCA_3p1())
})

output$in3Element2 <- renderUI({
    selectInput(inputId = "threeelement2", label = h4("Element"), choices =  outElementsPCA(), selected=outElementsPCA()[2])
})


outLinesPCA_3p2 <- reactive({
    
    metadata.dat <- xrfKReactive()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$threeelement2
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})

output$in3Line2 <- renderUI({
    selectInput(inputId = "threeline2", label=NULL, choices =  outLinesPCA_3p2())
})

output$in3Element3 <- renderUI({
    selectInput(inputId = "threeelement3", label = h4("Element"), choices =  outElementsPCA(), selected=outElementsPCA()[3])
})

outLinesPCA_3p3 <- reactive({
    
    metadata.dat <- xrfKReactive()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$threeelement3
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})

output$in3Line3 <- renderUI({
    selectInput(inputId = "threeline3", label=NULL, choices =  outLinesPCA_3p3())
})



###Five Element Map

output$in5Element1 <- renderUI({
    selectInput(inputId = "fiveelement1", label = h4("Element"), choices =  outElementsPCA(), selected=outElementsPCA()[1])
})

outLinesPCA_5p1 <- reactive({
    
    metadata.dat <- xrfKReactive()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$fiveelement1
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})

output$in5Line1 <- renderUI({
    selectInput(inputId = "fiveline1", label=NULL, choices =  outLinesPCA_5p1())
})

output$in5Element2 <- renderUI({
    selectInput(inputId = "fiveelement2", label = h4("Element"), choices =  outElementsPCA(), selected=outElementsPCA()[2])
})

outLinesPCA_5p2 <- reactive({
    
    metadata.dat <- xrfKReactive()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$fiveelement2
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})


output$in5Line2 <- renderUI({
    selectInput(inputId = "fiveline2", label=NULL, choices =  outLinesPCA_5p2())
})

output$in5Element3 <- renderUI({
    selectInput(inputId = "fiveelement3", label = h4("Element"), choices =  outElementsPCA(), selected=outElementsPCA()[3])
})

outLinesPCA_5p3 <- reactive({
    
    metadata.dat <- xrfKReactive()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$fiveelement3
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})


output$in5Line3 <- renderUI({
    selectInput(inputId = "fiveline3", label=NULL, choices =  outLinesPCA_5p3())
})

output$in5Element4 <- renderUI({
    selectInput(inputId = "fiveelement4", label = h4("Element"), choices =  outElementsPCA(), selected=outElementsPCA()[4])
})

outLinesPCA_5p4 <- reactive({
    
    metadata.dat <- xrfKReactive()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$fiveelement4
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})

output$in5Line4 <- renderUI({
    selectInput(inputId = "fiveline4", label=NULL, choices =  outLinesPCA_5p4())
})

output$in5Element5 <- renderUI({
    selectInput(inputId = "fiveelement5", label = h4("Element"), choices =  outElementsPCA(), selected=outElementsPCA()[5])
})

outLinesPCA_5p5 <- reactive({
    
    metadata.dat <- xrfKReactive()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$fiveelement5
    
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
    
})


output$in5Line5 <- renderUI({
    selectInput(inputId = "fiveline5", label=NULL, choices =  outLinesPCA_5p5())
})







plotInput2 <- reactive({
    
    spectra.line.table <- xrfKReactive()
    
    
    
    regular <- ggplot(data= spectra.line.table) +
    geom_point(aes(PC.1, PC.2, colour=as.factor(Cluster.Cluster), shape=as.factor(Cluster.Cluster)), size = input$spotsize+1) +
    geom_point(aes(PC.1, PC.2), colour="grey30", size=input$spotsize-2) +
    scale_x_continuous("Principle Component 1") +
    scale_y_continuous("Principle Component 2") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15)) +
    scale_shape_manual("Cluster", values=1:nlevels(as.factor(spectra.line.table$Cluster.Cluster))) +
    scale_colour_discrete("Cluster") +
    geom_point(aes(PC.1, PC.2), colour="grey30", size=input$spotsize-2)
    
    
    ellipse <- ggplot(data= spectra.line.table)+
    geom_point(aes(PC.1, PC.2, colour=as.factor(Cluster.Cluster), shape=as.factor(Cluster.Cluster)), size = input$spotsize+1) +
    geom_point(aes(PC.1, PC.2), colour="grey30", size=input$spotsize-2) +
    scale_x_continuous("Principle Component 1") +
    scale_y_continuous("Principle Component 2") +
    theme_light() +
    stat_ellipse(aes(PC.1, PC.2, colour=as.factor(Cluster.Cluster), linetype=as.factor(Cluster.Cluster))) +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15)) +
    guides(linetype=FALSE) +
    scale_shape_manual("Cluster", values=1:nlevels(as.factor(spectra.line.table$Cluster.Cluster))) +
    scale_colour_discrete("Cluster") +
    geom_point(aes(PC.1, PC.2), colour="grey30", size=input$spotsize-2)

    
    if (input$elipseplot1 == TRUE) {
        ellipse
    } else if (input$elipseplot1 == FALSE) {
        regular
    }
    
    
    
})


output$xrfpcaplot <- renderPlot({
    plotInput2()
    
})

xLength3 <- reactive({
    
    if(input$default3==TRUE){
        length(unique(xrfKReactive()$x))/length(unique(xrfKReactive()$y))
    } else {
        as.numeric("100")
    }
    
})


output$inresolution3 <- renderUI({
    
    sliderInput("resolution3", label = "Interpolation Resolution", value=xLength3(), min=10, max=10000)

    
})

interpSplit3one <- reactive({
    
    fishImport <- xrfKReactive()
    
    #fishSubset1 <- fishImport %>% filter(Line==input$threeline1 & Element==input$threeelement1)
    fishSubset1 <- fishImport[,c("x", "y", paste0(input$threeelement1, ".", input$threeline1))]
    colnames(fishSubset1)[3] <- "Net"

    
    ###Europe
    xmin <- min(fishSubset1$x)
    xmax <- max(fishSubset1$x)
    ymin <- min(fishSubset1$y)
    ymax <- max(fishSubset1$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    fish.int.1 <- with(fishSubset1, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution3, ny=input$resolution3*y.ratio))
    fish.int.melt.1 <- melt(fish.int.1$z, na.rm=TRUE)
    colnames(fish.int.melt.1) <- c("x", "y", "z")
    
    fish.int.melt.1$x <- fish.int.1$x[fish.int.melt.1$x]
    fish.int.melt.1$y <- fish.int.1$y[fish.int.melt.1$y]
    
    fish.int.melt.1$altz <- transform_0_1(fish.int.melt.1$z)
    
    fish.int.melt.1$z <- ifelse(fish.int.melt.1$z < 0, 0, fish.int.melt.1$z)

    fish.int.melt.1[is.na(fish.int.melt.1)] <- 0
    if(input$scale3==TRUE){fish.int.melt.1$z <- scale(fish.int.melt.1$z)}

    
    fish.int.melt.1
    
    
})


interpSplit3two <- reactive({
    
    fishImport <- xrfKReactive()
    
    #fishSubset2 <- fishImport %>% filter(Line==input$threeline2 & Element==input$threeelement2)
    fishSubset2 <- fishImport[,c("x", "y", paste0(input$threeelement2, ".", input$threeline2))]
    colnames(fishSubset2)[3] <- "Net"

    ###Europe
    xmin <- min(fishSubset2$x)
    xmax <- max(fishSubset2$x)
    ymin <- min(fishSubset2$y)
    ymax <- max(fishSubset2$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    
    fish.int.2 <- with(fishSubset2, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution3, ny=input$resolution3*y.ratio))
    fish.int.melt.2 <- melt(fish.int.2$z, na.rm=TRUE)
    colnames(fish.int.melt.2) <- c("x", "y", "z")
    
    fish.int.melt.2$x <- fish.int.2$x[fish.int.melt.2$x]
    fish.int.melt.2$y <- fish.int.2$y[fish.int.melt.2$y]
    
    fish.int.melt.2$altz <- transform_0_1(fish.int.melt.2$z)
    
    fish.int.melt.2$z <- ifelse(fish.int.melt.2$z < 0, 0, fish.int.melt.2$z)
    
    fish.int.melt.2[is.na(fish.int.melt.2)] <- 0
    if(input$scale3==TRUE){fish.int.melt.2$z <- scale(fish.int.melt.2$z)}
    
    fish.int.melt.2
    
})

interpSplit3three <- reactive({
    
    fishImport <- xrfKReactive()
    
    #fishSubset3 <- fishImport %>% filter(Line==input$threeline3 & Element==input$threeelement3)
    fishSubset3 <- fishImport[,c("x", "y", paste0(input$threeelement3, ".", input$threeline3))]
    colnames(fishSubset3)[3] <- "Net"

    ###Europe
    xmin <- min(fishSubset3$x)
    xmax <- max(fishSubset3$x)
    ymin <- min(fishSubset3$y)
    ymax <- max(fishSubset3$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    
    fish.int.3 <- with(fishSubset3, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution3, ny=input$resolution3*y.ratio))
    fish.int.melt.3 <- melt(fish.int.3$z, na.rm=TRUE)
    colnames(fish.int.melt.3) <- c("x", "y", "z")
    
    fish.int.melt.3$x <- fish.int.3$x[fish.int.melt.3$x]
    fish.int.melt.3$y <- fish.int.3$y[fish.int.melt.3$y]
    
    fish.int.melt.3$altz <- transform_0_1(fish.int.melt.3$z)
    
    fish.int.melt.3$z <- ifelse(fish.int.melt.3$z < 0, 0, fish.int.melt.3$z)
    
    fish.int.melt.3[is.na(fish.int.melt.3)] <- 0
    if(input$scale3==TRUE){fish.int.melt.3$z <- scale(fish.int.melt.3$z)}

    
     fish.int.melt.3
    

})

ranges3 <- reactiveValues(x = NULL, y = NULL)


dataSplit3 <- reactive({
    
    fish.int.melt.1 <- interpSplit3one()
    
    
    fish.int.melt.2 <- interpSplit3two()
    
    fish.int.melt.3 <- interpSplit3three()
    
    
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
    
    
    fish.merge[complete.cases(fish.merge), ]


})


plotInputThree <- reactive({

    fish.merge <- dataSplit3()
    
    

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
    
    input$actionprocess3

    isolate(plotInputThree())
})

# Float over info
output$hover_info3 <- renderUI({
    input$actionprocess3

    
    isolate(point.table <- dataSplit3())
    
    
    
    
    
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




xLength5 <- reactive({
    
    if(input$default5==TRUE){
        length(unique(xrfKReactive()$x))/length(unique(xrfKReactive()$y))
    } else {
        as.numeric("100")
    }
    
})


output$inresolution5 <- renderUI({
    
    sliderInput("resolution5", label = "Interpolation Resolution", value=xLength5(), min=10, max=10000)
    
    
})


ranges5 <- reactiveValues(x = NULL, y = NULL)


interpSplit5one <- reactive({
    
    fishImport <- xrfKReactive()
    
    #fishSubset1 <- fishImport %>% filter(Line==input$fiveline1 & Element==input$fiveelement1)
    fishSubset1 <- fishImport[,c("x", "y", paste0(input$fiveelement1, ".", input$fiveline1))]
    colnames(fishSubset1)[3] <- "Net"


    ###Europe
    xmin <- min(fishSubset1$x)
    xmax <- max(fishSubset1$x)
    ymin <- min(fishSubset1$y)
    ymax <- max(fishSubset1$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    fish.int.1 <- with(fishSubset1, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution5, ny=input$resolution5*y.ratio))
    fish.int.melt.1 <- melt(fish.int.1$z, na.rm=TRUE)
    colnames(fish.int.melt.1) <- c("x", "y", "z")
    
    fish.int.melt.1$x <- fish.int.1$x[fish.int.melt.1$x]
    fish.int.melt.1$y <- fish.int.1$y[fish.int.melt.1$y]
    
    fish.int.melt.1$altz <- transform_0_1(fish.int.melt.1$z)
    
    fish.int.melt.1$z <- ifelse(fish.int.melt.1$z < 0, 0, fish.int.melt.1$z)

    fish.int.melt.1[is.na(fish.int.melt.1)] <- 0
    if(input$scale5==TRUE){fish.int.melt.1$z <- scale(fish.int.melt.1$z)}

    
    fish.int.melt.1

    
})


interpSplit5two <- reactive({
    
    fishImport <- xrfKReactive()
    
    
    #fishSubset2 <- fishImport %>% filter(Line==input$fiveline2 & Element==input$fiveelement2)
    fishSubset2 <- fishImport[,c("x", "y", paste0(input$fiveelement2, ".", input$fiveline2))]
    colnames(fishSubset2)[3] <- "Net"

    
    ###Europe
    xmin <- min(fishSubset2$x)
    xmax <- max(fishSubset2$x)
    ymin <- min(fishSubset2$y)
    ymax <- max(fishSubset2$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    
    fish.int.2 <- with(fishSubset2, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution5, ny=input$resolution5*y.ratio))
    fish.int.melt.2 <- melt(fish.int.2$z, na.rm=TRUE)
    colnames(fish.int.melt.2) <- c("x", "y", "z")
    
    fish.int.melt.2$x <- fish.int.2$x[fish.int.melt.2$x]
    fish.int.melt.2$y <- fish.int.2$y[fish.int.melt.2$y]
    
    fish.int.melt.2$altz <- transform_0_1(fish.int.melt.2$z)
    
    fish.int.melt.2$z <- ifelse(fish.int.melt.2$z < 0, 0, fish.int.melt.2$z)
    
    fish.int.melt.2[is.na(fish.int.melt.2)] <- 0
    
    fish.int.melt.2 <- subset(fish.int.melt.2, fish.int.melt.2$altz > input$thresh5hold2)
    if(input$scale5==TRUE){fish.int.melt.2$z <- scale(fish.int.melt.2$z)}

    fish.int.melt.2
    

    
})


interpSplit5three <- reactive({
    
    fishImport <- xrfKReactive()
    
    #fishSubset3 <- fishImport %>% filter(Line==input$fiveline3 & Element==input$fiveelement3)
    fishSubset3 <- fishImport[,c("x", "y", paste0(input$fiveelement3, ".", input$fiveline3))]
    colnames(fishSubset3)[3] <- "Net"

    
    ###Europe
    xmin <- min(fishSubset3$x)
    xmax <- max(fishSubset3$x)
    ymin <- min(fishSubset3$y)
    ymax <- max(fishSubset3$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    
    fish.int.3 <- with(fishSubset3, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution5, ny=input$resolution5*y.ratio))
    fish.int.melt.3 <- melt(fish.int.3$z, na.rm=TRUE)
    colnames(fish.int.melt.3) <- c("x", "y", "z")
    
    fish.int.melt.3$x <- fish.int.3$x[fish.int.melt.3$x]
    fish.int.melt.3$y <- fish.int.3$y[fish.int.melt.3$y]
    
    fish.int.melt.3$altz <- transform_0_1(fish.int.melt.3$z)
    
    fish.int.melt.3$z <- ifelse(fish.int.melt.3$z < 0, 0, fish.int.melt.3$z)
    
    fish.int.melt.3[is.na(fish.int.melt.3)] <- 0
    if(input$scale5==TRUE){fish.int.melt.3$z <- scale(fish.int.melt.3$z)}

    
    fish.int.melt.3
    

    
    
})


interpSplit5four <- reactive({
    
    fishImport <- xrfKReactive()
    
    #fishSubset4 <- fishImport %>% filter(Line==input$fiveline4 & Element==input$fiveelement4)
    fishSubset4 <- fishImport[,c("x", "y", paste0(input$fiveelement4, ".", input$fiveline4))]
    colnames(fishSubset4)[3] <- "Net"

    
    ###Europe
    xmin <- min(fishSubset4$x)
    xmax <- max(fishSubset4$x)
    ymin <- min(fishSubset4$y)
    ymax <- max(fishSubset4$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    

    fish.int.4 <- with(fishSubset4, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution5, ny=input$resolution5*y.ratio))
    fish.int.melt.4 <- melt(fish.int.4$z, na.rm=TRUE)
    colnames(fish.int.melt.4) <- c("x", "y", "z")
    
    fish.int.melt.4$x <- fish.int.4$x[fish.int.melt.4$x]
    fish.int.melt.4$y <- fish.int.4$y[fish.int.melt.4$y]
    
    fish.int.melt.4$altz <- transform_0_1(fish.int.melt.4$z)
    
    fish.int.melt.4$z <- ifelse(fish.int.melt.4$z < 0, 0, fish.int.melt.4$z)
    
    fish.int.melt.4[is.na(fish.int.melt.4)] <- 0
    if(input$scale5==TRUE){fish.int.melt.4$z <- scale(fish.int.melt.4$z)}

    
    fish.int.melt.4
    

    
})

interpSplit5five <- reactive({
    
    fishImport <- xrfKReactive()
    
   
   #fishSubset5 <- fishImport %>% filter(Line==input$fiveline5 & Element==input$fiveelement5)
    fishSubset5 <- fishImport[,c("x", "y", paste0(input$fiveelement5, ".", input$fiveline5))]
    colnames(fishSubset5)[3] <- "Net"

    
    ###Europe
    xmin <- min(fishSubset5$x)
    xmax <- max(fishSubset5$x)
    ymin <- min(fishSubset5$y)
    ymax <- max(fishSubset5$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    

    fish.int.5 <- with(fishSubset5, interp(x=x, y=y, z=Net, duplicate="user", dupfun="mean", nx=input$resolution5, ny=input$resolution5*y.ratio))
    fish.int.melt.5 <- melt(fish.int.5$z, na.rm=TRUE)
    colnames(fish.int.melt.5) <- c("x", "y", "z")
    
    fish.int.melt.5$x <- fish.int.5$x[fish.int.melt.5$x]
    fish.int.melt.5$y <- fish.int.5$y[fish.int.melt.5$y]
    
    fish.int.melt.5$altz <- transform_0_1(fish.int.melt.5$z)
    
    fish.int.melt.5$z <- ifelse(fish.int.melt.5$z < 0, 0, fish.int.melt.5$z)
    
    fish.int.melt.5[is.na(fish.int.melt.5)] <- 0
    if(input$scale5==TRUE){fish.int.melt.5$z <- scale(fish.int.melt.5$z)}

    
    fish.int.melt.5

    
})

dataSplit5 <- reactive({
    
    fish.int.melt.1 <- interpSplit5one()
    
    fish.int.melt.2 <- interpSplit5two()
    
    fish.int.melt.3 <- interpSplit5three()
    
    fish.int.melt.4 <- interpSplit5four()
    
    fish.int.melt.5 <- interpSplit5five()
    
    
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
    fish.merge[complete.cases(fish.merge), ]

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
    coord_equal(xlim = ranges5$x, ylim = ranges5$y, expand = FALSE) +
    guides(alpha=FALSE) +
    scale_x_continuous("X (mm)") +
    scale_y_continuous("Y (mm)") +
    theme_classic()
    
    
    
    spectral.int.map
    
})


output$fiveMap <- renderPlot({
    
    input$actionprocess5

    isolate(plotInputFive())
})

# Float over info
output$hover_info5 <- renderUI({
    
    input$actionprocess5

    isolate(point.table <- dataSplit5())
    
    
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



ratioFrame <- reactive({
    
    spectra.line.table <- xrfKReactive()
    spectra.line.table$None <- rep(1, length(spectra.line.table$x))
    
    spectra.line.table

})



outElementsRatio <- reactive({
    metadata.dat <- ratioFrame()
    
    element.names <- unique(t(as.data.frame(strsplit(colnames(metadata.dat[,3:length(metadata.dat)]), split="[.]")))[,1])
    
    element.names
    
    
})






ratioChooseAElement <- reactive({
    
   outElementsPCA()[2]
   
})



output$inelementratioa <- renderUI({
    selectInput("elementratioa", "Element A", choices=outElementsPCA(), selected=ratioChooseAElement())
})



ratioChooseALine <- reactive({
    
    
    metadata.dat <- ratioFrame()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$elementratioa
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
})

output$inlineratioa <- renderUI({
    
    if(input$elementratioa!="None"){
    selectInput("lineratioa", label=NULL, choices=ratioChooseALine())
    }else{
        p()
    }
})



ratioChooseBElement <- reactive({

        "None"
    
})

output$inelementratiob <- renderUI({
    selectInput("elementratiob", "Element B", choices=outElementsRatio(), selected=ratioChooseBElement())
})

ratioChooseBLine <- reactive({
    
    
    metadata.dat <- ratioFrame()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$elementratiob
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
})


output$inlineratiob <- renderUI({
    
    if(input$elementratiob!="None"){
        selectInput("lineratiob", label=NULL, choices=ratioChooseBLine())
    }else{
        p()
    }
})



ratioChooseCElement <- reactive({

    outElementsPCA()[4]
    
})

output$inelementratioc <- renderUI({
    selectInput("elementratioc", "Element C", choices=outElementsPCA(), selected=ratioChooseCElement())
})


ratioChooseCLine <- reactive({
    
    
    metadata.dat <- ratioFrame()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$elementratioc
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
})


output$inlineratioc <- renderUI({
    
    if(input$elementratioc!="None"){
        selectInput("lineratioc", label=NULL, choices=ratioChooseCLine())
    }else{
        p()
    }
})


ratioChooseDElement <- reactive({

        "None"
        
})


output$inelementratiod <- renderUI({
    selectInput("elementratiod", "Element D", choices=outElementsRatio(), selected=ratioChooseDElement())
})


ratioChooseDLine <- reactive({
    
    metadata.dat <- ratioFrame()
    
    element.is <- gsub("[.].*$", "", colnames(metadata.dat))==input$elementratiod
    
    metadata.small <- metadata.dat[,c(element.is), drop=FALSE]
    
    
    line.names <- unique(t(as.data.frame(strsplit(colnames(metadata.small), split="[.]")))[,2])
    
    line.names
    
})

output$inlineratiod <- renderUI({
    
    if(input$elementratiod!="None"){
        selectInput("lineratiod", label=NULL, choices=ratioChooseDLine())
    }else{
        p()
    }
})






hoverHoldRatio <- reactive({
    
    spectra.line.table <- ratioFrame()
    
    
    
    first.ratio <- spectra.line.table[paste0(input$elementratioa, ".", input$lineratioa)]

        
        
    second.ratio <- if(input$elementratiob!="None") {
            spectra.line.table[paste0(input$elementratiob, ".", input$lineratiob)]
        }else{
            spectra.line.table["None"]
        }
        
    third.ratio <- spectra.line.table[paste0(input$elementratioc, ".", input$lineratioc)]

        
    fourth.ratio <- if(input$elementratiod!="None") {
            spectra.line.table[paste0(input$elementratiod, ".", input$lineratiod)]
        }else{
            spectra.line.table["None"]
        }
        


    
    
    ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, spectra.line.table$Cluster, spectra.line.table$x, spectra.line.table$y)
    colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Cluster", "x", "y"))
    
    
    
    ratio.frame$W <- if(input$elementratiob!="None"){
        ratio.frame[,1]/ratio.frame[,2]
    }else{
        ratio.frame[,1]
    }
    
    ratio.frame$Z <- if(input$elementratiod!="None"){
        ratio.frame[,3]/ratio.frame[,4]
    }else{
        ratio.frame[,3]
    }
    

    
    ratio.frame
    
})



plotInput4 <- reactive({
    ratio.frame <- hoverHoldRatio()
    
    if(input$elementratiob!="None"){ratio.names.x <- c(names(ratio.frame[1]), "/", names(ratio.frame[2]))}
    if(input$elementratiod!="None"){ratio.names.y <- c(names(ratio.frame[3]), "/", names(ratio.frame[4]))}
    
    if(input$elementratiob=="None"){ratio.names.x <- c(names(ratio.frame[1]))}
    if(input$elementratiod=="None"){ratio.names.y <- c(names(ratio.frame[3]))}
    
    ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
    ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
    
    
    cluster.ratio.plot <- qplot(W, Z, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
    geom_point(aes(colour=as.factor(ratio.frame$Cluster), shape=as.factor(ratio.frame$Cluster)), size=input$spotsize2+1) +
    geom_point(colour="grey30", size=input$spotsize2-2) +
    scale_shape_manual("Cluster", values=1:nlevels(as.factor(as.factor(ratio.frame$Cluster)))) +
    scale_colour_discrete("Cluster") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15)) +
    geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
    
    cluster.ratio.ellipse.plot <- qplot(W, Z, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
    stat_ellipse(aes(ratio.frame$W, ratio.frame$Z, colour=as.factor(ratio.frame$Cluster))) +
    geom_point(aes(colour=as.factor(ratio.frame$Cluster), shape=as.factor(ratio.frame$Cluster)), size=input$spotsize2+1) +
    geom_point(colour="grey30", size=input$spotsize2-2) +
    scale_shape_manual("Cluster", values=1:nlevels(as.factor(as.factor(ratio.frame$Cluster)))) +
    scale_colour_discrete("Cluster") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15)) +
    geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
    
    
    
    if (input$elipseplot2==FALSE) {
        cluster.ratio.plot
    } else if (input$elipseplot2==TRUE) {
        cluster.ratio.ellipse.plot
    }
    
})


output$elementratiotimeseries <- renderPlot({
    plotInput4()
    
    
})







output$hover_inforatio <- renderUI({
    
    point.table <- hoverHoldRatio()
    
    hover <- input$plot_hoverratio
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
    p(HTML(paste0("<b> X: </b>", point$x, "<br/>",
    "<b> Y: </b>", point$y, "<br/>",

    
    
    
    )))
    )
})

ratioTerm <- reactive({
    
    ratio.names <- paste(c(c(substr(input$elementratioa, 1,2), "-", substr(input$elementratiob, 1, 2)), "_", c(substr(input$elementratioc,1,2), "-", substr(input$elementratiod,1,2), "_RatioPlot")), collapse="")
    ratio.label <- paste(c(input$projectname, "_", ratio.names), collapse='')
    ratio.label
})

output$downloadPlot4 <- downloadHandler(


filename = function() { paste(ratioTerm(), '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInput4(), device="tiff", compression="lzw", type="cairo",  dpi=300, width=12, height=7)
}
)


 


})

 })




