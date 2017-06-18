library(pbapply)
library(reshape2)
library(TTR)
library(dplyr)
library(ggtern)
library(ggplot2)
library(shiny)





read_csv_x <- function(filename) {
    
    x <- as.numeric(substr(strsplit(filename,"\\X ")[[1]][2], 1, 6))
    
    x

    
}

read_csv_y <- function(filename) {
    
    y <- as.numeric(substr(strsplit(filename,"\\Y ")[[1]][2], 1, 6))
    
    
    y
    
}

read_csv_net <- function(filepath) {
    
    ret <- read.csv(file=filepath, sep=",", header=TRUE)
    element <- ret$Element
    line <- ret$Line
    net <- ret$Net
    background <- ret$Backgr.
    
    
    parsed.file <- data.frame(element, line, net, background)
    colnames(parsed.file) <- c( "Element", "Line", "Net", "Background")
    
    parsed.file

    
    
}

add_x <- function(netcounts, x) {
    
    netcounts$x <- x
    
    netcounts
    
}

add_y <- function(netcounts, y) {
    
    netcounts$y <- y
    
    netcounts
    
}

read_csv_all <- function(files){
    
    file.name <- files$inName
    file.path <- files$inPath
    
    
    x <- as.numeric(substr(strsplit(file.name,"\\X ")[[1]][2], 1, 6))
    y <- as.numeric(substr(strsplit(file.name,"\\Y ")[[1]][2], 1, 6))
    

    ret <- read.csv(file=file.path, sep=",", header=TRUE)
    element <- ret$Element
    line <- ret$Line
    net <- ret$Net
    background <- ret$Backgr.
    
    x.vector <- rep(x, length(element))
    y.vector <- rep(y, length(element))
    
    parsed.file <- data.frame(x.vector, y.vector, element, line, net, background)
    colnames(parsed.file) <- c("x", "y", "Element", "Line", "Net", "Background")
    
    parsed.file
    
}



range01 <- function(x){(x-min(x))/(max(x)-min(x))}

extract.with.context <- function(x, rows, after = 0, before = 0) {
    
    match.idx  <- which(rownames(x) %in% rows)
    span       <- seq(from = -before, to = after)
    extend.idx <- c(outer(match.idx, span, `+`))
    extend.idx <- Filter(function(i) i > 0 & i <= nrow(x), extend.idx)
    extend.idx <- sort(unique(extend.idx))
    
    return(x[extend.idx, , drop = FALSE])
}


read_csv_filename_x <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.res <- as.numeric(as.vector(ret$V2[18]))/1000
    return.chan.counts <-as.numeric(as.vector(ret$V1[22:2069]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}

read_csv_filename_y <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.live.time <- as.numeric(as.vector(ret$V2[10]))
    return.counts <- as.numeric(as.vector(ret$V2[22:2069]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}

file.0 <- function(file) {
    if (length(file) > 0)
    {
    return(file)
    }else{
        return(levels(file))
    }
}

is.0 <- function(cps, file) {
    file.0 <- function(file) {
        if (length(file) > 0)
        {
            return(file)
        }else{
            return(levels(file))
        }
    }
    if (length(cps) > 0)
    {
        hope <-data.frame(cps, file.0(file))
        return(hope)
    } else {
        empty <- rep(0, length(file.0(file)))
        framed <- data.frame(empty, file.0(file))
        return(framed)
    }
}


dt_options <- reactive({
    # dynamically create options for `aoColumns` depending on how many columns are selected.
    toggles <- lapply(1:length(input$show_vars), function(x) list(bSearchable = F))
    # for `species` columns
    toggles[[length(toggles) + 1]] <- list(bSearchable = T)
    
    list(
    aoColumns = toggles,
    bFilter = 1, bSortClasses = 1,
    aLengthMenu = list(c(10,25,50, -1), list('10','25', '50', 'Todas')),
    iDisplayLength = 10
    )
})

ifrm <- function(obj, env = globalenv()) {
    obj <- deparse(substitute(obj))
    if(exists(obj, envir = env)) {
        rm(list = obj, envir = env)
    }
}





