

list.of.packages <- c("XML","tidyverse", "data.table", "janitor")
new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(XML)
cleanGoogleTable <- function(dat, table=1, skip=0, ncols=NA, nrows=-1, header=TRUE, dropFirstCol=NA){
    if(!is.data.frame(dat)){
        dat <- dat[[table]]
    }
    if(is.na(dropFirstCol)) {
        firstCol <- na.omit(dat[[1]])
        if(all(firstCol == ".") || all(firstCol== as.character(seq_along(firstCol)))) {
            dat <- dat[, -1]
        }
    } else if(dropFirstCol) {
        dat <- dat[, -1]
    }
    if(skip > 0){
        dat <- dat[-seq_len(skip), ]
    }
    if(nrow(dat) == 1) return(dat)
    if(nrow(dat) >= 2){
        if(all(is.na(dat[2, ]))) dat <- dat[-2, ]
    }
    if(header && nrow(dat) > 1){
        header <- as.character(dat[1, ])
        names(dat) <- header
        dat <- dat[-1, ]
    }
    # Keep only desired columns
    if(!is.na(ncols)){
        ncols <- min(ncols, ncol(dat))
        dat <- dat[, seq_len(ncols)]
    }
    # Keep only desired rows
    if(nrows > 0){
        nrows <- min(nrows, nrow(dat))
        dat <- dat[seq_len(nrows), ]
    }
    # Rename rows
    rownames(dat) <- seq_len(nrow(dat))
    dat
}



readGoogleSheet <- function(url, na.string="", header=TRUE){
    stopifnot(require(XML))
    # Suppress warnings because Google docs seems to have incomplete final line
    suppressWarnings({
        doc <- paste(readLines(url), collapse=" ")
    })
    if(nchar(doc) == 0) stop("No content found")
    htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
    ret <- readHTMLTable(htmlTable, header=header, stringsAsFactors=FALSE, as.data.frame=TRUE)
    lapply(ret, function(x){ x[ x == na.string] <- NA; x})
}


nms_clean <- function(data_set){
    nms_old <- names(data_set)
    nms_new <- nms_old %>% tolower() 
    nms_new <- gsub("\\s", "_", nms_new)
    nms_new <- gsub("\\.$", "", nms_new)
    nms_new <- gsub("\\(|\\)|%", "", nms_new)
    nms_new <- gsub("/", "", nms_new)
    nms_new <- gsub("-", "_", nms_new)
    setDT(data_set)
    setnames(data_set, nms_old, nms_new)
    data_set
}

nms_clean_vec <- function(nms_old){
    nms_new <- nms_old %>% tolower() 
    nms_new <- gsub("\\s", "_", nms_new)
    nms_new <- gsub("\\.$", "", nms_new)
    nms_new <- gsub("\\(|\\)|%", "", nms_new)
    nms_new <- gsub("/", "", nms_new)
    nms_new <- gsub("-|\\.", "_", nms_new)
    nms_new <- gsub("_{2,5}", "_", nms_new)
    nms_new
}

#targets = 1:ncol(df)
data_table <- function(df){
    library(DT)
    datatable(df,
              rownames = FALSE,
              style = "bootstrap4", class = 'cell-border stripe',
              options = list(scrollX = TRUE,
                             columnDefs = list(list(className = 'dt-center'))
              ) 
    )%>%
        formatStyle(names(df), textAlign = 'center')
}


char_to_num <- function(x){
    x = str_replace_all(x, ",|\\s", "")
    x = as.numeric(x)
}
