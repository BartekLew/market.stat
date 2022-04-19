#!/usr/bin/env Rscript

# Table with indexes of all known instruments
etfs <- data.frame(); #don't override this table if exists

# if there is existing session file, load all data.
if(file.exists(".RData")) {
    try(load(".RData"));
}

source("util.R");

id <- function(x) x;
loc <- Sys.setlocale("LC_TIME", "C");


#####################
# Importing functions
#####################

csvTab <- function(name, transforms=c(), colNames=c(),
                   ret=function(d, name, ticker) d) {
    data <- read.csv(name);

    if(length(colNames)>0) {
        names(data) <- colNames;
    }
    

    if(class(fields <- names(transforms)) != "NULL") {
        for(fld in fields) {
            data[[fld]] <- transforms[[fld]](data[[fld]]);
        }
    } else {
        for(i in 1:length(transforms)) {
            data[[i]] <- transforms[i](data[[i]]);
        }
    }

    name <- sub('^.*/', '', name);
    name <- sub('\\.\\w+$', "", name);
    ticker <- sub('-.+$', "", name);
    name <- sub('[^-]+-', "", name);

    ret(data, name, ticker);
}

strToPrice <- function(x) {
    x <- gsub("[\\$£,]", "", x);

    as.numeric(x);
}

importETF <- function(file) {
    csvTab(file,transforms=c(price=strToPrice, open=strToPrice, high=strToPrice, low=strToPrice,
                             change=function(x) as.numeric(sub("%", "", x)),
                             date=function(x) as.Date(x, "%b %d, %Y")),
                colNames=c("date", "price", "open", "high", "low", "volume", "change"),
                ret=function(x, name, ticker) {
                    x<-x[order(x$date),];

                    min<-min(x$price);
                    max<-max(x$price);
                    avg<-mean(x$price);

                    trend <- trendCurve(x$date,x$price);
                    dTrend <- slope(scaleToMax(fitIn(trend$x, trend$y, x$date),1000))$y;

                    x <- cbind(x, trend=dTrend);
                    assign(ticker, x, envir=.GlobalEnv);

                    list(name, ticker,
                         age<-round(as.numeric(last(x$date) - x$date[1])/365,2),
                         round(abs(100*(max-min)/avg/age), 2),
                         round(x$price[length(x$price)] - avg, 2));
                });
};

for(f in commandArgs(trailingOnly=TRUE)) {
    etfs <- rbind(etfs, importETF(f), stringsAsFactors=FALSE);
}

names(etfs) <- c("name", "ticker", "age", "change(%)", "last");

print(etfs[order(as.numeric(etfs$last)),]);

save(list=ls(), file=".RData");
