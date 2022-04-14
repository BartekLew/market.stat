#!/usr/bin/env Rscript

trendCurve <- function (idx, arr) {
    ansX <- c(idx[1]); ansY <- c(arr[1]); alen <- 1;
    dy <- 0;

    for(i in 2:length(arr)) {
        y <- arr[i]; Dy <- y - ansY[alen]; 
        if(alen > 1 && sign(Dy) == sign(dy)) {
            if(abs(Dy) > 0) {
                dy = Dy;
                ansX[alen] = idx[i];
                ansY[alen] = y;
            }
        } else if (abs(Dy) >= abs(ansY[alen]*0.2)) {
            ansX = c(ansX, idx[i]);
            ansY = c(ansY, y);
            alen = alen + 1;
            dy = Dy; 
        }
    }

    ansX<-c(ansX, idx[length(arr)]);
    ansY<-c(ansY, arr[length(arr)]);
    data.frame(x=ansX, y=ansY);
};

dateLine <- function(date, start, min, max) {
    if(start < date) {
        lines(c(date, date), c(min, max), col="blue");
    }
};

library(ggplot2)

loc <- Sys.setlocale("LC_TIME", "C");
ans <- data.frame();

for(i in commandArgs(trailingOnly=TRUE)) {
    data <- read.csv(i);
    data$Date <- as.Date(data$Date, "%b %d, %Y");
    data <- data[order(data$Date),];

    dates <- data$Date;
    prices <- as.numeric(sub(",", "", data$Price));

    min <- min(prices);
    max <- max(prices);
    avg <- mean(prices);
    age <- as.numeric(dates[length(dates)] - dates[1])/365;
    ans <- rbind(ans, c(sub("-.+", "", i),
                        round(age, 2), min, max, round(avg, 2),
                        round(abs(100*(max-min)/avg/age), 2),
                        round(prices[length(prices)] - avg, 2)), stringsAsFactors=FALSE);

    amp <- max-min;
    plot(dates, prices, main=i);
    lines(trendCurve(dates, prices), col="red", lwd=4);
    dateLine(as.Date("2008-06-01", "%Y-%m-%d"), dates[1], min, max);
    dateLine(as.Date("2018-01-01", "%Y-%m-%d"), dates[1], min, max);
    dateLine(as.Date("2020-03-01", "%Y-%m-%d"), dates[1], min, max);
    dateLine(as.Date("2022-02-26", "%Y-%m-%d"), dates[1], min, max);
}

names(ans) <- c("ticker", "age", "min", "max", "mean", "change(%)", "last");
print(ans[order(as.numeric(ans$last)),]);
