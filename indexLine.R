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

    if(ansY[length(ansY)] != arr[length(arr)]) {
        ansX<-c(ansX, idx[length(arr)]);
        ansY<-c(ansY, arr[length(arr)]);
    }
    data.frame(x=ansX, y=ansY);
};

dateLine <- function(date, start, min, max) {
    if(start < date) {
        lines(c(date, date), c(min, max), col="blue");
    }
};

slope <- function(frame) {
    x<-frame[[1]];
    y<-frame[[2]];

    ans <- c();
    for(i in 2:length(y)) {
        ans[i-1] <- round((y[i]-y[i-1]) / as.numeric(x[i] - x[i-1]), digits=1);
    }
    ans[length(y)] = ans[length(ans)];
    
    data.frame(x=x, y=ans);
}

idxOf <- function(l) {
    ans <- 0;
    for(i in 1:length(l)) {
        if(l[i]) { ans<-i; break; }
    }
    ans;
}

fitIn <- function(tx, ty, vx) {
    ans <- c(ty[1]);

    for(i in 2:length(ty)) {
        base <- ty[i-1];
        dx <- as.numeric(tx[i]-tx[i-1]);
        dy <- ty[i] - base;
        xs <- vx[vx <= tx[i] & vx > tx[i-1]];
        step <- dy / dx;
        for(j in 1:length(xs)) {
            y <- base + step * as.numeric(xs[j]-tx[i-1]);
            ans <- append(ans, y);
        }
    }

    data.frame(x=vx, y=ans);
};

scaleToMax <- function(frame, max) {
    arr <- frame[[2]];
    fmax <- max(arr); scale <- max/fmax;
    data.frame(frame[[1]], arr*scale);
};

library(ggplot2)

loc <- Sys.setlocale("LC_TIME", "C");
ans <- data.frame();

for(i in commandArgs(trailingOnly=TRUE)) {
    data <- read.csv(i);
    data$Date <- as.Date(data$Date, "%b %d, %Y");
    data$Price <- as.numeric(sub(",", "", data$Price));

    data <- data[order(data$Date),];

    dates <- data$Date;
    prices <- data$Price;

    min <- min(prices);
    max <- max(prices);
    avg <- mean(prices);
    age <- as.numeric(dates[length(dates)] - dates[1])/365;
    ans <- rbind(ans, c(sub("-.+", "", i),
                        round(age, 2), min, max, round(avg, 2),
                        round(abs(100*(max-min)/avg/age), 2),
                        round(prices[length(prices)] - avg, 2)), stringsAsFactors=FALSE);

    amp <- max-min;
    trend <- trendCurve(dates,prices);
    plot(dates, prices, main=i);
    lines(trend, col="red", lwd=4);
    dateLine(as.Date("2008-06-01", "%Y-%m-%d"), dates[1], min, max);
    dateLine(as.Date("2018-01-01", "%Y-%m-%d"), dates[1], min, max);
    dateLine(as.Date("2020-03-01", "%Y-%m-%d"), dates[1], min, max);
    dateLine(as.Date("2022-02-26", "%Y-%m-%d"), dates[1], min, max);

    data <- cbind(data, Slope=slope(scaleToMax(fitIn(trend$x, trend$y, data$Date),1000))$y);
    write.csv(data, sprintf("bd/%s", i));
}

names(ans) <- c("ticker", "age", "min", "max", "mean", "change(%)", "last");
print(ans[order(as.numeric(ans$last)),]);
