#!/usr/bin/env Rscript

warnings();

# if there is existing session file, load all data.
if(file.exists(".RData")) {
    try(load(".RData"));
}

source("util.R");

id <- function(x) x;
loc <- Sys.setlocale("LC_TIME", "C");

###############
#Util functions
###############

# For given idx and value coordinate vectors, return trend curve levels
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

# append a line on particular date in current plot
dateLine <- function(date, start, min, max) {
    if(start < date) {
        lines(c(date, date), c(min, max), col="blue");
    }
};

# For given (x,y) frame, return data frame with slope of curve
# instead of original value.
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

# Return index of first TRUE in vector, otherwise 0.
idxOf <- function(l) {
    ans <- 0;
    for(i in 1:length(l)) {
        if(l[i]) { ans<-i; break; }
    }
    ans;
}

# tx and ty are original function. Create frame of function
# of vx (superset of tx), that return from ty if present, or 
# value estimated using linear function that have values in
# preceding and following ty
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

# scales second column of frame so that maximum value
# is max, and other are proportionaly smaller.
scaleToMax <- function(frame, max) {
    arr <- frame[[2]];
    fmax <- max(arr); scale <- max/fmax;
    data.frame(frame[[1]], arr*scale);
};


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

etfs <- data.frame();
for(f in commandArgs(trailingOnly=TRUE)) {
    etfs <- rbind(etfs, importETF(f), stringsAsFactors=FALSE);
}

names(etfs) <- c("name", "ticker", "age", "change(%)", "last");

print(etfs[order(as.numeric(etfs$last)),]);

save(list=ls(), file=".RData");
