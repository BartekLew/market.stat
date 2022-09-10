
id <- function(x) x;
loc <- Sys.setlocale("LC_TIME", "C");
last <- function(arr) { arr[length(arr)]; }

source('chaos.R');

#Show chart for given etf.
chart <- function(frame, name="", meanWindow=nrow(frame)%/%10,
                  from=frame[1,"date"], to=frame[nrow(frame),"date"],
                  attractorWindow=30, showVols=F) {
    f2 <- frame[frame$date >= from & frame$date <= to,];

    mini<-min(f2$price);
    maxi<-max(f2$price);
    means <- data.frame(frame$date[(meanWindow+1):nrow(frame)],
                        mean=windowOp(frame, meanWindow,
                        rowOp(mean, "price")));

    trend <- trendCurve(frame$date,frame$price);

    plot(x=f2$date, y=f2$high, xlab="Date", ylab="Price", main=name, type='l', col="#a0a0a0");
    lines(x=f2$date, y=f2$low, col="#a0a0a0");

    meansFL <- data.frame(date=frame$date[(attractorWindow+1):nrow(frame)],
                        mean=windowOp(frame, attractorWindow,
                        rowOp(mean, "price")));
    freqLines(meansFL, as.Date(from), as.Date(to), step=3*365);
    lines(x=f2$date, y=f2$price, xlab="Date", ylab="Price", main=name, type='l');
    
    lines(trend, col="red", lwd=4);
    lines(means, col="blue", lwd=4);

    dateLine(as.Date("2008-06-01", "%Y-%m-%d"), from, to, mini, maxi);
    dateLine(as.Date("2018-01-01", "%Y-%m-%d"), from, to, mini, maxi);
    dateLine(as.Date("2020-03-01", "%Y-%m-%d"), from, to, mini, maxi);
    dateLine(as.Date("2022-02-26", "%Y-%m-%d"), from, to, mini, maxi);

    if("vol" %in% colnames(f2) & showVols) {
        volDF <- f2[!is.na(f2$vol),];
        ampV <- max(volDF$vol) - min(volDF$vol);
        vols <- windowOp(volDF, 7, function(df) mean(df$vol));
        lines(volDF$date[8:nrow(volDF)], ((vols - min(vols))/ampV * (maxi-mini)) + mini, type='l', col='purple');
    }
}

shortChart <- function(frame, name="", meanWindow=nrow(frame)%/%10,
                  from=frame[1,"date"], to=frame[nrow(frame),"date"],
                  attractorWindow=30, showVols=F) {
    f2 <- frame[frame$date >= from & frame$date <= to,];

    mini<-min(f2$price);
    maxi<-max(f2$price);
    means <- data.frame((meanWindow+1):nrow(frame),
                        mean=windowOp(frame, meanWindow,
                        rowOp(mean, "price")));

    plot(x=1:nrow(f2), y=f2$price, xlab="Date", ylab="Price", main=name, type='b', col="black");
    lines(x=1:nrow(f2), y=f2$high, col="#4040a0");
    lines(x=1:nrow(f2), y=f2$low, col="#4040a0");

    lines(means, col="blue", lwd=4);

    for(i in 1:(nrow(f2)/34)) {
        x <- (i*34);
        lines(x=c(x, x), y=c(mini, maxi), col="#a0a0a0");
    }
}

atractors <- function(arr, resolution=100) {
    a <- min(arr);
    b <- max(arr);
    step <- b-a / resolution;
    x <- sapply(1:resolution, function(i) a + i * step);
    y <- sapply(x, function(i) length(arr[abs(arr - i)<step]));
    data.frame(x=x, y=y);
}

# Moving window operation
windowOp <- function(frame, size, action) {
    if(nrow(frame) < size*2) {
        stop("windowOp must be applied on frame of at least 2*size rows!");
    }

    ans <- c();

    for(i in (size+1):nrow(frame)) {
        ans <- append(ans, action(frame[(i-size):i,]));
    }
    
    ans;
}

# Accept a function of vector and row name. Function of frame
# that  apply source function on given row of that frame.
rowOp <- function(f, row) {
    function(frame) { f(frame[[row]]) };
}

# scales second column of frame so that maximum value
# is max, and other are proportionaly smaller.
scaleToMax <- function(frame, max) {
    arr <- frame[[2]];
    fmax <- max(arr); scale <- max/fmax;
    data.frame(frame[[1]], arr*scale);
};

# append a line on particular date in current plot
dateLine <- function(date, start, end, min, max) {
    if(start < date & end > date) {
        lines(c(date, date), c(min, max), col="blue");
    }
};

# For given idx and value coordinate vectors, return trend curve levels
trendCurve <- function (idx, arr, minLen=365) {
    ansX <- c(idx[1]); ansY <- c(arr[1]); alen <- 1;
    dy <- 0; lastI <- 0;

    for(i in 2:length(arr)) {
        y <- arr[i]; Dy <- y - ansY[alen]; 
        if(alen > 1 && sign(Dy) == sign(dy)) {
            if(abs(Dy) > 0) {
                dy = Dy;
                ansX[alen] = idx[i];
                ansY[alen] = y;
            }
        } else if (abs(Dy) >= abs(ansY[alen]*0.2) & i - lastI > minLen) {
            ansX = c(ansX, idx[i]);
            ansY = c(ansY, y);
            lastI <- i;
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

# For given (x,y) frame, return data frame with slope of curve
# instead of original value.
slope <- function(frame, digits=1) {
    x<-frame[[1]];
    y<-frame[[2]];

    ans <- c();
    for(i in 2:length(y)) {
        ans[i-1] <- round((y[i]-y[i-1]) / as.numeric(x[i] - x[i-1]), digits=digits);
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
    vx <- vx[vx >= tx[1] & vx <= tx[length(tx)]];
    ans <- data.frame();

    if(vx[1] == tx[1]) { ans <- data.frame(x=c(vx[1]), y=(ty[1]), p=0); }
    for(i in 2:length(ty)) {
        base <- ty[i-1];
        dx <- as.numeric(tx[i]-tx[i-1]);
        dy <- ty[i] - base;
        xs <- vx[vx <= tx[i] & vx > tx[i-1]];
        step <- dy / dx;
        ans <- rbind(ans, data.frame(x = c(as.Date(xs)),
                                     y = c(base + step * as.numeric(xs-tx[i-1])),
                                     p = seq(0, 1, by=1/(length(xs)-1))));
    }

    ans;
};

# Function to calculate price change corelations between noted assets.
corelations <- function (frame, varname="cors") {
    dim <- nrow(frame);
    rows <- sapply(1:dim, function(x) 1:dim);
    names(rows) <- frame$ticker;
    row.names(rows) <- frame$ticker;

    for(t1 in 1:dim) {
        for(t2 in 1:dim) {
            if(t1 == t2) {
                rows[t1,t2] <- NA;
            } else {
                a <- get(frame$ticker[t1]);
                b <- get(frame$ticker[t2]);
                if(a$date[1] > b$date[1]) { c<-a;a<-b;b<-c; }
                rows[t1,t2] <- NA;
                try({fitted <- fitIn(a$date, a$price, b$date);
                     rows[t1,t2] <- cor(fitted$y, b[b$date<=last(fitted$x),]$price);});
                assign(varname, rows, envir=.GlobalEnv);
                cat(frame$ticker[t1], frame$ticker[t2], rows[t1,t2], "\n");
            }
        }
    }
    rows
}

# convenient mapping frames
mapf <- function(frame, fun, cols=1:length(frame)) {
    sapply(1:nrow(frame), function(i) {
                             row <- frame[i,cols];
                             fun(row);
                          });
}

# Raw asset price history is not very useful for my analisys.
# I want to express them in term of standard deviations difference
# from mean.
norm <- function(vec) {
    mean <- mean(vec);
    sd <- sd(vec);
    vapply(vec, function(x) (x - mean) / sd, 0);
}

# for given array of numeric or dates and val, return how it relates
# to closest points in arr. 0 means val is element of arr; 0.5 means
# that it's exactly in the middle between two points of arr and 0.99
# means that it's almost equal, but smaller then some of arr points.
prog <- function(arr, val) {
    if(val < arr[1] | val > last(arr)) 
        stop("value in prog out of bounds");

}

# I want to transform raw asset information into statistics that could
# be useful to predict future movements.
tradingOdds <- function(frame, sampleLen=40) {
    tc <- trendCurve(frame$date, frame$price);
    mean10=windowOp(frame, 10, rowOp(mean, "price"));
    sdev10=windowOp(frame, 10, rowOp(sd, "price"));
    mean30=windowOp(frame, 30, rowOp(mean, "price"));
    sdev30=windowOp(frame, 30, rowOp(sd, "price"));
    meanYr=windowOp(frame, 365, rowOp(mean, "price"));
    sdevYr=windowOp(frame, 365, rowOp(sd, "price"));
    mean=mean(frame$price);
    sdev=sd(frame$price);

    #Let's gather some statistical facts about each day
    #trendSlope is the only one that says about future
    #and which is the most relevant.
    statdat<- data.frame(date=frame$date, daysd=mapf(frame, function(r) max(r) - min(r), cols=2:5),
                         close=mapf(frame, function(r) {
                                                min <- min(r); max <- max(r);
                                                if(min == max) 0 
                                                else (r$price - min) / (max-min)
                                           },
                                           cols=2:5),
                         pos=(frame$price - mean)/sdev,
                         pos10=(frame$price - mean10)/sdev10,
                         pos30=(frame$price - mean30)/sdev30,
                         posYr=(frame$price - meanYr)/sdevYr,
                         trendSlope=frame$trend);

    # Now, let's look at distributions in small groups
    statdat <- groupMap(statdat, denser(tc$x, sampleLen), fun=function(group,i,j) {
                                            data.frame(start=group$date[1],
                                                       end=group$date[nrow(group)],
                                                       mean10=mean(group$pos10),
                                                       mean30=mean(group$pos30),
                                                       meanYr=mean(group$posYr),
                                                       trendProgress=prog(tc$x,group$date[nrow(group)]),
                                                       slope=group$trend[1])
                                            });

    # Finally, I'm interested in probabilities that each mean positive 
    # come with positive slope and negative come with negative.
    pp <- nrow(statdat[statdat$mean10 >= 0 & statdat$slope >= 0,]);
    pn <- nrow(statdat[statdat$mean10 >= 0 & statdat$slope <= 0,]);
    np <- nrow(statdat[statdat$mean10 <= 0 & statdat$slope >= 0,]);
    nn <- nrow(statdat[statdat$mean10 <= 0 & statdat$slope <= 0,]);
    
    data.frame(positive = pp/(pp+pn), negative = nn/(nn+np));
}

# Take a data frame and then key=ranges pair. key is name of column
# of frame used as index. ranges is list of key values of key column
# that are used to group frame values in distinct frames. Run fun
# on each frame and return list/frame of values.
groupMap <- function(frame, ranges, key="date", fun=NA) {
    ans<-data.frame();
    for(i in 2:length(ranges)) {
        if(ranges[i] - ranges[i-1]>2)
            group <- frame[frame[,key] >= ranges[i-1] & frame[,key] < ranges[i],];
            if(nrow(group)>0)
                try(ans<-rbind(ans, fun(group, i, length(ranges))));
    }

    ans;
}

# accepts vector of numbers representing ranges. Add more points to
# it, so that each range is of given size.
denser <- function(range, step) {
    c(flatMap(2:length(range), function (i) {
                                width <- range[i] - range[i-1];
                                remainder <- as.numeric(width) %% step;
                                steps <- as.numeric(width) %/% step - 1;
                                c(range[i-1], range[i-1]+step+remainder,
                                  range[i-1] + remainder + step * 2:steps);
                             }),
      last(range));
}

flatMap <- function(vals, fun) {
    ans <- c();
    for(i in 1:length(vals)) {
        ans <- append(ans, fun(vals[i]));
    }

    ans;
}

trendCol <- function(idxs, vals) {
    trend <- trendCurve(idxs,vals);
    dTrend <- slope(scaleToMax(fitIn(trend$x, trend$y, idxs),1000))$y;
}

# Make dataframe using vector and function building particular rows.
buildFrame <- function(arr, fun) {
    acc <- fun(arr[1]);
    if(length(arr) > 1) {
        for(i in 2:length(arr)) {
            acc <- rbind(acc, fun(arr[i]));
        }
    }

    acc;
}

firstAndLast <- function(arr) {
    c(arr[1], arr[length(arr)]);
}

assetRow <- function(ticker, description, data) {
    min<-min(data$price);
    max<-max(data$price);
    avg<-mean(data$price);

    list(description, ticker,
            age<-round(as.numeric(last(data$date) - data$date[1])/365,2),
            round(abs(100*(max-min)/avg/age), 2),
            round(data$price[length(data$price)] - avg, 2));
}

loadAsset <- function(ticker, description, file, dateFmt="%b %d, %Y") {
    data <- read.csv(file);
    data$Date <- as.Date(data$Date, dateFmt);
    data <- data[,1:5];
    names(data) <- c('date', 'price', 'open', 'high', 'low');
    assign(ticker, data, envir=.GlobalEnv);
    assetRow(ticker, description, data);
}

rangeLines <- function(df, meanWindow=nrow(df)/10, color="green", column="price") {
    step <- nrow(df)/pieces;
    for(i in 0:pieces) {
        lim <- min((i+1)*step, nrow(df))
        piece <- df[(i*step):lim,];
        sd <- sd(piece[,column]);
        mean <- mean(piece[,column]);
        lines(piece$date[c(1,nrow(piece))], c(mean,mean), col=color);
        lines(piece$date[c(1,nrow(piece))], c(mean+sd,mean+sd), col=color);
        lines(piece$date[c(1,nrow(piece))], c(mean-sd,mean-sd), col=color);
    }
}

shortDat <- function(file) {
    df <- read.csv(file, stringsAsFactors=F);
    names(df) <- c('date', 'price', 'open', 'high', 'low', 'vol');
    df$date <- as.POSIXct(df$date, "%Y-%m-%d %H:%M");
    df[order(df$date),];
}
