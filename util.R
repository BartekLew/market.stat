last <- function(arr) { arr[length(arr)]; }

#Show chart for given etf.
chart <- function(frame, name="", meanWindow=nrow(frame)%/%10,
                  from=frame[1,"date"], to=frame[nrow(frame),"date"]) {
    f2 <- frame[frame$date >= from & frame$date <= to,];

    min<-min(f2$price);
    max<-max(f2$price);
    means <- data.frame(frame$date, mean=windowOp(frame, meanWindow, rowOp(mean, "price")));

    trend <- trendCurve(frame$date,frame$price);

    plot(x=f2$date, y=f2$price, xlab="Date", ylab="Price", main=name);
    lines(trend, col="red", lwd=4);
    lines(means, col="blue", lwd=4);
    dateLine(as.Date("2008-06-01", "%Y-%m-%d"), from, to, min, max);
    dateLine(as.Date("2018-01-01", "%Y-%m-%d"), from, to, min, max);
    dateLine(as.Date("2020-03-01", "%Y-%m-%d"), from, to, min, max);
    dateLine(as.Date("2022-02-26", "%Y-%m-%d"), from, to, min, max);
}

# Moving window operation
windowOp <- function(frame, size, action) {
    if(nrow(frame) < size*2) {
        stop("windowOp must be applied on frame of at least 2*size rows!");
    }

    ans <- c();

    for(i in 1:size) {
        ans <- append(ans, action(frame[1:i,]));
    }

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

    vx <- vx[vx >= tx[1] & vx <= tx[length(tx)]];
    ans <- data.frame();
    if(tx[1] == vx[1]) ans<-rbind(ans, list(tx[1], ty[1]));

    for(i in 2:length(ty)) {
        base <- ty[i-1];
        dx <- as.numeric(tx[i]-tx[i-1]);
        dy <- ty[i] - base;
        xs <- vx[vx <= tx[i] & vx > tx[i-1]];
        step <- dy / dx;
        ans <- rbind(ans, data.frame(xs, base + step * as.numeric(xs-tx[i-1])));
    }

    names(ans)<- c("x", "y");
    ans;
};

# Function to calculate price change corelations between noted assets.
corelations <- function (frame) {
    rows <- data.frame();
    for(t1 in frame$ticker) {
        row <- c();
        i<-0;
        for(t2 in frame$ticker) {
            if(t1 == t2) {
                row <- append(row, NA);
            } else {
                a <- get(t1);
                b <- get(t2);
                if(a$date[1] > b$date[1]) {
                    c <- a;
                    a <- b;
                    b <- c;
                }
                row[i] <- NA;
                try({fitted <- fitIn(a$date, a$price, b$date);
                     row[i] <- cor(fitted$y, b[b$date<=last(fitted$x),]$price);});
            }
        }
        rows <- rbind(rows, row);
    }
    names(rows) <- frame$ticker;
    row.names(rows) <- frame$ticker;
    rows;
}

