source('util.R');
source('changeStat.R');

readXtbDat <- function(ticker) {
    glob <- paste("xtb/", ticker, "*csv", sep="");
    fname <- Sys.glob(glob)[1];
    print(fname);

    symdat <- read.csv(fname);
    if(nrow(symdat) > 0) {
        symdat$date <- as.Date(symdat$date, "%Y-%m-%d");
        for(col in c('price', 'open', 'high', 'low', 'vol')) 
            symdat[,col] <- as.numeric(symdat[,col]);
    }

    symdat;
}

loadXtb <- function(ticker, window=30) {
    df <- statCols(readXtbDat(ticker), window=window);
}

noisePlot <- function(df, len=500) {
    plot(tail(df$relprice, n=len), type='l');
}

gradeAssets <- function(fun=stdGrade, allSym=read.csv('xtb.csv')) {
    newcols <- fun(allSym$symbol[1]);
    for(i in 2:nrow(allSym)) {
        r <- fun(allSym$symbol[i]);
        newcols <- rbind(newcols, r);
    }

    cbind(allSym[,c('symbol', 'description')], newcols);
}

stdGrade <- function(sym) {
    try(df <- readXtbDat(sym));
    len <- nrow(df);

    if(length(len) == 0 || len < 180) {
        data.frame(lastPos = NA, posDiff = NA, ampl = NA, relamp=NA, diff3y = NA, diffyr = NA, diff30 = NA, diff15 = NA, diff5 = NA);
    } else {
        if(len >= 360) {
            diffyr <- last(df$price) - df$price[len-360];
        } else {
            diffyr <- 0;
        }
        if(len > 1080) {
            diff3yr <- last(df$price) - df$price[len-1080];
        } else {
            diff3yr <- 0;
        }
        df <- statCols(df[-180:0+nrow(df),], window=15);
        len <- nrow(df);
        mip <- min(df$relprice);
        map <- max(df$relprice);
        meanp <- mean(df$price);
        meanps <- mean(df$price[-30:0 + 180]);
        lastpos <- (last(df$relprice) - mip) / (map-mip);
        lastdiff <- lastpos - ((df$relprice[nrow(df)-1] - mip) / (map-mip));
        data.frame(lastPos = lastpos,
                posDiff = lastdiff,
                ampl   = (max(df$high) - min(df$low)) / meanp,
                relamp = (max(df$relprice) - min(df$relprice)),
                diff3y = diff3yr / meanp,
                diffyr = diffyr / meanp,
                diff30 = (last(df$price) - df$price[len-30]) / meanps,
                diff15 = (last(df$price) - df$price[len-15]) / meanps,
                diff5  = (last(df$price) - df$price[len-5]) / meanps);
    }
}
