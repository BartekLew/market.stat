# I want to make predicions based on statistics. That's why I want
# to extend basic frames by statistic values

mysd <- function(arr) {
    if(length(arr)<2) 0.01
    else sd(arr);
}

statCols <- function(frame, window=nrow(frame)%/%10, col="price",
                            baseCols=c()) {
    if(length(baseCols) > 0) {
        frame <- frame[,baseCols];
    }

    ans <- cbind(frame, mean=round(windowOp(frame, window, rowOp(mean, col)),2),
                        sd=round(windowOp(frame, window, rowOp(mysd, col)),2));
    ans <- cbind(ans, daydiff=round((frame$high-frame$low) / ans$sd,2),
                      relprice=round((frame$price - ans$mean) / ans$sd,2));
}

extAssetTab <- function(frame, window=30) {
    onPhases(frame, function(group, sval) {
                groupMap(group, denser(firstAndLast(group$date), window),
                         fun=function(gp, i, max) {
                            data.frame(from=gp$date[1],
                                       to=gp$date[nrow(gp)],
                                       daydiff=round(mean(gp$daydiff),2),
                                       max=round(max(gp$relprice),2),
                                       min=round(min(gp$relprice),2),
                                       sd=round(sd(gp$relprice),2),
                                       mean=round(mean(gp$relprice),2),
                                       trendProg=round(i/max, 2));
                         });
                });
}

onPhases <- function(frame, fun) {
    tc <- trendCurve(frame$date, frame$price);
    groupMap(statCols(frame, baseCols=1:5),
             tc$x,
             fun=function(group, stage, max) {
                slen <- as.numeric(tc$x[stage] - tc$x[stage-1]);
                sval <- (tc$y[stage] - tc$y[stage-1])/slen

                group <- cbind(group, trendProg = (1:nrow(group))/nrow(group));

                cbind(fun(group, sval), slope=sval);
             });
}

oddsProf <- function(eat, tgtCol="slope", srcCol="mean",
                     outCols=c('positive', 'negative')) {
    s <- eat[[srcCol]];
    t <- eat[[tgtCol]];

    pp <- nrow(eat[t >= 0 & s >= 0,]);
    pn <- nrow(eat[t >= 0 & s < 0,]);
    np <- nrow(eat[t < 0 & s >= 0,]);
    nn <- nrow(eat[t < 0 & s < 0,]);

    ans <- data.frame(positive = round(pp/(pp+pn),2), negative = round(nn/(nn+np),2));
    names(ans) <- outCols;
    ans
}

# Compute trend length profile
trendLen <- function(asset) {
    tc <- trendCurve(asset$date, asset$price);
    l <- nrow(tc);
    trends <- data.frame(length=vapply(2:(l-1), function(i) as.numeric(tc$x[i] - tc$x[i-1])/365, 0),
                         sign=vapply(2:(l-1), function(i) tc$y[i] - tc$y[i-1], 0));
    
    round(data.frame(cLen=as.numeric(tc$x[l] - tc$x[l-1]) / 365,
               cSig=-(tc$y[l-1] - tc$y[l-2]),
               meanp=mean(trends[trends$sign>0,]$length),
               meann=mean(trends[trends$sign<=0,]$length),
               sdp=sd(trends[trends$sign>0,]$length),
               sdn=sd(trends[trends$sign<=0,]$length),
               minp=min(trends[trends$sign>0,]$length),
               minn=min(trends[trends$sign<=0,]$length),
               maxp=max(trends[trends$sign>0,]$length),
               maxn=max(trends[trends$sign<=0,]$length)),2);
}

# Probabilities that price below 30-day mean comes with negative
# trend.
probCols <- function(assets) {
    buildFrame(assets$ticker, function(tick) {
        eat <- extAssetTab(get(tick));
        ans<-try(cbind(oddsProf(eat, outCols=c("meanPredPosit", "meanPredNeg")),
                       oddsProf(eat, srcCol="max", outCols=c("maxPredPosit", "maxPredNeg")),
                       oddsProf(eat, srcCol="min", outCols=c("minPredPosit", "minPredNeg"))));
        try(row.names(ans) <- c(tick));
        print(ans);
        ans;
    });
}

# make basic prediction of current trend on given asset
forecast <- function(asset) {
    history <- statCols(get(asset)[,1:5], window=30);
    l <- nrow(history);
    history <- history[(l-30):l,];
    print(data.frame(mean = mean(history$relprice),
                     max = max(history$relprice),
                     min = min(history$relprice)));
    print(basicPredicators[asset,]);
}
