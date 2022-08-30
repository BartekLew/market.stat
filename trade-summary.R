allSyms <- read.csv("xtb.csv");

forkey <- function(dates, date, vals) {
    diff <- dates[1] - date;
    cdate <- dates[1];
    val <- vals[1];
    for(i in 2:length(dates)) {
        if(abs(dates[i] - date) < abs(cdate - date)) {
            cdate <- dates[i];
            val <- vals[i];
        }
    }

    val
}

tradesum <- function(sf, currency) {
    df <- read.csv(sf, stringsAsFactors=F);
    names(df) <- c("symbol", "volume", "open", "bought", "close", "sold");
    df$open <- as.Date(df$open, "%a %b %d %H:%M:%S CEST %Y");
    df$close <- as.Date(df$close, "%a %b %d %H:%M:%S CEST %Y");

    df$gain <- (df$sold - df$bought) / df$bought;
    df$earned <- (df$sold - df$bought) * df$volume;
    df$days <- as.numeric(df$close - df$open);

    df$currency <- sapply(df$symbol, function(sym) allSyms[allSyms$symbol == sym, 'currency']);
    df$curpair <- sapply(df$currency, function(cur) if(cur == currency) NA
                                                    else paste(cur, currency, sep=""));

    df$oprice <- sapply(1:nrow(df), function(i) {
                                        if(is.na(df$curpair[i])) 1
                                        else {
                                            pdf <- get(df$curpair[i]);
                                            forkey(pdf$date, df$open[i], pdf$price);
                                        }
                                    });

    df$cprice <- sapply(1:nrow(df), function(i) {
                                        if(is.na(df$curpair[i])) 1
                                        else {
                                            pdf <- get(df$curpair[i]);
                                            forkey(pdf$date, df$close[i], pdf$price);
                                        }
                                    });
                                    
   df$paid <- df$bought * df$oprice;
   df$cashed <- df$sold * df$cprice;
   df$total <- (df$cashed - df$paid) * df$volume

   print(df);

   ans <- data.frame(invested=sum(df$paid * df$volume),
                     cashed=sum(df$cashed * df$volume),
                     time=as.numeric(max(df$close) - min(df$open)));
   ans$earned <- ans$cashed - ans$invested;
   ans$ratio <- round(ans$earned / ans$invested * 100, 2);

   ans;
}
