
# This script uses XTB API to download the newest data about
# chosen symbols.

Sys.setlocale("LC_TIME", "C");

if(format(Sys.Date(), "%w") %in% 1:5) {
    load(".RData");

    syms <- c('SXDPEX.DE', 'IBCI.DE', 'IEDY.UK', 'GDXJ.UK_9', 'CNYA.DE', 'USDPLN', 'INR.FR_9', 'XSMI.DE', 'AIGI.UK', 'AIGI.UK', 'USPY.UK', 'KER.PL_9');

    for(sym in syms) {
        ticker <- sub('\\..*$', '', sym);
        history <- get(ticker)[1:5];
        period <- Sys.Date() - last(history$date);
        if(period > 1) {
            data <- system(paste('python3', 'xtb.py', 'history', period, sym), intern=TRUE);
            for(row in data) {
                row <- strsplit(row,",")[[1]];
                history <- rbind(history, data.frame(date=as.Date(row[1], "%Y-%m-%d"),
                                                     price=as.numeric(row[2]),
                                                     open=as.numeric(row[3]),
                                                     high=as.numeric(row[4]),
                                                     low=as.numeric(row[5])));
            }

            assign(ticker, history, envir=.GlobalEnv);
            cat("updated", length(data), "rows in", ticker, "\n");
        }
    }

    save(list=ls(), file=".RData");
}

