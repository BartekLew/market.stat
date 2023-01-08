# Load all symbols available on XTB platform
source('util.R');

allSyms <- read.csv('allSyms.csv');
allSyms <- allSyms[,c('symbol', 'currency', 'categoryName', 'groupName', 'description')];
write.csv(allSyms, 'xtb.csv', row.names=F)

for(sym in allSyms$symbol[1:nrow(allSyms)]) {
    fname <- paste('xtb/', sym, '.csv', sep='');
    print(sym);
    df <- data.frame();
    linesToRead <- 10000;
    if(file.exists(fname)) {
        df <- read.csv(fname);
        if(nrow(df) > 0) {
            df$date <- as.Date(df$date, "%Y-%m-%d");
            for(col in c('price', 'open', 'high', 'low', 'vol')) 
                df[,col] <- as.numeric(df[,col]);
            linesToRead <- Sys.Date() - last(df$date);
        }
    }
    data <- system(paste('python3', 'xtb.py', 'history', linesToRead, sym), intern=TRUE);
    for(row in data) {
        vals <- strsplit(row, ",")[[1]];
        df <- rbind(df, data.frame(date=as.Date(vals[1], "%Y-%m-%d"),
                                   price=as.numeric(vals[2]),
                                   open=as.numeric(vals[3]),
                                   high=as.numeric(vals[4]),
                                   low=as.numeric(vals[5]),
                                   vol=as.numeric(vals[6])));
    }
    write.csv(df, fname, row.names=F);
}

