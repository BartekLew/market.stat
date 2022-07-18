# Load all symbols available on XTB platform

allSyms <- read.csv('allSyms.csv');
allSyms <- allSyms[,c('symbol', 'currency', 'categoryName', 'groupName', 'description')];
write.csv(allSyms, 'xtb.csv', row.names=F)

for(sym in allSyms$symbol[1:nrow(allSyms)]) {
    print(sym);
    df <- data.frame();
    data <- system(paste('python3', 'xtb.py', 'history', 10000, sym), intern=TRUE);
    for(row in data) {
        vals <- strsplit(row, ",")[[1]];
        df <- rbind(df, data.frame(date=as.Date(vals[1], "%Y-%m-%d"),
                                   price=as.numeric(vals[2]),
                                   open=as.numeric(vals[3]),
                                   high=as.numeric(vals[4]),
                                   low=as.numeric(vals[5]),
                                   vol=as.numeric(vals[6])));
    }
    write.csv(df, paste('xtb/', sym, '.csv', sep=''), row.names=F);
}

