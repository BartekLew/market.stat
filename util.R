last <- function(arr) { arr[length(arr)]; }

#Show chart for given etf.
chart <- function(frame, name="") {
    min<-min(frame$price);
    max<-max(frame$price);
    trend <- trendCurve(frame$date,frame$price);
    plot(x=frame$date, y=frame$price, xlab="Date", ylab="Price", main=name);
    lines(trend, col="red", lwd=4);
    dateLine(as.Date("2008-06-01", "%Y-%m-%d"), frame$date[1], min, max);
    dateLine(as.Date("2018-01-01", "%Y-%m-%d"), frame$date[1], min, max);
    dateLine(as.Date("2020-03-01", "%Y-%m-%d"), frame$date[1], min, max);
    dateLine(as.Date("2022-02-26", "%Y-%m-%d"), frame$date[1], min, max);
}


