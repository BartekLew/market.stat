
matchLine <- function(ys, off=0) {
    n <- length(ys);
    xs <- 1:n;
    as <- sapply(2:(n/2)-1,
                 function(i) (ys[n-i] - ys[i]) / (xs[n-i] - xs[i]));
    bs <- sapply(2:(n/2)-1,
                 function(i) ys[i] - as[i] * xs[i]);

    a <- mean(as);
    b <- mean(bs);

    function(x) a * (x - off) + b;
}

trends <- function(ys, chunks = 10) {
    n <- length(ys);
    step <- n / chunks;

    plot(ys, type='l');
    for(i in 1:chunks) {
        xsi <- floor((i-1)*step) : floor(i*step);
        ysi <- ys[xsi];
        appr <- matchLine(ysi, xsi[1]);
        s <- xsi[1]; e <- xsi[length(xsi)];
        lines(c(s,e), c(appr(s), appr(e)));
    }
}
