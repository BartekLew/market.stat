
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

bisectMatch <- function(ys, matcher, errGrade, total = NA, tg = NA, offset = 0) {
    n <- length(ys);
    if(!is.function(total)) {
        total <- matcher(ys, offset);
        tg <- errGrade(total, 1:n, ys);
    }

    if(n<10) {data.frame(x=c(1,n), y=c(total(1), total(n)));}
    else {
        sectOn <- floor(n/2);
        a <- matcher(ys[1:sectOn],offset);
        ag <- errGrade(a, 1:sectOn, ys[1:sectOn]);
        b <- matcher(ys[sectOn:n], offset+sectOn);
        bg <- errGrade(b, sectOn:n, ys[sectOn:n]);
    
        print(c(ag, bg, tg));
        if(ag < tg || bg < tg) {
            ar <- bisectMatch(ys[1:sectOn], matcher, errGrade, a, ag, offset);
            br <- bisectMatch(ys[sectOn:n], matcher, errGrade, b, bg, offset+sectOn);
            rbind(ar, br);
        } else {
            data.frame(x = c(1,n) + offset, y = c(total(1 + offset), total(n + offset)));
        }
    }
}

trends <- function(ys) {
    tr <- bisectMatch(ys, matchLine,
          function(f, xs, ys) sum(abs(ys - f(xs)))/(length(xs)**1.5));

    plot(ys, type='l');
    lines(tr);

    tr
}
