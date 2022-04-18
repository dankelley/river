# https://wateroffice.ec.gc.ca/download/report_e.html?dt=46&df=csv&ext=zip
f1 <- "01AK003_HG_Apr-18-2022_09_13_00PM.csv" # Fredericton
f2 <- "01AO002_HG_Apr-18-2022_09_44_51PM.csv" # Maugerville
f3 <- "01AO012_HG_Apr-18-2022_09_41_30PM.csv" # Gagetown
f4 <- "01AP003_HG_Apr-18-2022_09_33_23PM.csv" # OakPoint
f5 <- "01AP005_HG_Apr-18-2022_09_19_20PM.csv" # StJohn

readRiver <- function(file)
{
    lines <- readLines(file)
    headerEnd <- grep("^Date", lines)
    header <- lines[1:headerEnd]
    data <- read.csv(file, skip=headerEnd-1, header=TRUE)
    names <- names(data)
    names[names=="Value..m."] <- "height"
    names(data) <- names
    data$t <- as.POSIXct(data[,1])
    rval <- list(header=header, data=data)
    class(rval) <- "river"
    rval
}
plot.river <- function(r, add=FALSE, ...)
{
    t <- r$data$t
    y <- r$data$height
    if (add) {
        lines(t, y, lwd=2, ...)
    } else {
        oce::oce.plot.ts(t, y, lwd=2,
            ylab="Height [m]", drawTimeRange=FALSE, grid=TRUE, ...)
    }
}

r1 <- readRiver(f1)
r2 <- readRiver(f2)
r3 <- readRiver(f3)
r4 <- readRiver(f4)
r5 <- readRiver(f5)
ylim <- range(c(r1$data$height, r2$data$height, r3$data$height, r4$data$height, r5$data$height))
if (!interactive())
    pdf("river_watch.pdf")
plot(r1, ylim=ylim)
plot(r2, add=TRUE, col=2)
plot(r3, add=TRUE, col=3)
plot(r4, add=TRUE, col=4)
plot(r5, add=TRUE, col=5)
legend("topleft", col=1:5, lwd=2, bg="white",
    legend=c("Frederiction", "Maugerville",
        "Gagetown",
        "Oak Point", "St. John"))
if (!interactive())
    dev.off()

