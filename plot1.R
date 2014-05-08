read_dataset <- function(dir) {
    filename <- file.path(dir, "household_power_consumption.txt")
    con <- file(filename)
    open(con)
    l <- readLines(con)
    lines <- grep("^(1/2/2007|2/2/2007)", 
                  x = l)

    seek(con, where=0)
    setAs("character", "mydate", function(from) as.Date(from, 
                                                        format="%d/%m/%Y"))
    setAs("character", "mytime", function(from) strptime(from,
                                                         format="%H:%M:%S"))
    setClass("mydate")
    setClass("mytime")
    df <- read.table(con, header=TRUE, sep=";", row.names = NULL,
                     col.names=c("Date",
                                 "Time",
                                 "Global_active_power",
                                 "Global_reactive_power",
                                 "Voltage",
                                 "Global_intensity",
                                 "Sub_metering_1",
                                 "Sub_metering_2",
                                 "Sub_metering_3"),
                     colClasses=c("mydate", 
                                  "mytime",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric"),
                     skip = lines[1] - 1,
                     nrows = length(lines))
    close(con)
    df
}

ds <- read_dataset(getwd())
png("plot1.png", width=480, height=480, type="quartz")
hist(ds[,"Global_active_power"], freq=TRUE, 
     main="Global Active Power", 
     xlab="Global Active Power(kilowatts)",
     col="red")
dev.off()