tempDF <- topicfile()
timestampvector <- as.POSIXct(tempDF$timestamp, format = input$timeformat)
if (input$clusteringtype == "Date") {
  timestampvector <-
    cut(timestampvector, breaks = paste(input$breaks, "day"))
}
if (input$clusteringtype == "Date-Time") {
  timestampvector <-
    cut(timestampvector, breaks = paste(input$breaks, "hour"))
}
if (input$clusteringtype == "Time") {
  time <- strftime(timestampvector, format = "%H:%M:%s")
  timestampvector <- as.POSIXct(time, format = "%H:%M:%s")
  timesstampvector <-
    cut(timestampvector, breaks = paste(input$breaks, "hour"))
}

tempDF$newTimeStamp <- as.character(timestampvector)
newTimeStamp <- unique(tempDF$newTimeStamp)

newTimeStamp <- as.data.frame(newTimeStamp)
clustercount <- nrow(newTimeStamp)
timeseriescluster <- c(1:nrow(newTimeStamp))

temdf <- cbind(newTimeStamp, timeseriescluster)
outputDF <-
  merge(x = tempDF,
        y = temdf,
        by = "newTimeStamp",
        all.x = TRUE)
outputDF