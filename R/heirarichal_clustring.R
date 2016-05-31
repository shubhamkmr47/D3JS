if (!require("RJSONIO")) {
  install.packages("RJSONIO", dependencies = TRUE)
  library(RJSONIO)
}
if (!require("rjson")) {
  install.packages("rjson", dependencies = TRUE)
  library(rjson)
}

data <- mtcars
rows <- rownames(mtcars)
data <- cbind(rows, data)
json_file <- toJSON(unname(split(data, 1:nrow(data))))

json_file <- fromJSON(json_file)

json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

data <- do.call("rbind", json_file)
data <- data.frame(data)
rownames(data) <- data$rows
data$rows <- NULL
d <- dist(data)
hc <- hclust(d)
#plot(hc)

#names(hc)
mergeData <- data.frame(hc$merge)
lableData <- hc$labels
size <- dim(mergeData)
storeData <- c(1:size[1])

for (i in 1:size[1]) {
  index1 <- mergeData$X1[i]
  index2 <- mergeData$X2[i]
  
  if (index1 < 0 & index2 < 0) {
    string <- paste('{"name": "","children":[{"name":"',
                    lableData[-index2], '"},{"name":"',lableData[-index1],'"}]}',
                    collapse = "")
  }
  else if (index1 > 0 & index2 > 0) {
    string <- paste('{"name": "","children":[',
                    storeData[index1], ',',storeData[index2],']}',
                    collapse = "")
  }
  else if (index1 > 0 & index2 < 0) {
    string <- paste('{"name": "","children":[',
                    storeData[index1], ',{"name":"',lableData[-index2],'"}]}',
                    collapse = "")
  }
  else if (index1 < 0 & index2 > 0) {
    string <- paste('{"name": "","children":[{"name":"',
                    lableData[-index1],'"},',storeData[index2],']}',
                    collapse = "")
  }
  storeData[i] <- string
}
message <- storeData[size[1]]
