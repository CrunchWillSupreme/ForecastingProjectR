require(xts)
require(forecast)
require(forecast)

setwd('C:\\DAPT\\Tesla Weather Practicum\\SPLtxt')
SPL <- as.data.frame(read.table('mx_cancun_spl_act.txt', header=T, sep=" ", na= '-99.000000'))

#MAKE NEW DF WITH DATE, TIME, TEMP COLUMNS ONLY
dtt <- as.data.frame(cbind(SPL$date, SPL$time, SPL$tempc))

#SELECT A SAMPLE 
preclean <- (dtt[6:72, 1:3])
head(preclean, n=15)
#ADD COLUMN NAMES
colnames(preclean) <- c('date', 'time', 'T')
#FORMAT DATE COLUMN
preclean$date<- as.Date(sapply(preclean$date, function(x) paste0(substr(x,1,4), '-', substr(x,5,6), '-', substr(x, 7, 8))))
#MAKE THE "0" IN TIME COLUMN TO "000"
preclean$time[preclean$time %in% "0"] <- "000"
#FUNCTION TO MAKE ALL VALUES IN TIME COLUMN 6 CHARACTERS
sixchar<-function(y){
  hr<-(y)
  for (i in 1:length(hr)){
    if(nchar(hr[i])==3){
      hr[i]<-paste("0",hr[i],sep="")
    }
  }
  # for (i in 1:length(hr)){
  #   if(nchar(hr[i])==4){
  #     hr[i]<-paste(hr[i], "00",sep="")
  #   }
  # }
  return(hr)
}
#RUN THE FUNCTION
preclean$time <- sixchar(preclean$time)
#ADD COLONS EVERY TWO CHARACTERS OF TIME
preclean$time<-gsub("(\\d{2})(?=\\d{2})", "\\1:", preclean$time, perl = T)

#sampleset$time <- sprintf('%02d:00:00', sampleset$time)

#CONCATENATE DATE AND TIME COLUMNS TO MAKE NEW COLUMN "datetime"
preclean$datetime<- paste(preclean$date, preclean$time, sep=" ")
#CONVERT DATETIME TO UTC
preclean$datetime <- as.POSIXct(preclean$datetime, "UTC")
#REMOVE THE DATE AND TIME COLUMNS
clean<-preclean[,-(1:2), drop= F]

data <- clean[,c(2,1)]

#CONVERT THE DATA INTO A READABLE TIME SERIES
datatsindex <- seq(from = as.POSIXct("2000-01-01 00:00:00"), to = as.POSIXct("2000-01-03 18:00:00"), by="hour")
nValid <- 24
nTrain <- length(data$T) - nValid
train.ts <- window(data$T, start = c(1, 1), end = c(1,nTrain))
valid.ts <- window(data$T, start = c(1, nTrain+1), end = c(1, nTrain+nValid))

head(train.ts)

par(mfrow = c(3, 1))
ESOpt <- ets(train.ts)
ESOpt.pred <- forecast(ESOpt, level=c(80,95), h = nValid)
plot(ESOpt.pred, ylim = c(17, 30),  ylab = "Temperature", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1, 73), main = "", flty = 2)
axis(1, at = seq(1, 73, 1), labels = format(seq(1, 73, 1)))
lines(ESOpt.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)




