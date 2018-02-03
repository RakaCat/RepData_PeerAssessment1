unzip('data.zip')
raw <- read.csv('activity.csv', quote = '"')
raw[2]<- as.Date(raw[,2])
raw[1]<- as.integer(raw[,1])
raw[3]<- as.integer(raw[,3])

#Mean total number of steps per day
hist(raw$steps)
mean(raw$steps)
median(raw$steps)

#Average daily activity pattern
plot(raw$interval, raw$steps)