unemp <- read.csv("C:/Data Science with R/Pet Projects/unemp.csv")
unemp[1:3,]
set.seed(1)
grpunemp <- kmeans(unemp[,c("mean","stddev")], centers = 3, nstart = 10)
## list of cluster assignments
o <- order(grpunemp$cluster)
data.frame(unemp$state[o],grpunemp$cluster[o])
plot(unemp$mean, unemp$stddev, type="n", xlab="mean", ylab="stddev")
text(x=unemp$mean,y=unemp$stddev,labels = unemp$state, col = grpunemp$cluster+1)
