# dynamic time warping distance
library(dtw)
idx <- seq(0, 2*pi, len = 100)
a <- sin(idx) + runif(100)/10
b <- cos(idx)

align <- dtw::dtw(a, b, step = asymmetricP1, keep.internals = T)
dtw::dtwPlotTwoWay(align)



# synthetic control chart time series
# 6 classes - [1-100] Normal, [101-200] cyclic, [201-300] increasing trend, [301-400] decreasing trend,
# [401-500] upwards shift, [501-600] downward shift

sc <- read.table("synthetic_control.data", header = F, sep = "")

#show one sample from each class
idx <- c(1,101,201,301,401,501)

sample1 <- t(sc[idx,])

plot.ts(sample1, main = "")

# sample n cases from each class
n <- 10
s <- sample(1:100, n)
idx <- c(s, 100+s, 200+s, 300+s, 400+s, 500+s)

sample2 <- sc[idx,]

observedLabels <- c(rep(1,n), rep(2,n), rep(3,n), rep(4,n), rep(5,n), rep(6,n))

# hierarchical clustering
hc <- stats::hclust(dist(sample2), method = "ave")
plot(hc, labels = observedLabels, main = "")

myDist <- dist(sample2, method = "DTW")
hc <- stats::hclust(myDist, method = "average")
plot(hc, labels = observedLabels, main = "")
memb <- stats::cutree(hc, k = 8)
table(observedLabels, memb)

library(dtwclust)
#Fuzzy(distance = "dtw_basic", centroid ="fcm")
dtwclust::tsclust(sample1, distance = "dtw_basic", centroid = "fcm", arg = "mean")
Fuzzy(distance = "dtw", centroid ="fcm")

