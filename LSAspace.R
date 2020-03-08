dtm_tfidf <- weightTfIDF(dtm_new)
dtm_scale <- scale(dtm_tfidf)

LSAspace <- lsa((dtm_scale), dims = dimcalc_raw())
dim(LSAspace$tk)

sk_cum <- c("")
temp <- 0

for(i in 1:length(LSAspace$sk)){
	temp <- temp + ((LSAspace$sk[i]^2 / sum(LSAspace$sk^2))*100)
	sk_cum <- c(sk_cum, round(temp,2))
}

sk_cum <- sk_cum[-1]
num_cluster <- length(LSAspace$sk)

fit.km <- kmeans(LSAspace$tk, num_cluster, nstart = 25)

round(fit.km$clusters, digits = 1)


