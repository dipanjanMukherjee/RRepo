library(topicmodels)

#set parameter for gibbs
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003,5,63,100 000, 765)
nstart < 5
best <- TRUE

#no of topics
k <- 4

dtm.lda <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))

ldaout <- LDA(dtm.lda.new , k, method = "Gibbs", control = liist(nstart = nstart,
		seed = seed, burnin = burnin, iter = iter, thin = thin))

#docs to topics
ldaout.topics <- as.matrix(topics(ldaout))

#top 6 terms in each topic 
ldaout.terms <- as.matrix(terms(ldaout, 6))

#prob associated with each topic assignment
topic_probabilities <- as.data.frame(ldaout@gamma)

#find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm.lda.new), function(x)
	sort(topic_probabilities [x,])[k] / sort(topic_probabilities[x,])[k-1])

#map document to topics
topic_vector <- as.vector(ldaout.topics)
lda_cluster <- cbind(df.lda.new, topic_vector)

#no. of topics
k <- 2
tokenizer <- function(x)
	NGramTokenizer (x, weka_control(min = 2, max = 2))

BiGramTokenizer <- function(x)
	NGramTokenizer (x, weka_control(min = 2, max = 2))

dtm.lda <- DTM(corpus, control = list(weighting = weightTf, tokenize = BigramTokenizer))

dtm.lda <- removeSparseTerms(dtm.lda, sparse = 0.98)

