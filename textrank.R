auto <- read.csv("auto.csv")
unique <- unique(auto$cluster)

class(unique)

auto$summary <- as.character(auto$summary)
auto$customlabel <- NULL
library(doSNOW)

#start cluster
library(parallel)
cl <- parallel::makeCluster(10, type = "sock")
doSNOW::registerDoSNOW(cl)

start.time <- Sys.time()

for(i in 1:length(unique)){
  rown <- row.names(subset(auto, auto$cluster = unique[i]))
  s <- subset(auto, auto$cluster == unique[i])
}

#textrank starts
library(textrank)
s$summary <- as.character(s$summary)

library(tokenizers)
s$copy <- tokenizers::tokenize_words(s$summary, lowercase = TRUE, stopwords = NULL, strip_punct = F, strip_numeric = F,
                                     simplify = T)

#remove alpha nnumemric
for(i in 1:nrow(s)){
  s$copy1[i] <- paste(gsub(".*[0-9].*[a-z].[0-9]", "", unlist(s$copy[i])), sep = " ", collapse = " ")
}

s$copy1 <- gsub('[0-9', "x", s$copy1)

#textrank
capture <- textrank::textrank_keywords(s$copy1, relevant = rep(TRUE, length(s$copy1)), p = 1/3, ngram_max = 5, sep = "-")

auto$customlabel[as.numeric(rown)] <- capture$terms[1]

total.time <- Sys.time() - start.time
total.time

write.csv(auto, 'custom.csv')