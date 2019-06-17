library(plyr)
library(arules)
library(arulesViz)

require(dplyr)
require(tidyr)
require(stringr)


# rule generation

df <- read.csv("file.csv")
keeps <- c("topics", "timeSeriesCluster")
df <- df[keeps]

df <- unique(df)

df_itemList <- plyr::ddply(df, c("timeSeriesCluster"), function(df1) paste(df1$topics, collapse = ","))

write.csv(df_itemList, "transactionsfile.csv", quote = F, row.names = F)

txn <- arules::read.transactions("transactionsfile.csv", quote = F, row.names = F)

basket <- arules::apriori(txn, parameter = list(sup = 0.5, conf = 0.5, target = "rules", minlen = 2, maxlen = 4))

plot(basket, method = "graph", interactive = T)

data <- as(basket, "data.frame")

write.csv(data, "rules.csv")


# generate sub plots
rules_data <- read.csv("filteredrules.csv")

sp1 <- tidyr::separate(rules_data, rules, c("A", "B"), sep = "=>")
sp1$A <- gsub("\\s?\\{\\s?","",sp1$A)
sp1$A <- gsub("\\s?\\}\\s?","",sp1$A)
sp1$B <- gsub("\\s?\\{\\s?","",sp1$B)
sp1$B <- gsub("\\s?\\{\\s?","",sp1$B)

sp1$D <- paste(sp1$A, ",", sp1$B, sep = "")

it1 <- sp1$A
it2 <- sp1$B

it1 <- as.data.frame(it1)
it1$split <- stringr::str_split(it1$it1, ",")

for(i in 1:nrow(it1)){
  new <- subset(basket, subset = lhs %in% c(unlist(it1$split[i])) & rhs %in% c(it2[i]))
  if(i==1){
    updatedbasket <- new[1]
  }
  else{
    updatedbasket <- c(updatedbasket, new[1])
  }
}

plot(updatedbasket, method = "graph", interactive = TRUE)