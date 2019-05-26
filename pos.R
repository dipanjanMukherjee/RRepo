library(NLP)
library(openNLP)
library(rJava)
library(qdap)

#sets heap size
options(java.parameters = "-Xmx8000m")

book <- read.csv("telephony.csv")

output <- data.frame()

output <- data.frame(book$summary)

#remove rows with blanks
output <- (output[!(book$summary == ""), 1])

output$new <- 1
output$new1 <- 1

#remove special characters
output$new <- gsub('[[:punct:]]+', '', output$book.Summary)

#remove digits
output$new1 <- gsub('[[:digit:]]+', '', output$new)

#remove all single words, soln. for 'cannot convert strings to data
for(i in 1:nrow(output)){
  if(qdap::wc(output[i,]$new1)=="1"){
    print(i)
    output <- output[-i,]
  }
}

#garbage collection
jgc <- function()
{
  gc().jcall("java/lang/System", method = "gc")
}



nlp <- function(y){
  jgc()
  
  # store description column in a vector
  row <- y
  
  # convert row vector to string for computation purpose
  x <- as.String(row)
  
  # sentence annotation
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  
  # POS tag the wordsand extract the words from the output
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  
  # extract tags from words
  tags <- sapply(POSwords$features, '[[', "POS")
  
  # create data frame with words and tags
  tokenizedAndTagged <- data.frame(Tokens = x[POSwords], Tags = tags, stringsAsFactors = FALSE)
  
  # define a flag(tags_mod) for pos tags : flag set to 1 if it contains the interested pos else 0
  # noun and adjective tags(NN,JJ)
  # this will also capture variations like NNP,NNPS
  tokenizedAndTagged$Tags_mod <- grepl("NN|JJ", tokenizedAndTagged$Tags)
  
  # initialize a vector to store chunk indexes
  chunk = vector()
  
  # iterate through each word and assign each word to a group
  # if word doesn't belong to NN|JJ tags (i.e tags_mod flag is 0) assign it to the default group (0)
  # if ith tag is in NN|JJ (i.e tags_mod flag is 1) assign it to group i-1 if the (i-1)th tag_mod flag is also 1d
  chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
  
  for(i in 2:nrow(tokenizedAndTagged)) {
    if(!tokenizedAndTagged$Tags_mod[i]){
      chunk[i] = 0
    }
    else if(tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]){
      chunk[i] = chunk[i-1]
    }
    else{
      chunk[i] = max(chunk) + 1
    }
  }
  
  # split and chunk words
  text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
  tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
  names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
  
  # extract chunks matching pattern
  res <- text_chunk[grepl("JJ-NN|NN-NN|NN.-NN", names(text_chunk))]
  
  res <- lapply(res, function(x) paste(x, collapse = " "))
  
  res2 <- as.data.frame(res)
  
  names(res2) <- NULL
  
  res3 <- sapply(res2[1,], function(x) paste(x, collapse = ","))
  
  res3 <- paste(unlist(res2), collapse = ",")
  
}

keywords <- lapply(output$new1, nlp)
keywords <- unlist(keywords)
output$X3 <- keywords
