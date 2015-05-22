##installation of packages and loading all required packages.
for (package in c('tm', 'wordcloud', 'topicmodels', 'LDAvis', 'dplyr', 'stringi', 'servr')) {
  if (!require(package, character.only=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#read csv file into data frame. change name of file when needed
text.all <- read.csv("voc-comments.csv", header = TRUE, sep = ",")

#read corpus from column in data frame. change column name when needed
text.corpus <- Corpus(VectorSource(text.all$Comment.Text))

### LDA topics modelling function
#set up number of topics to mine. change number to desired numberof topics
topics <- 5

#set up controls for converting corpus to a document term matrix
dtm.control <- list(tolower = TRUE, 
                   removePunctuation = TRUE, 
                   removeNumbers = TRUE, 
                   stopwords = c(stopwords("english")), 
                   stemming = FALSE, 
                   wordLengths = c(3, Inf), 
                   weighting = weightTf)

#convert corpus to document term matrix. sparse terms are removed, tweak parameter in removeSparseTerms to control definition of sparse-ness
dtm <- DocumentTermMatrix(text.corpus, control = dtm.control)
dtm <- removeSparseTerms(dtm, 0.999)
dtm <- dtm[rowSums(as.matrix(dtm))>0, ]


lda.model <- LDA(dtm, topics)
  

##LDAvis
topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

#convert document term matrix back to a Corpus
dtm2list <- apply(dtm, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})
myCorp <- VCorpus(VectorSource(dtm2list))

#convert model to json for visualization 
lda.vis <- topicmodels_json_ldavis(lda.model, myCorp, dtm)

##visualize lda model
serVis(lda.vis)
