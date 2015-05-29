##installation of packages and loading all required packages.
for (package in c('tm', 'wordcloud', 'topicmodels', 'LDAvis', 'dplyr', 'stringi', 'servr', "Rmpfr", "SnowballC")) {
  if (!require(package, character.only=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#read csv file into data frame. change name of file when needed
text.file <- file.choose()
text.all <- read.csv(text.file, header = TRUE, sep = ",")

#read corpus from column in data frame. change column name when needed
winDialog(type = "ok", "Opening a view of the first rows of the text dataset\n\nPlease note the column name of the column containing the text to be analyzed.\n\nCopy the exact name (case sensitive) of the column and paste it into the next dialog box")
View(head(text.all))

Sys.sleep(5)
col.num <- winDialogString("Enter the name of the column containing text to be analyzed", "")
text.corpus <- Corpus(VectorSource(text.all[, which( colnames(text.all)==col.num)]))

### LDA topics modelling function

#set up controls for converting corpus to a document term matrix
dtm.control <- list(tolower = TRUE, 
                   removePunctuation = TRUE, 
                   removeNumbers = TRUE, 
                   stopwords = c(stopwords("english")), 
                   stemming = FALSE, 
                   wordLengths = c(3, Inf), 
                   weighting = weightTf)

#dtm.control$stemming <- ifelse(winDialog(type = "yesno", "Stem words in corpus?") == "YES", TRUE, FALSE)

#convert corpus to document term matrix. sparse terms are removed, tweak parameter in removeSparseTerms to control definition of sparse-ness
dtm <- DocumentTermMatrix(text.corpus, control = dtm.control)
inspect(dtm[1:10, 1:5])

sparse.num <- as.numeric(winDialogString("Enter maximal allowed sparsity (0-99.99) of terms to be included in clustering (the smaller, the lesser the terms)", "95"))/100
dtm <- removeSparseTerms(dtm, sparse = sparse.num)

dtm <- dtm[rowSums(as.matrix(dtm))>0, ]


# The log-likelihood values are then determined by first fitting the model using for example
#set up parallel computing
library(parallel)
cl <- makeCluster(8)

par.setup <- parLapply( cl, 1:length(cl),
                        function(xx) {
                          require(topicmodels)
                        })

clusterExport(cl, c('dtm', 'burnin', 'iter', 'keep'))

# generate numerous topic models with different numbers of topics
sequ <- seq(2, 50, 1) # in this case a sequence of numbers from 1 to 50, by ones.
#fitted_many <- lapply(sequ, function(k) LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))Z
fitted_many <- parLapply(cl, sequ, function(k) LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))

stopCluster(cl)

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
## discover optimal number of topics
harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
plot(sequ, hm_many, type = "l")

# compute optimum number of topics
topics <- sequ[which.max(hm_many)]

#set up number of topics to mine. change number to desired numberof topics
dialog.text <- as.character(paste0("Optimal number of topics = ", topics, ". Enter the number of topics for LDA"))
topics <- as.numeric(winDialogString(dialog.text, ""))


lda.model <- LDA(dtm, topics)
terms(lda.model,30)

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


