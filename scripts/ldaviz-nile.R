setwd("/Users/shawngraham/nile-diary-analysis")
install.packages("rio")
library("rio")
install.packages("tm")
library(tm)
options(java.parameters = "-Xmx5120m")

##importing opencontext site diaries
importdocs <- lapply("test-to-dec1-wo-dates.csv", readLines)  ## change this to the version that doesn't have the date columns, or figure out how to tell r to just grab the text column
## or rather, maybe just the year column, because that becomes a word, in which case might see some interesting patterns
stop_words <- stopwords("SMART")

importdocs <- gsub("'", "", importdocs)  # remove apostrophes
importdocs <- gsub("[[:punct:]]", " ", importdocs)  # replace punctuation with space
importdocs <- gsub("[[:cntrl:]]", " ", importdocs)  # replace control characters with space
importdocs <- gsub("^[[:space:]]+", "", importdocs) # remove whitespace at beginning of documents
importdocs <- gsub("[[:space:]]+$", "", importdocs) # remove whitespace at end of documents
importdocs <- tolower(importdocs)

doc.list <- strsplit(importdocs, "[[:space:]]+")
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

doc.list <- strsplit(importdocs, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

####

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  

# MCMC and model tuning parameters:
K <- 15   ## this is the number of topics.
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
install.packages("lda")
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

#need to change atip01 to something more meaningful
atip01 <- list(phi = phi,
                  theta = theta,
                  doc.length = doc.length,
                  vocab = vocab,
                  term.frequency = term.frequency)

install.packages("LDAvis")
library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = atip01$phi, 
                   theta = atip01$theta, 
                   doc.length = atip01$doc.length, 
                   vocab = atip01$vocab, 
                   term.frequency = atip01$term.frequency)
install.packages("servr")
serVis(json, out.dir = 'vis', open.browser = TRUE)
# you can view in browser at http://127.0.0.1:4321/