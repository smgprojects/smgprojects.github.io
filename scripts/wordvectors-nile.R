## I always do this, just in case- MALLET uses java, who knows what else.
options(java.parameters = "-Xmx5120m")
### Ben Schmidt's WEM
library(devtools)
install_github("bmschmidt/wordVectors")
install.packages("magrittr")
library(magrittr)
install.packages("rtools")
library("rtools")
## get the diaries into WEM
library(wordVectors)
library(tsne)
install.packages('dplyr')
library(dplyr)

setwd("/Users/shawngraham/nile-diary-analysis")
#text uncleanded. lowercase vs uppercase can be meaningful...

model = train_word2vec("test-to-dec1-wo-dates.csv",output="nile.vectors",threads = 4,vectors = 1000,window=12)

##Check
model
rownames(model)

# precolumbian
nearest_to(model,model[["fine"]])

fine_words = model %>% nearest_to(model[[c("fine")]],100) %>% names
sample(fine_words,50)

# morning

nearest_to(model,model[["morning"]])

morning_words = model %>% nearest_to(model[[c("morning")]],100) %>% names
sample(morning_words,50)


# She

nearest_to(model,model[["her"]])

herwords = model %>% nearest_to(model[[c("her")]],100) %>% names
sample(herwords,50)

# church

nearest_to(model,model[["church"]])

churchwords = model %>% nearest_to(model[[c("church")]],100) %>% names
sample(churchwords,50)

# return

nearest_to(model,model[["drove"]])

returnwords = model %>% nearest_to(model[[c("drove")]],100) %>% names
sample(returnwords,50)

## get 50 close words to the 'return' vector
g = model[rownames(model) %in% returnwords [1:50],]

group_distances = cosineDist(g,g) %>% as.dist
plot(as.dendrogram(hclust(group_distances)),horiz=F,cex=1,main="Cluster dendrogram of the fifty words closest to a 'return' vector\nin ATIP request")

## get 50 close words to the 'battlefieldrecovery' vector
g2 = model[rownames(model) %in% egypt_words [1:50],]

group_distances = cosineDist(g2,g2) %>% as.dist
plot(as.dendrogram(hclust(group_distances)),horiz=F,cex=1,main="Cluster dendrogram of the fifty words closest to an 'Egypt' vector\nin ATIP request")



###some tsne plots
plot(model)

some_groups = nearest_to(model,model[[c("Egypt", "Mali", "Nigeria")]],150)
plot(filter_to_rownames(model,names(some_groups)))

##binaries?
nearest_to(model,model[["they"]])

## there is no 'bad' in these documents, so going to use 'problem' instead

##moral
library(ggplot2)

moral_vector = model[["me"]] - model[["they"]]
word_scores = data.frame(word=rownames(model))
word_scores$moral_score = model %>% cosineSimilarity(moral_vector) %>% as.vector

ggplot(word_scores %>% filter(abs(moral_score)>.7)) + geom_bar(aes(y=moral_score,x=reorder(word,moral_score),fill=moral_score<0),stat="identity") + coord_flip()+scale_fill_discrete("moral?",labels=c("good","bad")) + labs(title="The words showing the strongest skew along the good-bad continuum")

##archaeologists
nearest_to(model,model[["valued"]])

gender_vector = model[["archaeologists"]] - model[["looters"]]
word_scores = data.frame(word=rownames(model))
word_scores$gender_score = model %>% cosineSimilarity(gender_vector) %>% as.vector

ggplot(word_scores %>% filter(abs(gender_score)>.725)) + geom_bar(aes(y=gender_score,x=reorder(word,gender_score),fill=gender_score<0),stat="identity") + coord_flip()+scale_fill_discrete("words associated with",labels=c("archaeologist","looter")) + labs(title="The words showing the strongest skew along the archaeologist-looter binary")


