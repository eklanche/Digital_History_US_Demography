#construct a 200-topic model (latent dirichlet allocation) 
#of the JSTOR population studies literature, 1915-2004

library(rJava)
.jinit(parameters="-Xmx8g")
gc()

library(mallet)
options(java.parameters = "-Xmx8000m")

documents <-mallet.read.dir(".//jstor//2015//clean")
mallet.instances <- mallet.import(documents$id, documents$text,"./stopwords.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
gc()

ntopics = 200  
topic.model <- MalletLDA(num.topics=ntopics)

topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()

topic.model$setAlphaOptimization(20,50)

topic.model$train(200)
topic.model$maximize(10)

gc()

doc.topics <- mallet.doc.topics(topic.model, smoothed = F, normalized = T)
gc()
topic.words <- t(mallet.topic.words(topic.model, smoothed = F, normalized = T))

doc_topics <- data.frame(doc.topics)
topic_words <- data.frame(topic.words)
topics <- list()
for (i in 1:ntopics){
  topics <- append(topics, (paste("topic",i,'')))
}
names(doc_topics) <- topics
doc_topics["doc"] <- documents$id
names(topic_words) <- topics
topic_words["word"] <- vocabulary

write.csv(doc_topics, paste(".//jstor//2015//topic-docs-",toString(ntopics),".csv",sep=''), row.names = FALSE)
write.csv(topic_words, paste(".//jstor//2015//topic-words-",toString(ntopics),".csv",sep=''), row.names = FALSE)