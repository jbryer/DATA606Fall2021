library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

ompWordCloud <- function(text) {
	docs <- Corpus(VectorSource(text))
	# Convert the text to lower case
	docs <- tm_map(docs, content_transformer(tolower))
	# Remove numbers
	docs <- tm_map(docs, removeNumbers)
	# Remove english common stopwords
	docs <- tm_map(docs, removeWords, stopwords("english"))
	# Remove your own stop word
	# specify your stopwords as a character vector
	docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
	# Remove punctuations
	docs <- tm_map(docs, removePunctuation)
	# Eliminate extra white spaces
	docs <- tm_map(docs, stripWhitespace)
	dtm <- TermDocumentMatrix(docs)
	m <- as.matrix(dtm)
	v <- sort(rowSums(m),decreasing=TRUE)
	d <- data.frame(word = names(v),freq=v)
	head(d, 10)
	wordcloud(words = d$word, freq = d$freq, min.freq = 2,
			  max.words=200, random.order=FALSE, rot.per=0.35,
			  colors=brewer.pal(8, "Dark2"))
}
