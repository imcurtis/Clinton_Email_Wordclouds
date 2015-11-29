library(dplyr)
install.packages("RSQLite")
library(RSQLite)

#Set up connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname = "clinton.sqlite")

#Clean up connection to the database
dbDisconnect(connection)

cmd <- 'select c.doc_id,c.reason_id,c.doc_count,d.body from classreason_doc as c, docs as d where c.doc_id = d.id;'
joined <- dbGetQuery(connection,cmd)
View(joined)

############################FOR LOOP###########################
joined_split <- split(joined, joined$reason_id)
new_names <- c("one", "two", "three", "four", "five", "six", "seven","eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen")
for (i in 1:length(joined_split)) {
  assign(new_names[i], joined_split[[i]])
  }
###############################################################
library(wordcloud)
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(wordcloud) # Library for plotting wordcloudsd
##library(gdata) # library for reading excel

joined.corpus <- Corpus(DataframeSource(data.frame(thirteen[4]))) ##dataframe also could be joined.body
joined.corpus <- tm_map(joined.corpus, removePunctuation)
joined.corpus <- tm_map(joined.corpus, removeNumbers)
joined.corpus <- tm_map(joined.corpus, stripWhitespace)
joined.corpus <- tm_map(joined.corpus, tolower)

#first inspect
inspect(joined.corpus)

joined.corpus <- tm_map(joined.corpus, removeWords, c("date", "department", "unclassified", "doc", "state", "case", "nnnnn", stopwords("english")))

#joined.corpus <- tm_map(joined.corpus, dictionary=joined.corpus)

joined.corpus <- tm_map(joined.corpus, PlainTextDocument)

#second inspect
inspect(joined.corpus)

set.seed(123) # Keeps the layout the same

# Document word term matix

dtm <- DocumentTermMatrix(joined.corpus)
tdm <- TermDocumentMatrix(joined.corpus)

#most frequent word
inspect

freq <- colSums(as.matrix(dtm))
ord <- order(freq)
freq

m <- as.matrix(tdm)

#words in decreasing order
v <- sort(rowSums(m),decreasing=TRUE)

v
#data frame with words and frequencies
d <- data.frame(word = names(v),freq=v)
d

joined.cloud <- wordcloud(d$word,d$freq,scale=c(3,.1),min.freq=2,max.words=30, random.order=F, rot.per=.15, colors="green", vfont=c("sans serif","plain"))
