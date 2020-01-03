# https://www.r-bloggers.com/qualitative-data-science-using-rqda-to-analyse-interviews/
# https://lucidmanager.org/qualitative-data-science/
# https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
# https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf
# https://datascienceplus.com/qualitative-research-in-r/
# 
#


# Instaling packages
install.packages("tidyverse")
install.packages("RQDA")
install.packages("tm")
install.packages("wordcloud")
install.packages("topicmodels")
install.packages("igraph")
install.packages("tidytext")


library(tidyverse)
library(tm)
library(topicmodels)
library(wordcloud)
library(igraph)
library(ggplot2)
library(stringr)
library(tidytext)
library(dplyr)
library(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# coding?
require("RQDA")
RQDA() # launches the coding environment


# Loading the data from rqda
openProject("rqda-sample.rqda")

# extracting the basic data: files and text
transcripts <- data.frame(name = RQDAQuery("SELECT name FROM source"),
                         text = RQDAQuery("SELECT file FROM source"))
file_names <- transcripts$name


# creating a dataset with every file and the text within
data <- data.frame(file=transcripts$name, text=transcripts$file)

# creating a dataset with every file and every sentence
s <- strsplit(transcripts$file, split = "\n")
sentences <- data.frame(name = rep(transcripts$name, sapply(s, length)), text = unlist(s))
sentences <- sentences %>%
  filter(text != "") %>%
  na.omit()


# building document term matrix at file level
text <- Corpus(VectorSource(data$text))
text <-  tm_map(text, stripWhitespace)
text <-  tm_map(text, content_transformer(tolower))
text <-  tm_map(text, removeWords, stopwords("english"))
text <-  tm_map(text, removePunctuation)
text <-  tm_map(text, removeNumbers)

# Word cloud
set.seed(42)
wordcloud(text, min.freq = 10, max.words = 50, rot.per=0.35, 
          colors = brewer.pal(8, "Blues")[-1:-5])

# topic modeling
n_topics = 6
n_top_words = 10
dtm <- DocumentTermMatrix(text)
dtm <- removeSparseTerms(dtm, 0.99)
ldaOut <- LDA(dtm, k = n_topics)
terms(ldaOut, n_top_words)


ap_topics <- tidy(ldaOut, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(n_top_words, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()



ap_documents <- tidy(ldaOut, matrix = "gamma")
ap_documents

threshold = 0.60
topics_files <- ap_documents %>%
  filter(gamma > threshold)

topics_files %>% 
  rowwise() %>% 
  mutate(name= file_names[as.numeric(document)])



############################################
## Load and transform data
codings <- getCodingTable()[,4:5]
categories <- RQDAQuery("SELECT filecat.name AS category, source.name AS filename 
                         FROM treefile, filecat, source 
                         WHERE treefile.catid=filecat.catid
                         AND treefile.fid=source.id AND treefile.status=1")
# codings <- merge(codings, categories, all.y = TRUE)

head(codings)

# replace filename with year
codings$filename <- str_extract(codings$filename, "[0-9]{4}")

## Open coding
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

ggplot(data=codings, aes(reorder_size(codename))) + geom_bar(stat="count") + 
  facet_grid(~filename) + coord_flip() + 
  theme(legend.position="bottom", legend.title=element_blank()) + 
  ylab("Code frequency in data") + xlab("Code")


##### Applying topic modeling at the sentence level

# building document term matrix at file level
text <- Corpus(VectorSource(sentences$text))
text <-  tm_map(text, stripWhitespace)
text <-  tm_map(text, content_transformer(tolower))
text <-  tm_map(text, removeWords, stopwords("english"))
text <-  tm_map(text, removePunctuation)
text <-  tm_map(text, removeNumbers)


# topic modeling
n_topics = 45
dtm <- DocumentTermMatrix(text)
dtm <- removeSparseTerms(dtm, 0.99)

raw.sum <- apply(dtm,1,FUN=sum) #sum by raw each raw of the table

sentences <- sentences[raw.sum != 0,]
dtm <- dtm[raw.sum != 0,]


ldaOut <- LDA(dtm, k = n_topics, control = list(alpha=0.1))


ap_documents <- tidy(ldaOut, matrix = "gamma")
ap_documents


sentences_likely_topics <- ap_documents %>% 
  filter(gamma > 0.6) %>% 
  rowwise() %>% 
  mutate(name= sentences$text[as.numeric(document)]) %>%
  arrange(topic) %>%
  na.omit()


head(sentences_likely_topics %>% filter(topic == 1) %>% pull(name), n = 15)


head(sentences_likely_topics %>% filter(topic == 2) %>% pull(name), n = 15)


head(sentences_likely_topics %>% filter(topic == 3) %>% pull(name), n = 15)


head(sentences_likely_topics %>% filter(topic == 4) %>% pull(name), n = 15)


head(sentences_likely_topics %>% filter(topic == 5) %>% pull(name), n = 15)
