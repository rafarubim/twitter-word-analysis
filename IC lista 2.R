# Rafael Rubim Cabral - 1511068

#install.packages("twitteR")
#install.packages("stringr")
#install.packages("RTextTools")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("cluster")
#install.packages("fpc")

library(wordcloud)
library(twitteR)
library(RTextTools)
library(tm)
library(stringr)
library(cluster)
library(fpc)

# Access was revoked
consumerKey = ""
consumerSecret = ""
accessToken = ""
accessSecret = "" 

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)

tweets = searchTwitter('politica', n = 200, geocode = "-22.90278,-43.2075,60km")

tweetsFrame = twListToDF(tweets)

textosTweets = tweetsFrame$text

# Get stop words, include 'RT' >>> Stopwords do arquivo abaixo são mais completas
#stpWords = stopwords(kind = "portuguese")
# Colocar local do arquivo
setwd('F:\\Rafael\\Projetos\\R') # <<<<<<<<<<<<<<<<<<<<<<<<<< CHANGE THIS LINE TO THE PATH OF YOUR STOPWORDS.TXT
fil = file("stopwords.txt")
stpWords = readLines(fil)
close.connection(fil)
stpWords = c(stpWords, 'rt')

vecTexts = vector()
baseDtm = data.frame(dummyCol = 0)
allWords = vector()

for (texto in textosTweets) {
  # Tirar urls
  texto = str_replace_all(texto, "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)", " ")
  # Tirar símbolos que invalidam palavras, como: @s, #s, …s e palavras com letras
  texto = str_replace_all(texto, "(\\w*[0-9]\\w*|[@#]\\w*|\\w*…)", " ")
  # Tirar o que não é palavra
  texto = str_replace_all(texto, "[-_:\\+\\/.;=,\\<>()\\[\\]{}“”!?~`'\"´^]", " ")
  # Tirar emojis
  texto = str_replace_all(texto, "[\\x{1F000}-\\x{1F9FF}]", " ")
  # Transformar em minúsculas
  texto = tolower(texto)
  # Tirar espaços em branco
  texto = str_replace_all(texto, "\\s+", " ")
  # Trim
  texto = str_replace_all(texto, "(^\\s+|\\s+$)", "")
  # Remover stopwords
  strNoStop = removeWords(texto, stpWords)
  # Tirar espaços em branco
  strNoStop = str_replace_all(strNoStop, "\\s+", " ")
  # Trim
  strNoStop = str_replace_all(strNoStop, "(^\\s+|\\s+$)", "")
  
  # Split e stemming
  listWords = strsplit(strNoStop, split = " ")
  stemRes = wordStem(listWords[[1]], language = "portuguese")
  
  # Get each different stemmed word to be used as a dataframe column
  for (word in stemRes) {
    if (is.null(baseDtm[[word]])) {
      allWords = c(allWords, word)
      baseDtm[[word]] = 0
    }
  }
  
  # Back to full text os stemmed words
  fullText = paste(stemRes, collapse = " ")
  
  # Add text to texts vector
  vecTexts = c(vecTexts, fullText)
}

baseDtm$dummyCol = NULL

# Copy the base DTM so the column names can be kept
# dtm is the final dtm
dtm = baseDtm

for (text in vecTexts) {
  wordsInText = strsplit(text, " ")
  tempDtm = baseDtm
  for (word in wordsInText[[1]]) {
    tempDtm[[word]] = tempDtm[[word]] + 1
  }
  dtm = rbind(dtm, tempDtm)
}

# Remove first row which only contains 0s
dtm = dtm[-1,]
rownames(dtm) = 1:nrow(dtm)

print("DTM:")
print(dtm)
termFrequencies = colSums(dtm)
orderedTermFrequencies = sort(termFrequencies, decreasing = TRUE)
print("Term frequencies (ordered by most frequent):")
print(orderedTermFrequencies)

moreThen40Terms = termFrequencies[termFrequencies>=40]

# Histograma das frequências das frequências de termos
hist(moreThen40Terms)

# Histograma da frequências de cada termo
barplot(moreThen40Terms)

moreThen20Terms = termFrequencies[termFrequencies>=20]
moreThen20TermNames = names(as.list(moreThen20Terms))

# (I only found out later there's a simpler way to do what I did above)
docs_tm = VCorpus(VectorSource(vecTexts))
dtm_tm = DocumentTermMatrix(docs_tm)

ouroStemmed = wordStem("ouro", language = "portuguese")

# Correlated terms to "ouro" by more then 60%
correlatedTerms = findAssocs(dtm_tm, ouroStemmed, corlimit=0.60)

# Wordcloud of terms with more then 20 frequency
wordcloud(moreThen20TermNames, moreThen20Terms)

# Same wordcloud but colored
wordcloud(moreThen20TermNames, moreThen20Terms, colors=1:10, random.color=TRUE)

dtm_tm_notSparce = removeSparseTerms(dtm_tm, 0.15)
distanceWords = dist(t(dtm_tm), method="euclidian")
fit <- hclust(d=distanceWords, method="complete")

# Draw the dendrogram
plot(fit, hang=-1)
# From the dendrogram above, we find appr 9 clusters to be a good number
groups <- cutree(fit, k=9)
# Draw the boxes to adequate to clearly see the clusters
rect.hclust(fit, k=9, border="red")

# K-means clusters
kMeansFit = kmeans(distanceWords, 9)
clusplot(as.matrix(distanceWords), kMeansFit$cluster, color=T, shade=T, labels=2, lines=0)
