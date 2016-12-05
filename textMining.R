# Document Processing and Text , "Mining"
# Based on Basic Text Mining in R 
# (https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html)
# With additional exclusions based on preliminary analysis of the message text

# Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)   
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")   

library(tm)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

# Using previous analyses, I built lists of words to avoid

colors <-  c("black", "brown", "blue","gold", "gray", "green"
            ,  "maroon", "dark", "red", "silver", "tan", "white")
carnames <- c("accord", "altima", "buick", "chevrolet", "caravan"
              , "charger", "corolla", "dodge", "door"
              , "elantra", "ford", "honda", "hyundai"
              , "kia", "nissan", "pontiac", "sedan", "suv"
              , "truck", "toyota", "van", "pickup", "chrysler"
              , "gmc", "jimmy")
placenames <- c("area", "areas", "carter", "city" , "commercial"
                , "county", "highway", "houston", "hwy", "district"
                , "mandan", "local", "south", "union", "main", "street")
times <- c("am", "ast","cdt","cst","edt", "est"
           , "hst", "mdt", "mst","now", "pm", "pst", "pdt"
           , "new", "old", "til", "till" )
instructions <- c("advisory", "avoid", "call", "check"
                  , "civil", "closed", "due", "effect"
                  , "emergency", "flag", "issued", "hazard"
                  , "lic", "notice", "open", "order", "place", "please"
                  , "prepare", "take", "update", "use", "watch"
                  , "severe", "test")
organizations <- c("mcem", "media", "nws", "residents"
                   , "sherrif", "suspect", "system", "male")

exclude <- c(colors, carnames, placenames, times
             , instructions, organizations)

if (!file.exists("data/texts")) {
        dir.create("data/texts")
}

docs <- Corpus(DirSource("data/texts/"))
summary(docs)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)

for(j in seq(docs))
{
        docs[[j]] <- gsub("/", " ", docs[[j]])
        docs[[j]] <- gsub("lic[a-z]*", "", docs[[j]])
        docs[[j]] <- gsub("((\\w*)( )*(storm))", "\\2\\4", docs[[j]])
        docs[[j]] <- gsub("((\\w*)(fire) )", "\\2 \\4", docs[[j]])
        docs[[j]] <- gsub("((\\w*)( )*(flood))", "\\2_\\4", docs[[j]])
        docs[[j]] <- gsub("((\\w*)( )*(warning))","\\2", docs[[j]])
        docs[[j]] <- gsub("((\\w*)( )*(alert))|((amber))()(alert)", "\\2_\\4 ", docs[[j]])
        docs[[j]] <- gsub("fireweatherwarningineffectuntil8PMCDT"
                          , "fire weather warning in effect until"
                          , docs[[j]])
        docs[[j]] <- gsub("Sunhumidityhigh"
                          , "sun humidity high"
                          , docs[[j]])
        docs[[j]] <- gsub("evac\\w*", "evacuate", docs[[j]])
}

docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, exclude)
## Not going to stem words due to short messages

# library(SnowballC)
# 
# docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

# Finished preprocessing

dtm <- DocumentTermMatrix(docs)
freq <- colSums(as.matrix(dtm))
ord <- order(freq)

tdm <- TermDocumentMatrix(docs)

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

p <- ggplot(top_n(wf, n=10), aes(word, log(freq))) + 
        geom_bar(stat = "identity") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        coord_flip()

print(p)

 library(wordcloud)

 set.seed(142)
 dark2 <- brewer.pal(6, "Dark2")
 wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)
 
# Associated words - specifying a correlation limit of 0.98   
 
 #findAssocs(dtm, c("mandatory_water"), corlimit=0.9) 
 
 
 
# ## Clustering
# 
# dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
# inspect(dtmss)  
# 
# # Dendrogram
# 
# library(cluster)   
# d <- dist(t(dtmss), method="euclidian")   
# fit <- hclust(d=d, method="ward")   
# fit   
# 
# plot(fit, hang=-1)   
# 
# # K-Means
# 
# library(fpc)   
# d <- dist(t(dtmss), method="euclidian")   
# kfit <- kmeans(d, 2)   
# clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
# 
 