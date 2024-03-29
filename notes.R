library(sentimentr)


trumptweets <- read.csv("./data/trumptweets.csv", stringsAsFactors = FALSE, sep = "ø", 
                      header = TRUE)
trumptweets$ID <- 1:nrow(trumptweets)

#remove links
  #at end of line
  trumptweets$text <- gsub("https://.{+}$", "", trumptweets$text)
  
  #not at end of line
  trumptweets$text <- gsub("https://.{+}[[:blank:]]", "", trumptweets$text)

  #remove entries only consisting of links
  trumptweets <- trumptweets[!(trumptweets$text %in% c("", " ")),]  


#replace &amp; with &
  trumptweets$text <- gsub("&amp;", "&", trumptweets$text)
    
#split into sentences
#library(tokenizers)
#trump <- data.frame(sentence = unlist(tokenize_sentences(trumptweets$text)),
#                    stringsAsFactors = FALSE)
trump <- data.frame(sentence = unlist(get_sentences(trumptweets$text)),
                    stringsAsFactors = FALSE)

#count number of syllables in each sentence
library(quanteda)
trump$syllables <- nsyllable(trump$sentence)  

#drop sentences with syllables not in {5,7}
trump <- trump[trump$syllables %in% c(5,7),]

#add ID number
trump$ID <- as.character(1:nrow(trump))

#add sentiment score

sentiments <- as.data.frame(sentiment(trump$sentence))
sentiments <- sentiments[!is.na(sentiments$word_count),] 
trump$sentimentscore <- sentiments$sentiment
