library(sentimentr)


trumptweets <- read.csv("./data/trumptweets.csv", stringsAsFactors = FALSE, sep = "ø", 
                        header = TRUE)
trumptweets$ID <- 1:nrow(trumptweets)

#change \n to whitespace
trumptweets$text <- gsub("\n", " ", trumptweets$text)

#remove entries only consisting of links
trumptweets <- trumptweets[!(trumptweets$text %in% c("", " ")),]  

#remove @ and #
trumptweets$text <- gsub("@", "", trumptweets$text)
trumptweets$text <- gsub("#", "", trumptweets$text)

#replace &amp; with &
trumptweets$text <- gsub("&amp;", "&", trumptweets$text)

#split into sentences
#library(tokenizers)
#trump <- data.frame(sentence = unlist(tokenize_sentences(trumptweets$text)),
#                    stringsAsFactors = FALSE)
trump <- data.frame(sentence = unlist(get_sentences(trumptweets$text)),
                    stringsAsFactors = FALSE)



#remove links
#at end of line
trump$sentence <- gsub("https://.{+}$", "", trump$sentence)

#not at end of line
trump$sentence <- gsub("https://.{+}[[:blank:]]", "", trump$sentence)


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

#save data
save(list = "trump", file = "./data/trumpData.rda")
