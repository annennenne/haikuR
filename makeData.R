library(sentimentr)
library(data.table)

#function for converting numbers to words
source(url("https://gist.githubusercontent.com/psychemedia/150cb9901529da58124a/raw/a12bfd600af11065255f39787c965b7a4be086d0/numbers2words.R"))

trumptweets_text <- as.data.frame(fread("./data/trumptweets_16-09-19_text.txt", sep = NULL))
trumptweets_id <- read.csv("./data/trumptweets_16-09-19_id.txt", stringsAsFactors = FALSE, sep = "ø", 
                             header = TRUE, colClasses = "character")


trumptweets <- cbind(trumptweets_text, trumptweets_id)

#drop date
trumptweets <- trumptweets[, c("text", "id_str")]

#problem with reading last line. We just drop it:
trumptweets <- trumptweets[-nrow(trumptweets), ]
#trumptweets$ID <- 1:nrow(trumptweets)

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
allsentences <- get_sentences(trumptweets$text)

trump <- data.frame(sentence = unlist(get_sentences(trumptweets$text)),
                    stringsAsFactors = FALSE,
                    id = rep(trumptweets$id_str, times = sapply(allsentences, function(x) sum(length(x))))
                    )



#remove links
#at end of line
trump$sentence <- gsub("https://.{+}$", "", trump$sentence)

#not at end of line
trump$sentence <- gsub("https://.{+}[[:blank:]]", "", trump$sentence)


#count number of syllables in each sentence
#do it in loop in order to add more information if sentence is possibly miscounted, 
#and to manually fix problems due to miscounted numbers
library(quanteda)

trump$syllables <- NA

for (i in 1:nrow(trump)) {
  thisSentence <- trump$sentence[i]
  thisCount <- nsyllable(thisSentence)  
  theseWords <- NULL
  
  #deal with numbers
  if (grepl("[[:digit:]]", thisSentence)) {
    #split into words
    theseWords <- tokens(thisSentence)$text1
    numWords <- as.numeric(theseWords[grepl("[[:digit:]]", theseWords)])
    numCounts <- sum(nsyllable(numbers2words(numWords)))
    
    #note: numbers are counted as zero (NA) in nsyllable
    thisCount <- thisCount + numCounts
  }
  
  #add one to syllable count for "Mr. " 
  if (grepl("Mr.", thisSentence)) {
    if (is.null(theseWords)) {
      theseWords <- tokens(thisSentence)$text1
    }
    mrWords <- sum("Mr." == theseWords)
    
    #Add one to syllable count for every "Mr." occurance
    thisCount <- thisCount + mrWords
  }
  
  trump$syllables[i] <- thisCount 
}

#trump$syllables <- nsyllable(trump$sentence)  

#drop sentences that are empty:
trump <- trump[!is.na(trump$sentence),]
trump <- trump[!(trump$sentence %in% c("", " ")),]


#drop sentences with syllables > 7
trump <- trump[is.na(trump$syllables) | trump$syllables <= 7,]

save(list = "trump", file = "trump_wduplicates.rda")

#remove duplicates
trump <- trump[!duplicated(trump$sentence), ]

save(list = "trump", file = "trump_beforemanualsylcount.rda")







load("trump_aftermanualsylcount.rda")


#drop sentences with syllables not in {5,7}
trump <- trump[trump$syllables %in% c(5,7),]

#add ID number
#trump$ID <- as.character(1:nrow(trump))

#add sentiment score
sentiments <- as.data.frame(sentiment(trump$sentence))
sentiments <- sentiments[!is.na(sentiments$word_count),] 
trump$sentimentscore <- sentiments$sentiment

#save data
save(list = "trump", file = "./data/trumpData.rda")
