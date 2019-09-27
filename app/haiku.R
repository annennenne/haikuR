
#random haiku
randomHaiku <- function(source = trump, doReturn = FALSE,
                        sentiment = "random") {
  nfive <- sum(source$syllables == 5)
  nseven <- sum(source$syllables == 7) 
  
  fiveprobs <- rep(1/nfive, nfive)
  sevenprobs <- rep(1/nseven, nseven)
  
  if (!(sentiment == "random")) {
    sign <- 1
    if (sentiment == "negative") {
      sign <- -1
    }
    
    fiveprobs <- exp(sign*2*source$sentiment[source$syllables == 5])
    fiveprobs <- fiveprobs/sum(fiveprobs)
    
    sevenprobs <- exp(sign*2*source$sentiment[source$syllables == 7])
    sevenprobs <- sevenprobs/sum(sevenprobs)
  }
  
  fiveindexes <- sample(source$ID[source$syllables == 5], 2, replace = FALSE,
                        prob = fiveprobs)
  sevenindex <- sample(source$ID[source$syllables == 7], 1, 
                       prob = sevenprobs)
  
  firstfive <- source[source$ID == fiveindexes[1], "sentence"]
  sevener <- source[source$ID == sevenindex, "sentence"]
  secondfive <- source[source$ID == fiveindexes[2], "sentence"]
  
  if (doReturn) {
    return(c(firstfive, sevener, secondfive))
  } else {
    cat(paste(firstfive, sevener, secondfive, sep = "\n"))
  }
}