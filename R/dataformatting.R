
#'  Makes a tibble of the raw conll string provided. Assumes one sentence.
#' 
#' @param conll the raw string representing a sentence
#' @param conll_cols the structure of the conll (names of columns)
#' @importFrom dplyr as_tibble
#' 
#' @export

ConllAsTibble <- function(conll){
    conll_cols <- getOption("conll_cols", default=c("tokenid","token","lemma","pos","pos2","feat","head","dep","null1","null2"))
    tab <- read.csv(text=conll,sep="\t",row.names=NULL, header=F)  
    colnames(tab) <- conll_cols
    return (as_tibble(tab))
}



#' A convenience function for getting a vector of sentences
#' 
#' @param filename path to the source file to be used
#' @importFrom readr read_file
#' @export

GetSentencesFromFile <- function(filename) {
    rawtext <- readr::read_file(filename)
    sents <- unlist(strsplit(rawtext, "\n\n"))
    return(sents)
}
