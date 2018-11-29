
#' Returns a human readable representation of the sentence
#'
#' @param tab the raw repreentation of a sentence or a tibble
#' @return the sentence in a human readable form (as a string)
#'
#' @export

ConllAsSentence <- function(tab){
    if(is.character(tab)) {
        tab <- ConllAsTibble(tab)
    }
    tab$token <- as.character(tab$token)

    punct_and_space <- c(".","?","!",";",":",",")
    no_space_before <- c("(")
    no_space_after <- c(")")
    vec <- c()
    for(i in c(1: length(tab$token))){
        thistoken <- tab$token[i]
        nexttoken <- ""
        prevtoken <- ""
        space <- " "
        if(length(tab$token) >= i +1) {
            nexttoken <- tab$token[i + 1]
        }
        if(i > 1){
            prevtoken <- tab$token[i-1]
        }
        if(nexttoken %in% c(")",".",",",";",":","\"") | 
           thistoken %in% c("(") |
           i == length(tab$token)) {
            space <- ""
        }
        thistoken <- tab$token[i]
        output <- thistoken
        vec <- c(vec,paste0(output, space))
    }
    return (paste(vec, collapse="",sep=""))
}

