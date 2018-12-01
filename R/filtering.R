#' Filters a tibble representing a sentence and returns matches
#'
#' @param tab a conll represented sentence as a tibble OR a string representation of the sentence which will be converted to a tibble
#' @param column_name e.g. POS, lemma
#' @param column_val what the value of the column should be, e.g N, igrat'
#' @param use_regex if false, exact matches will be searched, if true, regexes
#' @param is_negative if True, will filter only rows NOT matching the condition
#'
#' @return a filtered tibble
#'
#' @export

FilterConllRows <- function(tab, column_name, column_val, use_regex=F, is_negative=F) {
    if(is.character(tab)) {
        tab <- ConllAsTibble(tab)
    }

    if(use_regex) {
        indices <- which(grepl(column_val,tab[[column_name]]))
    }
    else if (length(column_val) > 1) {
        #multiple possible non-regex values
        indices <- which(tab[[column_name]] %in% column_val)
    }
    else {
        indices <- which(tab[[column_name]] == column_val)
    }

    if(length(indices) == 0 & is_negative){
        return(tab)
    }
    else if(is_negative){
        return (tab[-indices, ])
    }
    else{
        return (tab[indices, ])
    }
}


#' Checks if a sentence contains a case where a is governed by b
#'
#' @param headw c(column_name, column_value, [as_regex=T/F]) 
#' @param depw c(column_name, column_value, [as_regex=T/F])
#' @param tab the sentence to look in, as a tibble OR a raw string
#'
#' @export

ContainsDepRel <- function(tab, depw, headw){
    if(is.character(tab)) {
        tab <- ConllAsTibble(tab)
    }
    heads <- FilterConllRows(tab, headw[1], headw[2], 
                             ifelse(length(headw)>2, headw[3], F))
    deps <- FilterConllRows(tab, depw[1], depw[2], 
                             ifelse(length(depw)>2, depw[3], F))

    if(nrow(heads) > 0 & nrow(deps) > 0) {
        governed <- FilterConllRows(deps, "head", heads$tokenid, F)
        if(nrow(governed) > 0) {
            return ( TRUE )
        }
    }
    return ( FALSE )
}


#' Gets the head of a word / words
#'
#'
#' @return a filtered tibble
#'
#' @export

GetHeads <- function(words, sentence) {
    return (FilterConllRows(sentence, "tokenid", words$head))
}

#' Gets the dependents of a word / words
#'
#'
#' @return a filtered tibble
#'
#' @export

GetDeps <- function(words, sentence) {
    return (FilterConllRows(sentence, "head", words$tokenid))
}



#' Applies a user-defined filter to a set of sentences
#' 
#' @param sents a vector of sentences
#' @param filter_funct a user-defined filter function
#' @param return_type 'raw', 'matches', 'both', 'both_pretty': raw returns just a filtered vector, matches returns the actual words that matched, both returns matches and the conll input as a separate variable and both_pretty transforms the conll input to an ordinary human readable sentence
#' 
#' @return depending on the return_type parameter, either a vector of sentences or a tibble of matches
#' 
#' @importFrom dplyr  %>% progress_estimated
#' @export

ApplyConllFilter <- function(sents, filter_funct, return_type="raw") {
    #launch a progress bar from dplyr
    if(return_type == "raw"){
        p <- progress_estimated(length(sents))
        #create a temporary callback
        funct <- function(s){
            matched <- filter_funct(s)
            p$tick()$print()
            #return a truthy value
            return(nrow(matched))
        }
        #Run base R's Filter
        rawmatches <- Filter(funct, sents)
        p$stop()

        return(rawmatches)
    }
    else{
        p <- progress_estimated(length(sents))
        #create a temporary callback
        funct <- function(s){
            matched <- filter_funct(s)
            if(nrow(matched) & return_type == "both"){
                matched$sent <- s
            }
            if(nrow(matched) & return_type == "both_pretty"){
                matched$sent <- ConllAsSentence(s)
            }
            p$tick()$print()
            #return the matched rows
            return(matched)
        }

        #return a combined tibble
        matchlist <- lapply(sents, funct) 
        matchtibble <- do.call(rbind, matchlist)  
        p$stop()
        return(matchtibble)
    }

}

#' Applies a filter to a data frame
#' 
#' @param a tibble / the data frame containing a column with conll annotated data
#' @param funct a user-defined filter function
#' @param sent_col the name of the column with conll data
#' @param return_col if specified, will add a new column to the data frame (called filtered_col) with the value of the specified column in the conll input  (e.g. lemmas of the matched words)
#' @param return_all if return_col as been specified and this is set to TRUE, returns the whole data frame and adds the filtered_col value to every row
#' 
#' 
#' @importFrom dplyr  %>% left_join select mutate
#' @export

ApplyConllFilter_df <- function(tab, funct, sent_col, return_col="", return_all=F){
    if(return_col != ""){
        matched <- ApplyConllFilter(tab[[sent_col]], funct, "both")
        mtab <- matched  %>% 
            select(sent, filtered_col=return_col) 
        mtab[[sent_col]] <- mtab$sent
        mtab <- mtab  %>% select(-sent)
        filtered <- mtab %>% left_join(tab, by=sent_col) 
        if(return_all){
            tab2 <- tab %>% mutate(filtered_col=NA)
            filtered <- tab2[!tab2[[sent_col]] %in% mtab[[sent_col]], ] %>% 
                rbind(., filtered)
        }
    }
    else{
        matched <- ApplyConllFilter(tab[[sent_col]], funct)
        filtered <- tab[tab[[sent_col]]  %in% matched, ]
    }
    return (filtered)
}

