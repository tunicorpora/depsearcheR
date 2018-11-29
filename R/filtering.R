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

    if(is_negative){
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

#' Gets the dependends of words specified by a condition
#'
#' @param tab a conll represented sentence as a tibble
#' @param column_name e.g. POS, lemma
#' @param column_val what the value of the column should be, e.g N, igrat'
#' @param use_regex if false, exact matches will be searched, if true, regexes
#' @param is_negative if True, will filter only rows NOT matching the condition
#'
#' @return a filtered tibble
#'
#' @export

GetDeps <- function(tab, column_name, column_val, use_regex=F, is_negative=F) {
    if(is.character(tab)) {
        tab <- ConllAsTibble(tab)
    }
    heads <- FilterConllRows(tab, column_name, column_val, use_regex, is_negative)
    return (FilterConllRows(tab, "head", heads$tokenid))
}

