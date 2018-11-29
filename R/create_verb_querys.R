
#' Reads the frequency list from a file and returns a tibble
#'
#' @importFrom dplyr %>% filter as_tibble select
#'
#' @export

GetFrequencyListFromSource <- function(data_folder){
    read.table(paste0(data_folder, "verb_freqs1.txt"),sep="\t",header=T)  %>% 
        as_tibble  %>% 
        select(lemma,Frequency)  %>% 
        filter(!grepl("ся$", lemma)) %>% 
        #head(.,500)  %>% 
        return
}

#' Filters the manually filtered list of verbs a little more
#'
#' @importFrom dplyr %>% filter select slice
#'
#' @export

TakeSampleFromFlist <- function(verbs_filtered){
    verbs_filtered  %>% 
        filter(filter=="y", lemma != "работать") %>% 
        select(-Frequency.y)  %>% 
        slice(1:100)  %>% 
        return
}


#' Creates a list for querying the aranea corpora
#'
#' @importFrom dplyr %>% mutate
#' @importFrom readr write_lines
#' @importFrom stringi stri_trans_general
#'
#' @export

MakeQueryForCrawler <- function(verbs_sample, data_folder){
    verbs_sample  %>% 
        mutate(query=paste0('\'[lc="не"][lemma="', lemma,'"]\''))   %>% 
        mutate(name=stri_trans_general(lemma,'cyrillic-latin'))  %>% 
        mutate(name=stri_trans_general(name,'latin-ascii'))  %>% 
        mutate(name=gsub("'","",name))  %>% 
        apply(., 1, function(r)
                paste(r[["name"]],r[["query"]],sep=": ")
              )  %>% 
        paste(.,sep="\n") %>% 
        write_lines(.,path=paste0(data_folder,"verb_queries.yaml"))
}

#Huom, myös сь
#read.csv("/tmp/bu.csv",sep="|")  %>%  as_tibble  %>% left_join(., verbs_filtered,by="lemma") -> verbs_filtered
#verbs_filtered$filter <- pbapply(verbs_filtered,1,CheckSample_df,
#                                  cols_to_show=c("lemma","Frequency"))
