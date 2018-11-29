#!/usr/bin/Rscript
#' Trying to implement a simple ability to query dependency annotated data in
#' r


#'  Gets the source data from json
#' 
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export

GetDataFromSource <- function(data_folder){
    cat("Fetching the data, please wait", "\n")
    raw_data = fromJSON(paste0(data_folder, "parsed_sample.json"))
    mydata <- list()

    for(verb in names(raw_data)){
        mydata[[verb]] <- unlist(raw_data[[verb]])
    }

#     names(mydata) <- c("добавить", "брать", "искать", "играть",
#                        "использовать", "купить", "читать", "хотеть", "найти",
#                        "направить", "чувствовать", "думать", "обеспечить",
#                        "иметь", "давать", "любить", "дать", "написать",
#                        "называть", "говорить", "назвать", "начать", "делать") 
    return (mydata)
}


#'  Processes each verb in order to find what we're looking for
#' 
#' @importFrom pbapply pblapply
#' @importFrom dplyr %>% 
#' @importFrom readr write_lines
#' 
#' @export

ProcessAllData <- function(thisdata){
    return (
            pblapply(names(thisdata),function(verb,alldata){
                     write_lines(paste0("Processing ",
                                        verb,
                                        " (of total ",  
                                        length(names(thisdata)),
                                        " verbs )"),
                                 "/tmp/sim_dep_search.log",append=T)
                    pblapply(alldata[[verb]],ProcessThisVerb,verb=verb) %>% return
                },
                alldata=thisdata
             )  %>%  setNames(.,names(thisdata))
            )
}

#'  Processes each concordance including this verb 
#' 
#' 
#'  @importFrom pbapply pblapply
#' 
#' @export

ProcessThisVerb <- function(concordances, verb){
    mylist <- lapply(concordances,ProcessThisConcordance, verb=verb)
    return (mylist)
}


#'  Processes each concordance including this verb 
#' 
#' @export

ConcAsTibble <- function(concordance, verb){
    conll_cols <- c("tokenid","token","lemma","pos","pos2","feat","head","dep","null1","null2")
    tab <- read.csv(text=concordance,sep="\t",row.names=NULL)  
    colnames(tab) <- conll_cols
    return(GetBigram(tab, verb))
}


#'  Searches for the verb and  its complement
#' 
#' @param tab the concordance as a data frame
#' 
#' @importFrom dplyr %>% filter tibble select
#' 
#' @export

GetBigram <- function(tab, verb){
    verb_idx <- which(tab$lemma==verb)
    verb_tokenid <- tab[verb_idx, "tokenid"]
    ret  <- 0
    if(length(verb_idx)>0){
        deps <- tab %>% filter(head==verb_tokenid,dep=="1-компл",pos!="S") 
        if(nrow(deps)>0){
            return(tibble(
                        verblemma = verb,
                        deptopken = deps[1,"token"],
                        deplemma = deps[1,"lemma"],
                        verbfeat = tab$feat[verb_idx],
                        depfeat = deps[1,"feat"],
                        order = ifelse(verb_idx > deps[1,"tokenid"], "OV", "VO"),
                        bigram = paste("не", tab$token[verb_idx], deps[1,"token"]),
                        bigram_ordered = ifelse(verb_idx > deps[1,"tokenid"], 
                                                paste("не", tab$lemma[verb_idx], deps[1,"token"]),
                                                paste(deps[1,"token"], "не", tab$lemma[verb_idx])),
                        bigram_lemma = paste("не", tab$lemma[verb_idx], deps[1,"token"]),
                        context = paste(tab$token, collapse=" ")
                        )
                    )
        }
    }
    return(NULL)
}

#' Converts the list of tibbles to one single tibble
#' 
#' @param mylist the list to start with
#' 
#' @importFrom dplyr %>% 
#' 
#' @export

TidyList <- function(mylist){
    mytibble <- tibble()
    i<-0
    for(item in mylist){
        item[[1]]  %>% rbind(.,mytibble) -> mytibble
    }
    return(mytibble)
}

#' Adds some more annotations based on the existing variables
#' 
#' @param mylist the list to start with
#' 
#' @importFrom dplyr %>% filter select mutate case_when
#' 
#' @export

AddPredictors <- function(mytibble){
    mytibble %>% 
        mutate(tense=substr(verbfeat,4,4))  %>% 
        mutate(pos=substr(depfeat,1,1))  %>% 
        mutate(gender=case_when(
                              pos=="N" ~ substr(depfeat,3,3),
                              TRUE ~ "unspecified"
                              )
        ) %>% 
        mutate(case=case_when(
                              pos=="P" ~ substr(depfeat,6,6),
                              TRUE ~ substr(depfeat,5,5) 
                              )
        ) %>% 
        mutate(animate=case_when(
                                 pos=="P" ~ substr(depfeat,8,8),
                                 TRUE ~ substr(depfeat,6,6)
                                 )
        )  %>% 
        mutate(tense=case_when(tense=="s" ~ "past",
                               tense=="p" ~ "present",
                               TRUE ~ "none"
                               ),
               case =case_when(
                               case %in% c("n","a") ~ "acc",
                               case %in% c("g") ~ "gen",
                               case %in% c("i") ~ "instr",
                               case %in% c("d") ~ "dat",
                               case %in% c("l") ~ "prep",
                               TRUE ~ "other"
                               )
               )  %>% 
        filter (!case %in% c("dat","instr","prep"))  %>% 
        filter (!pos  %in% c("M","Q","V","R","C","A",",","-"))  %>% 
        filter (animate != "y")  %>% 
        filter (!(pos=="P" & case == "other"))  %>% 
        return
}


#' Just a wrapper to  combine all the data gathering functions
#'
#'
#' @param mydata raw list of data from the json file produced by inst/python/.....py
#'
#' @export

CompileAllData <- function(mydata){
    #mydata <- GetDataFromSource("~/drive/work/tutkimus/data/usage_based/")
    ProcessAllData(mydata)  %>% 
        pblapply(., TidyList)  %>% 
        do.call(rbind,.) %>% 
        AddPredictors  %>% 
        return
}

#bigramlist <- ProcessAllData(mydata[c(2,3)])
#bigramlist_cleaned <- pblapply(bigramlist, TidyList)
#bigrams <- do.call(rbind,bigramlist_cleaned) %>% AddPredictors








