## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------

library(depsearcheR)
library(readr)
mytext <- readr::read_file(
                           system.file("extdata", 
                                       "varpunen_wikipedia.txt",
                                       package="depsearcheR")
                           )

cat(substr(mytext,1,300))



## ------------------------------------------------------------------------


library(dplyr)
library(readr)
sentences <- readr::read_file(
                           system.file("extdata", 
                                       "varpunen.conll",
                                       package="depsearcheR")
                           ) %>% 
        strsplit("\n\n")  %>% 
        unlist




## ------------------------------------------------------------------------

sentences <- varpunen_sentences


## ------------------------------------------------------------------------
options("conll_cols" = c("tokenid","token","lemma","feat","none", "head", "dep"))

## ------------------------------------------------------------------------
options("conll_cols" = c("tokenid","token","lemma","pos","pos2","feat","head","dep","null1","null2"))

## ------------------------------------------------------------------------

FilterConllRows(sentences[1], "pos", "NOUN")


## ------------------------------------------------------------------------
FilterConllRows(sentences[1], "feat", "Case=Ine", use_regex=T)

## ------------------------------------------------------------------------
FilterConllRows(sentences[1], "pos", c("NOUN","ADJ"))

## ------------------------------------------------------------------------
FilterConllRows(sentences[1], "pos", c("NOUN","ADJ"), is_negative=T)

## ------------------------------------------------------------------------

FilterConllRows(sentences[1], "pos", c("NOUN","PROPN"),is_negative=T) %>% 
    FilterConllRows("feat", "Case=Ine", use_regex=T) 

## ------------------------------------------------------------------------


#Get all the finite verbs in the sentence
finverbs <- FilterConllRows(sentences[2], "feat", "VerbForm=Fin", T)
#Get their dependents
deps <- FilterConllRows(sentences[2], "head", finverbs$tokenid)
deps
                         

## ------------------------------------------------------------------------


#Get the dependents of finite verbs
deps <- GetDeps(sentences[2],"feat","VerbForm=Fin", T)
deps
                         

## ------------------------------------------------------------------------

ContainsDepRel(sentences[1],
             depw=c("pos","ADJ"),
             headw=c("pos","NOUN")
             )



## ------------------------------------------------------------------------

sentences  <- GetSentencesFromFile("../inst/extdata/varpunen.conll")


## ------------------------------------------------------------------------

MyFilterFunction <- function(sentence) {
            my_matches <- 
                GetDeps(sentence,"feat","VerbForm=Fin", T) %>% 
                FilterConllRows("dep","nsubj") %>% 
                FilterConllRows("pos","PROPN") 

            return(nrow(my_matches))
} 



## ------------------------------------------------------------------------

matched_sentences <- Filter(MyFilterFunction, sentences)


## ------------------------------------------------------------------------

ConllAsTibble(matched_sentences)



## ------------------------------------------------------------------------

ConllAsSentence(matched_sentences)


## ------------------------------------------------------------------------
library(dplyr)

#initialize the progress bar
p <- progress_estimated(length(sentences))

matched_sentences <- Filter(
            function(sentence) {
                my_matches <- 
                    GetDeps(sentence,"feat","VerbForm=Fin", T) %>% 
                    FilterConllRows("dep","nsubj") %>% 
                    FilterConllRows("pos","PROPN") 
                #Move the progress bar:
                p$tick()$print()
                return(nrow(my_matches))
            }, sentences)

#Remove the progress bar
p$stop()


## ------------------------------------------------------------------------

options("conll_cols" = c("tokenid","token","lemma","feat","none", "head", "dep"))
sentences  <- GetSentencesFromFile("../inst/extdata/gospel_of_john.txt.conll")

#initialize the progress bar
p <- progress_estimated(length(sentences))

matched_sentences <- Filter(
            function(sentence) {
                my_matches <- 
                    GetDeps(sentence,"feat","VBZ", T) %>% 
                    FilterConllRows("dep","nsubj") %>% 
                    FilterConllRows("feat","NN") 
                #Move the progress bar:
                p$tick()$print()
                return(nrow(my_matches))
            }, sentences)

#Remove the progress bar
p$stop()


## ------------------------------------------------------------------------

#library(rtweet)
#ref <- search_tweets("(#UCL OR #ChampionsLeague OR #UEFAChampionsLeague) referee",  n = 5000)


## ------------------------------------------------------------------------

ucl_ref


