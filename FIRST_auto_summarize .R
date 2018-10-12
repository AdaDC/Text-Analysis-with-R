# Install packages.  PDF tools scrapes the document, lexRankr extracts the most important sentences. 
# install.packages("pdftools")
# install.packages("lexRankr")
# install.packages("tidyverse") # for quick data manipulation

# First download the file and save as "surveyresults.pdf"
url <- "https://www.collegeparkmd.gov/document_center/Admin/CityManager/Communications/College%20Park%20Community%20Survey%20Report%20FINAL%20web.pdf"
download.file(url, "surveyresults.pdf")
# Use the pdftools package to open pdf
library(pdftools)
txt <- pdf_text("surveyresults.pdf") # entire text of document
toc <- pdf_toc("surveyresults.pdf") # table of contents of the document
# Extract the sections of the survey results
out <- vector("list", length = 8)
for (i in 1:8) {
        out[[i]] <- toc[2]$children[[5]]$children[[i]]$title
}
survey.sections <- unlist(out) #contains the 8 topics that we should focus on

# Use lexRankr package to summarize text for a section
library(lexRankr)
# This function will pull the top sentences for each section.  Enter the number of sentences to pull, and the document.  Can be used on sections of document.
get_summary <- function(document, totalsentenceswanted = 5) {
        top_10 = lexRankr::lexRank(document,
                                   #only 1 article; repeat same docid for all of input vector
                                   docId = rep(1, length(document)),
                                   #return 3 sentences to mimick /u/autotldr's output
                                   n = totalsentenceswanted,
                                   continuous = TRUE)
        
        #reorder the top 3 sentences to be in order of appearance in article
        order_of_appearance = order(as.integer(gsub("_","",top_10$sentenceId)))
        #extract sentences in order of appearance
        ordered_top_10 = top_10[order_of_appearance, "sentence"]
        return(ordered_top_10)
}

# Pull summaries for each section
library(tidyverse)
survey.sections[1] # on pages 5-10
x <- get_summary(txt[5:10])
(gsub("\n", " ", x)) %>% 
        paste(collapse = " ")

survey.sections[2] # on pages 11-15 
y <- get_summary(txt[11:15], 8)
(gsub("\n", " ", y[c(1,4,7)])) %>% 
        paste(collapse = " ")

survey.sections[3] # on pages 16-18
z <- get_summary(txt[16:18], 10)
(gsub("\n", " ", z[c(1,2,3,7,9)])) %>% 
        paste(collapse = " ")

survey.sections[4] # on pages 19-20
a <- get_summary(txt[19:20], 10)
(gsub("\n", " ", a[c(1,4,5,7)])) %>% 
        paste(collapse = " ")

survey.sections[5] # on pages 21-27
b <- get_summary(txt[21:27], 10)
(gsub("\n", " ", b[c(1,3, 5, 9)])) %>% 
        paste(collapse = " ")

survey.sections[6] # on pages 28-29
c <- get_summary(txt[28:29], 10)
(gsub("\n", " ", c[c(1,2, 3, 4, 6)])) %>% 
        paste(collapse = " ")

survey.sections[7] # on page 30
d <- get_summary(txt[30], 10)
(gsub("\n", " ", d[c(1:6)])) %>% 
        paste(collapse = " ")

survey.sections[8] # on page 31
e <- get_summary(txt[31], 1)
(gsub("\n", " ", e[1])) %>% 
        paste(collapse = " ")


