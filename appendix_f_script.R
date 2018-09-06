#devtools::install_github("tidyverse/googledrive")
#install.packages("quanteda")

# helper libraries to 


#drive_files <- googledrive::drive_find(team_drive = googledrive::team_drive_find("legal_rep"))

#library(googledrive)

library(dplyr)

drive_txt_to_memory <- function (file) 
{

  file <- googledrive::as_dribble(file)
  file <- googledrive::confirm_single_file(file)
  request <- googledrive::generate_request(endpoint = "drive.files.export", 
                              params = list(fileId = file$id, mimeType = "text/plain"))
  response <- googledrive::make_request(request)
  
  processed_response <- httr::content(response, encoding = "UTF-8")
  
  processed_response
  
}

# helper function to parse by sentence
split_by_sentence <- function (text) {
  
  # split based on periods, exclams or question marks
  result <- unlist(strsplit(text, split = "[\\.!?]+"))
  
  # do not return empty strings
  result <- stringi::stri_trim_both(result)
  result <- result[nchar(result) > 0]
  
  # ensure that something is always returned
  if (length(result) == 0)
    result <- ""
  
  result
  
}


get_readability <- function(...){
  
  # get drive files
  drive_files <- googledrive::drive_find(...)
  
  # list of ids to search for
  files_list <- drive_files$id
  
  # pre-populate a list to hold fk values 
  fk_list <- vector("list", length(files_list))
  fe_list <- vector("list", length(files_list))
  
  for (i in 1:length(files_list)) {
    file <- drive_txt_to_memory(googledrive::as_id(files_list[i]))
    # vector of sentences from file
    file_sentences <- split_by_sentence(file)
    # vector of words from file 
    file_words_raw <- unlist(sapply(file, strsplit, "\\s+", USE.NAMES = FALSE))
    # vector of syllables in each word in file 
    file_syllables <- quanteda::nsyllable(quanteda::tokens(file_words_raw, remove_punct = TRUE))
    # total sentences in each file 
    ttl_sentences <- length(file_sentences)
    # total sylllables in each file 
    ttl_syllables <- file_syllables %>% unlist %>% sum(na.rm = TRUE)
    # total words in each file 
    ttl_words <- file_syllables %>% unlist %>% is.na %>% sum(!.)
    
    # formulas below taken from https://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_tests
    # grade-level 
    flesch_kincaid_grade_level <- 0.39*(ttl_words/ttl_sentences) + 11.8*(ttl_syllables/ttl_words) - 15.59
    # reading ease 
    flesch_ease <- 206.835-1.015*(ttl_words/ttl_sentences) - 84.6*(ttl_syllables/ttl_words)
    
    fk_list[[i]] <- flesch_kincaid_grade_level
    fe_list[[i]] <- flesch_ease
    
  }
  
  names(fk_list) <- drive_files$name
  names(fe_list) <- drive_files$name
  
  data_frame(`Flesch-Kincaid Grade Level` = unlist(fk_list)
             ,`Flesch Ease Level` = unlist(fe_list)
             ,`document_title` = drive_files$name) %>%
    mutate(document_title = stringr::str_replace_all(document_title, "_", " ")
           ,document_title = tools::toTitleCase(document_title)) %>%
    rename(`Document Title` = document_title) %>%
    knitr::kable(digits = 1, "latex", booktabs = T, caption = "Readability, by Document") %>%
    kableExtra::kable_styling(position = "center")
  
}




