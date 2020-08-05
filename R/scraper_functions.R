get_text_body <- function(Url) {
  
  ##Get response from input URL
  response = xml2::read_html(Url)
  
  # response_body1 = get_H_tags(Url) %>% 
  #   paste0(., collapse=" ") %>% 
  #   tolower()
    
  response_body = response %>%
      rvest::html_nodes("p") %>%
      rvest::html_text() %>%
      stringr::str_replace_all(., "[\r\n\t]" , " ") %>% # remove all space and new lines
      paste0(., collapse = " ") %>%
      gsub("\\r\\t", " ", .) %>% #removes spaces in text strings
      gsub("@\\w+", "", .) %>% #removes username tags
      gsub("[[:punct:]]", " ", .) %>% #removes punctuation
      gsub("http\\w+", " ", .) %>% #removes links
      gsub("[\n]", " ", .) %>% #replaces tabs with blank space
      #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
      tm::removeWords(., tm::stopwords()) %>% 
      gsub("\\s{2,}", " ", .) %>% #replaces blanks with a single blank space
      gsub("^ ", " ", .) %>% #removes# blank at beginning of string
      gsub("\\s+\\$", " ", .)  %>% #removes blank at end of string
      gsub("\\s+[a-zA-Z]\\s+","", .) %>% #removes isolated characters
      gsub("^[a-zA-Z]$","", .) %>% #removes isolated characters
      tolower
      
  # response_body = paste0(response_body1, response_body2, collapse = " ")
      
  return(response_body)

}

get_text_title <- function(Url) {
  ##Get response from input URL
  response = xml2::read_html(Url)
  response_title = response %>%
    rvest::html_nodes("title") %>%
    rvest::html_text() %>%
    stringr::str_replace_all(., "[\r\n\t]" , " ") %>%
    gsub("rt", "", .) %>% #removes spaces in text strings
    gsub("@\\w+", "", .) %>% #removes spaces in text strings
    gsub("[[:punct:]]", "", .) %>% #removes punctuation
    gsub("http\\w+", "", .) %>% #removes links
    gsub("[\n]", " ", .) %>% #replaces tabs with blank space
    #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
    tm::removeWords(., tm::stopwords()) %>% 
    gsub("\\s{2,}", " ", .) %>% #replaces blanks with a single blank space
    gsub("^\\s+", " ", .) %>% #removes# blank at beginning of string
    gsub("\\s+\\$", " ", .) %>% #removes blank at end of string
    gsub("\\s+[a-zA-Z]\\s+","", .) %>% #removes isolated characters
    gsub("^[a-zA-Z]$","", .) %>% #removes isolated characters
    tolower
  
  return(response_title)
}

formatDomain = function(url){

  #Remove prefix from url
  stringr::str_remove_all(url, pattern = "^(http[s]?://www\\.|http[s]?://|www\\.)") %>%
    stringr::str_trunc(width=35) #limit to 35 characters

}
