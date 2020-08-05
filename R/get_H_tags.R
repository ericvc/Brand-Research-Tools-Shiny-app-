get_H_tags <- function(Url){
  
  ##Get response from input URL
  response = xml2::read_html(Url)
  
  ##Parse and re-format page content to calculate properties of interest
  htags = paste0("h", 1:6)
  text = sapply(htags, function(htag) {
    response %>%
      rvest::html_nodes(htag) %>%
      rvest::html_text() %>%
      stringr::str_replace_all(., "[\r\n\t]" , " ") %>% # remove all space and new lines
      paste0(., collapse = " ") %>%
      gsub("@\\w+", "", .) %>% #removes spaces in text strings
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
    
  })
  
  return(text)
}
