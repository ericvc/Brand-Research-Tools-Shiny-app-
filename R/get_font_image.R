#Function to scrape direct link to image displaying specified font
get_font_image <- function(font){
  
  if(grepl("\\s{1,}",font)){
    font <- gsub("\\s{1,}","-",font)
  }
  
  url <- sprintf("https://www.fontsquirrel.com/fonts/%s", tolower(font))
  headers = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:65.0) Gecko/20100101 Firefox/65.0"
  response <- httr::GET(url, config = httr::add_headers("user-agent"=headers))
  
  if(response$status_code == 200){
    
    linkToImage <- response %>%
      read_html %>%
      html_nodes(sprintf("img[alt='%s Regular free font']",stringr::str_to_title(font))) %>%
      html_attr("src")
    
    return(paste0("<a href='", linkToImage, "'>", "Click here for example text", "</a>"))
    
  } else{
    
    return("No example found")
    
  }
  
}