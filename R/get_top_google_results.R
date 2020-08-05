#Get the top 1 <= n <= 20 results from a google frontpage search
get_top_google_results <- function(query, num_results=20, platform="desktop"){
  
  query. <- query %>%
    str_trim() %>%
    str_replace_all(pattern="\\s{1,}", replacement = "+")
  
  if(platform == "desktop"){
    headers = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:65.0) Gecko/20100101 Firefox/65.0"
  } else{
    # mobile user-agent
    headers = "Mozilla/5.0 (Linux; Android 7.0; SM-G930V Build/NRD90M) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.125 Mobile Safari/537.36"
  }
  
  Url <-  paste0("https://www.google.com/search?q=",query.,"&num=",num_results)
  
  response <- httr::GET(Url, config = httr::add_headers("user-agent"=headers))
  
  if(response$status_code == 200){
    
    pageTitle <- response %>%
      read_html %>%
      html_nodes("div[class='r']") %>%
      html_nodes("h3") %>%
      html_text()
    
    pageLinks <- response %>%
      read_html %>%
      html_nodes("div[class='r']") %>%
      html_node("a") %>%
      html_attr("href")
    
    #pageLinks <- pageLinks[pageLinks!="#"
    data <- as_tibble(cbind(pageTitle,pageLinks))
    return(data)
    
  } else{
    warning("Status code returned was not 200 (",response$status_code,")")
  }
  
}