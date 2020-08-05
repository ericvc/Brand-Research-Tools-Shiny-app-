#Function to check inputted URLs and reformat them if needed
input_urls <- function(input){
  
  urls = c(input$site1,
           input$site2,
           input$site3,
           input$site4,
           input$site5)
  urls = urls[urls != ""]
  
  #check for transfer protocol prefix
  l <- sapply(urls, function(x) grepl(pattern = "^https?://", x = x) )
  if(any(!l)){
    urls[!l] <- paste0("https://",urls[!l])
  }
  
  return(urls)
}
