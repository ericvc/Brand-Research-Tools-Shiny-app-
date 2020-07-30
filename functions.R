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

#Plot layout function - takes the number of URLs entered into the sidebar as an argument. Returns the
#plotting dimensions for the resulting visualizations.
plf_wc <- function(n){
  dim = c()
  if(n==1) dim = c(1,1) * 350 
  if(n==2) dim = c(1,2) * c(320,450)
  if(n==3) dim = c(1,3) * c(320,350)
  if(n==4) dim = c(2,2) * c(320,450)
  if(n==5) dim = c(2,3) * c(320,350)
  return(dim)
}

#Function for horizontal placement of textInput objects.
textInputRow <- function(inputId, label, value = ""){
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

#Function for horizontal placement of numericInput objects.
numericInputRow <- function(inputId, label, value = ""){
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "numeric", value = value,class="input-small"))
}

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

font_function <- function(fonts, URL){
  
  ## check function input
  assert_that(not_empty(fonts), noNA(fonts),
              is.string(URL), not_empty(URL), noNA(URL))
  
  ## Example from input fonts
  examples = list()
  for (i in 1:length(fonts)) {
    #examples[i] = tags$p(style = paste0('font-family:',fonts[i],';font-size:25px', ' >Example Text'))
    examples[[i]] = get_font_image(fonts[i])
  }
  ## create data table from input fonts
  dat <- data.frame(URL = rep(URL, length(fonts)), Fonts = fonts)
  dat$Example <- examples
  
  return(dat)
  
}

web_colors <- function(Url,
                       num_colors = 10,
                       seed = 123,
                       color_names=FALSE,
                       zoom = "Normal") {
  
  #check for transfer protocol prefix
  if(!grepl(pattern = "^https?://", x = Url) ){
    Url <- paste0("https://",Url)
  }

  #location of temp file for website screenshot
  destFile <- paste0("temp/webshot/",
                     paste(
                       xml2::url_parse(Url)$server,
                       xml2::url_parse(Url)$path,
                       zoom,
                       sep = "-"
                     ),
                     ".jpeg")
  
  #Interpret zoom (resolution) input
  assert_that(is.string(zoom), zoom %in% c("Low","Medium","High"), not_empty(zoom))
  zoom. = ifelse(zoom == "Low", 0.5, ifelse(zoom=="Medium", 1, 1.33))
  
  #Check if website screenshot already exists in cache
  if(file.exists(destFile)){

    creation_time = file.info(destFile)$ctime
    current_time = Sys.time()
    delta_weeks = difftime(current_time, creation_time, units = "weeks")

    #Check if existing file is older than 1 week old
    if (delta_weeks >= 1) {

      #Delete existing file in cache
      file.remove(destFile)
      #get screenshot of url
      tryCatch(
        webshot::webshot(Url,
                zoom = zoom.,
                file = destFile,
                delay = 0.5),
                error = function(e){}
        )

    }


  } else{

    #get screenshot of url
    tryCatch(
      webshot::webshot(Url,
                       zoom = zoom.,
                       file = destFile,
                       delay = 0.5),
                       error = function(e){}
    )

  }

  #read temp (.jpeg) file into workspace
  if(file.exists(destFile)){
    
    jpg <- jpeg::readJPEG(source = destFile)
  
    #convert RGB dimensions of jpeg image into a data.frame
    r_mat <- reshape::melt(jpg[,,1])
    g_mat <- reshape::melt(jpg[,,2])
    b_mat <- reshape::melt(jpg[,,3])
    rgb_mat <- cbind(r_mat, g_mat[, 3], b_mat[, 3]) %>%
      data.frame()
    names(rgb_mat) <- c("row", "col", "red", "green", "blue")
  
    #K-means clustering for RGB values
    set.seed(seed)
    # k_all <- lapply(3:10, function(x) {
    #   kmeans(x = rgb_mat[,-c(1:2)],
    #          centers = x,
    #          iter.max = 30)
    # })
    # k <- k_all[[which.max(sapply(k_all, function(k) k$tot.withinss))]]
    k <- kmeans(x = rgb_mat[,-c(1:2)],
                centers = num_colors,
                iter.max = 30)
    hex_cols <- rgb(k$centers)
    props <- prop.table(table(k$cluster)) %>% 
      as.vector()
  
    #Create output data.frame
    if(color_names){
  
      name_cols <- sapply(hex_cols, function(x) plotrix::color.id(x)[1]) %>%
                      as.vector()
      dat <- data.frame(color=name_cols, prop=props)
      #dat$color = factor(name_cols, levels = name_cols[order(props)])
  
  
    } else{
  
        dat <- data.frame(color=hex_cols, prop=props)
        #dat$color = factor(hex_cols, levels = hex_cols[order(props)])
  
      }

  } else{
    
    NA_vec <- rep(NA,num_colors)
    dat <- data.frame(color=NA_vec, prop=NA_vec)
    
  }
  
  return(dat)

}

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
