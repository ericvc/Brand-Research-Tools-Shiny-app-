textInputRow<-function (inputId, label, value = "") {
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

#google authentication function from the 'pagespeedParseR' had a broken link
#replicated function needed to use Page Speed API.
auth_pagespeed2 <- function(api_key, verbose = TRUE){
  # assert_that(noNA(api_key), not_empty(api_key), is.string(api_key),
  #             nchar(api_key) > 0, noNA(verbose), not_empty(verbose),
  #             is.logical(verbose))
  x <- GET(url = "https://www.googleapis.com/pagespeedonline/v5/runPagespeed",
           query = list(url = "https://www.w3.org/", key = api_key,
                        strategy = "desktop"))
  Sys.sleep(0.5)
  if (x$status_code == 200) {
    Sys.setenv(PAGESPEED_API_KEY = api_key)
    if(verbose)
      message("API key authorized.")
  }
  else {
    stop(paste0("Authorization error: HTTP status code ",
                x$status_code, ". Check your API key."))
  }
}

get_top_N <- function(query, num_results=20, platform="desktop"){

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

  response <- httr::GET(Url, config = add_headers("user-agent"=headers))

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

web_colors <- function(Url,
                       num_colors = 10,
                       seed = 123,
                       color_names=FALSE) {

  #location of temp file for website screenshot
  destFile <- paste0("temp/webshot/",
                     paste(
                       xml2::url_parse(Url)$server,
                       xml2::url_parse(Url)$path,
                       sep = "-"
                     ),
                     ".jpeg")

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
      webshot(Url,
              zoom = 0.5,
              file = destFile,
              delay = 0.5)

    }


  } else{

    #get screenshot of url
    webshot(Url,
            zoom = 0.5,
            file = destFile,
            delay = 0.5)

  }

  #read temp (.jpeg) file into workspace
  jpg <- readJPEG(source = destFile) * 255

  #convert RGB dimensions of jpeg image into a data.frame
  r_mat <- reshape::melt(jpg[,,1])
  g_mat <- reshape::melt(jpg[,,2])
  b_mat <- reshape::melt(jpg[,,3])
  rgb_mat <- cbind(r_mat, g_mat[, 3], b_mat[, 3]) %>%
    data.frame()
  names(rgb_mat) <- c("row", "col", "red", "green", "blue")

  #K-means clustering for RGB values
  set.seed(seed)
  k <- kmeans(x=rgb_mat[,-c(1:2)]/255, centers=num_colors, iter.max=30)
  hex_cols <- rgb(k$centers)
  props <- prop.table(table(k$cluster)) %>% as.vector()

  #Create output data.frame
  if(color_names){

    name_cols <- sapply(hex_cols, function(x) plotrix::color.id(x)[1]) %>%
                    as.vector()
    dat <- data.frame(color=name_cols, prop=props)
    dat$color = factor(name_cols, levels = name_cols[order(props)])


  } else{

      dat <- data.frame(color=hex_cols, prop=props)
      dat$color = factor(hex_cols, levels = hex_cols[order(props)])

    }

  return(dat)

}

get_H_tags <- function(Url){

  ##Get response from input URL
  response = read_html(Url)

  ##Parse and re-format page content to calculate properties of interest
  htags = paste0("h", 1:6)
  text = sapply(htags, function(htag) {
    response %>%
      html_nodes(htag) %>%
      html_text() %>%
      str_replace_all(., "[\r\n\t]" , " ") %>% # remove all space and new lines
      paste0(., collapse = " ") %>%
      gsub("@\\w+", "", .) %>% #removes spaces in text strings
      gsub("[[:punct:]]", " ", .) %>% #removes punctuation
      gsub("http\\w+", " ", .) %>% #removes links
      gsub("[\n]", " ", .) %>% #replaces tabs with blank space
      gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
      gsub("^ ", " ", .) %>% #removes blank at beginning of string
      gsub("^ ", " ", .)  #%>% #removes blank at end of string
  })

  return(text)
}

get_text_body <- function(Url) {
  response <- read_html(Url)
  ##Parse and re-format page content to calculate properties of interest
  response_body = response %>%
    html_nodes("p") %>%
    html_text() %>%
    str_replace_all(., "[\r\n\t]" , " ") %>% # remove all space and new lines
    paste0(., collapse = " ") %>%
    gsub("\\r\\t", " ", .) %>% #removes spaces in text strings
    gsub("@\\w+", "", .) %>% #removes spaces in text strings
    gsub("[[:punct:]]", " ", .) %>% #removes punctuation
    gsub("http\\w+", " ", .) %>% #removes links
    gsub("[\n]", " ", .) %>% #replaces tabs with blank space
    gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
    gsub("^ ", " ", .) %>% #removes# blank at beginning of string
    gsub("^ ", " ", .)  %>% #removes blank at end of string
    #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
    tm::removeWords(., tm::stopwords()) %>%
    tolower
  return(response_body)
}

get_text_title <- function(Url) {
  ##Get response from input URL
  response = read_html(Url)
  response_title = response %>%
    html_nodes("title") %>%
    html_text() %>%
    str_replace_all(., "[\r\n\t]" , " ") %>%
    gsub("rt", "", .) %>% #removes spaces in text strings
    gsub("@\\w+", "", .) %>% #removes spaces in text strings
    gsub("[[:punct:]]", "", .) %>% #removes punctuation
    gsub("http\\w+", "", .) %>% #removes links
    gsub("[\n]", " ", .) %>% #replaces tabs with blank space
    gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
    gsub("^ ", "", .) %>% #removes blank at beginning of string
    gsub("^ ", "", .)  %>% #removes blank at end of string
    #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
    #tm::removeWords(., tm::stopwords()) %>%
    tolower # remove all space and new lines

  return(response_title)
}

formatDomain = function(url){

  #Remove prefix from url
  str_remove_all(url, pattern = "^(http[s]?://www\\.|http[s]?://|www\\.)") %>%
    str_trunc(width=35) #limit to 35 characters

}
