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
