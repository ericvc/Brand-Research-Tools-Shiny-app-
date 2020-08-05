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