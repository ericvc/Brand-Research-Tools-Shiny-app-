## Install packages from GitHub repositories
#devtools::install_github("Leszek-Sieminski/pagespeedParseR")
#devtools::install_github("nik01010/dashboardthemes")

## Load R Packages
#Shiny-related
library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(DT)
#Web and HTML-related
library(httr)
library(xml2)
library(rvest)
library(webshot)
#Plotting
library(ggplot2)
library(ggwordcloud)
library(ggplotify)
library(grid)
library(gridExtra)
library(raster)
library(plotrix)
library(R.devices)
#Misc
library(gtrendsR)
library(jpeg)
library(syuzhet)
library(tidyverse)
library(assertthat)
library(stringr)

#Check if need to install PhantomJS
if(!webshot::is_phantomjs_installed()){
  webshot::install_phantomjs()
}

## Load helper functions in current working directory
source("functions.R")

## Load ISO country codes for use in later menu options
data(countries)
codes <- countries %>%
  filter(sub_code == "")
codes <- as_tibble(codes[1:217,])
codes <- codes %>%
  mutate(label = stringr::str_to_title(name))
codes <- codes %>%
  mutate(country_code = as.character(country_code))
codes <- codes[order(codes$label),]

## Load map data for use in later plots
map48 = sf::st_read("data/contiguous48.shp")
centroids = map48 %>%
  sf::as_Spatial() %>%
  geosphere::centroid() %>%
  as_tibble
names(centroids) <- c("x","y")
centroids$names <- map48$NAME_1 %>%
  as.character

## Create sidebar for app
sidebar <- dashboardSidebar(
  tags$head(tags$style(".wrapper {overflow: visible !important;}")),
  width = 300,
  # App title  w/ company logo in top right corner----
  titlePanel(div(img(
    src = "index.jpg", height = 96.33
  ), style = "")),
  #Text box input to define Domains for comparison
  h4(HTML('<p style="color:white;margin-left:35px">Compare up to 5 websites</p>'), .noWS = "outside"),
  textInput("site1", label = NULL, value = "", placeholder = "https://www.example.com"),
  textInput("site2", label = NULL, value = "", placeholder = "https://www.example.com"),
  textInput("site3", label = NULL, value = "", placeholder = "https://www.example.com"),
  textInput("site4", label = NULL, value = "", placeholder = "https://www.example.com"),
  textInput("site5", label = NULL, value = "", placeholder = "https://www.example.com")
  # Clear results
  #actionButton("clear", "Clear Output")

)

## Create body of dashboard page
body <- dashboardBody(
  ### changing theme
  shinyDashboardThemes(theme = "blue_gradient"),
  fluidRow(style = "height:1000px",
    tabBox(
      title = NULL,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      height = "100%",
      width = "100%",
      #First Tab
      tabPanel(
        title="Google Search",
        textInput("query","Enter Keywords"),
        numericInput("num_results","Max. # of Results", min = 1, max = 20, value = 10, step=1, width = "8%"),
        #checkboxInput("platform", label = "Use mobile", value = FALSE),
        actionButton("search2","Search Google"),
        h5("Get the top results from the Google search engine for any kewyword phrase. The table below will show the title, URL, and domain of top rated sites."),
        br(),
        br(),
        dataTableOutput("SERP", width = "85%"),
        br(),
        br(),
        h6(textOutput("serp_warnings"))
      ),
      #Second Tab
      tabPanel(
        title = "Google Keywords Trends",
        fluidPage(height="100%",
          h4("Analyze the interest over time from Google search for up to five keyword phrases."),
          br(),
          h5(HTML("<b>Enter keyword(s)</b>")),
          textInputRow(inputId="keyword1", label="", value = ""),
          textInputRow(inputId="keyword2", label="", value = ""),
          textInputRow(inputId="keyword3", label="", value = ""),
          textInputRow(inputId="keyword4", label="", value = ""),
          textInputRow(inputId="keyword5", label="", value = ""),
          br(),
          br(),
          selectInput(
            inputId = "country",
            label = "Select geographic interest",
            choices = codes$label,
            multiple = TRUE,
            selected = "United States",
            width = "15%"
          ),
          sliderInput("from.", "How many months for comparisons?: ", 0, 60, 12, 1, round = 0),
          actionButton("submit_gt", "Get Trends", width = "15%"),
          br(),
          br(),
          downloadButton("dl_gtrends1", label="Export Plot 1 to PDF"),
          downloadButton("dl_gtrends2", label="Export Plot 2 to PDF"),
          plotOutput("gtrends_plots", height="1000px")
        )
      ),
      #Third Tab
      tabPanel(
        title="Text Analysis",
        h4("Analyze the word content from the landing pages of each site domain appearing in the sidebar."),
        actionButton("submit_ta", "Analyze", width = "10%"),
        h4(textOutput("TextTitle")),
        h6(plotOutput("WordCloud")),
        downloadButton("dl_wordcloud", "Export to PDF"),
        h6(plotOutput("Valence")),
        downloadButton("dl_textvalence", "Export to PDF"),
        br()
      ),
      #Fourth Tab
      tabPanel("Website Color Palettes", "",
               h4("Isloate the color palettes from the landing pages of each site domain appearing in the sidebar."),
               numericInput("num_colors", label = "# Colors", value = 10, min = 3, max = 20, step = 1, width = "10%"),
               checkboxInput("color_names", label = "Use Color Names", value = FALSE, width="15%"),
               actionButton("submit_wc", "Analyze", width = "10%"),
               h4("Website Color Analysis"),
               plotOutput("Webcolors", width = "95%"),
               downloadButton("dl_webcolors", "Export to PDF")
      ),
      #Fifth Tab
      tabPanel(
        title="Header Tags", "",
        h4("Gather the page header text from the landing pages of each site domain appearing in the sidebar."),
        actionButton("submit_ht", "Get Headers", width = "10%"),
        h4(textOutput("PageHeaderTagsTitle")),
        h6(tableOutput("Htags"))
      )
    )
  ),
  fluidRow(infoBoxOutput("tabset1Selected"))
)

#UI logic
ui = dashboardPage(
  dashboardHeader(title = "Brand Research Tools", titleWidth=300),
  sidebar,
  body
)

#Server logic
server = function(input, output, session) {

  N_sites <- function(){

    urls <- c(input$site1, input$site2, input$site3, input$site4, input$site5)
    urls <- urls[urls!=""]
    if(length(urls)<1){
      stop("You must enter atleast one valid URL.")
    }
    return(length(urls))

  }

  wordcloud_fun <- function(){

    #Iterate over websites
    urls <- c(input$site1, input$site2, input$site3, input$site4, input$site5)
    urls <- urls[urls!=""]
    if(length(urls)<1){
      stop("You must enter atleast one valid URL.")
    }

    progress <- Progress$new(session, min = 1, max = 3*length(urls))
    on.exit(progress$close())

    wc = list()
    for(site in 1:length(urls)){

      progress$set(message = 'Creating wordcloud from site text',
                   detail = 'Reading text from URLs')
      progress$set(value = 3*site-2)


      #Isolate and clean page text
      site_text <- get_text_body(urls[site]) %>%
        str_split(" ") %>%
        unlist %>%
        as_tibble %>%
        filter(.!="")

      progress$set(message = 'Creating wordcloud from site text',
                   detail = 'Formatting text for output')
      progress$set(value = 3*site-2)

      site_text_words <- sapply(site_text$value, function(word){
        paste0(
          unlist(
            str_extract_all(word, "[a-z]")
            ), collapse="")

          }
        ) %>%
        table %>%
        reshape::melt(.) %>%
        as_tibble
      names(site_text_words) <- c("words","freq")

      # progress$set(message = 'Creating wordcloud from site text',
      #              detail = 'Creating plots')
      # progress$set(value = 3*site-2)

      wc[[site]] <-
        as.grob(
          ggwordcloud(
            site_text_words$words,
            site_text_words$freq,
            colors = viridis::viridis(8)
          ) +
          theme_bw() +
          theme(plot.title = element_text(hjust=0.5, size=19)) +
          ggtitle(formatDomain(urls[site]))
        )

    }
    grobs <- gridExtra::arrangeGrob(grobs = wc, ncol = length(wc))
    plot(grobs)

    #write file to temp directory for export later
    R.devices::suppressGraphics({
      png("temp/wc_plt.png", width = (2+3*N_sites()), height = 5, units = "in", res=72)
        grid.newpage()
        grid.draw(grobs)
      dev.off()
    })

  }

 wordcloud_out <- eventReactive(input$submit_ta, wordcloud_fun())

 text_valence_fun <- function(){

    #Iterate over websites
    urls <- c(input$site1, input$site2, input$site3, input$site4, input$site5)
    urls <- urls[urls!=""]
    if(length(urls)<1){
      stop("You must enter atleast one valid URL.")
    }

    # progress <- Progress$new(session, min = 1, max = 3*length(urls))
    # on.exit(progress$close())

    sites_text <- list()
    sites_text <-
    lapply(urls, function(url){

      #Isolate and clean page text
      site_text <- get_text_body(url) %>%
        tolower() %>%
        gsub("@\\w+", "", .) %>% #removes spaces in text strings
        gsub("[[:punct:]]", " ", .) %>% #removes punctuation
        gsub("http\\w+", " ", .) %>% #removes links
        gsub("[\n]", " ", .) %>% #replaces tabs with blank space
        gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
        gsub("^ ", " ", .) %>% #removes blank at beginning of string
        gsub("^ ", " ", .)  %>% #removes blank at end of string
        tm::removeWords(tm::stopwords())

      # progress$set(message = 'Page text sentiment',
      #              detail = 'Formatting text for output')
      # progress$set(value = 3*site-2)

      #Sentiment scores
      #getting emotions using in-built function
      sentiment <- get_nrc_sentiment(site_text)
      #aggregated sentinment from all tweets
      sentiment_scores = data.frame(colSums(sentiment))
      names(sentiment_scores) <- "Score"
      sentiment_scores$sentiment = rownames(sentiment_scores)
      return(sentiment_scores)

    })

    sites <- reshape::melt(sites_text)
    names(sites) <- c("sentiment","variable","value","url")
    sites$site <- formatDomain(urls[sites$url])
    sites$site <-  factor(sites$site, formatDomain(urls))

    plt <- as.grob(
      ggplot(data = sites, aes(x = sentiment, y = value)) +
        geom_bar(aes(fill = sentiment),
                 color = "gray40",
                 size = 0.5,
                 stat = "identity") +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size=14),
          title = element_text(face = "bold", size = 17.5, hjust = 0.5),
          strip.text.x = element_text(size = 17),
          axis.text = element_text(size = 16),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 17)
        ) +
        xlab("Sentiment Category") +
        ylab("Emotional Valence Score") +
        scale_fill_viridis_d("", direction = -1) +
        facet_wrap(.~site, scales = "free", ncol=max(sites$url))
    )

    #write file to temp directory for export later
    R.devices::suppressGraphics({
      png("temp/tv_plt.png", width = (2+3*N_sites()), height = 5, units = "in", res=72)
        grid.newpage()
        grid.draw(plt)
      dev.off()
    })

    grid.newpage()
    grid.draw(plt)

 }

 text_valence <- eventReactive(input$submit_ta, {text_valence_fun()})

 htags_out <- eventReactive(input$submit_ht, {
   #add functions to build output data.frame.

   progress <- Progress$new(session, min = 1, max = 3)
   on.exit(progress$close())
   progress$set(message = 'Gathering header tags', detail = '')
   progress$set(value = 1)

   #Iterate over websites
   urls <-
     c(input$site1,
       input$site2,
       input$site3,
       input$site4,
       input$site5)
   urls <- urls[urls != ""]
   if (length(urls) < 1) {
     stop("You must enter atleast one valid URL.")
   }

   progress$set(value = 2)

   out = lapply(urls, function(url) get_H_tags(url)) %>%
     do.call("cbind", .) %>%
     data.frame
   names(out) = sapply(urls, function(url)
     formatDomain(url))
   out = cbind(data.frame(Tag=paste0("h", 1:6)),out)
   return(out)

   progress$set(value = 3)

 })

 webcolors_fun <- function(){
   #add functions to build output data.frame.

   progress <- Progress$new(session, min = 1, max = 4)
   on.exit(progress$close())
   progress$set(message = 'Analyzing webpage color palettes',
                detail = 'This may take a couple of minutes, depending on the number of sites analyzed.')
   progress$set(value = 1)

   #Iterate over websites
   urls <-
     c(input$site1,
       input$site2,
       input$site3,
       input$site4,
       input$site5)
   urls <- urls[urls != ""]
   if (length(urls) < 1) {
     stop("You must enter atleast one valid URL.")
   }

   progress$set(value = 2)

   pal <-
     lapply(urls, function(url)
       web_colors(
         url,
         color_names = input$color_names,
         num_colors = input$num_colors
       )) #returns data for the color palette from URL(s)

   #define plot theme and variables
   ggtitle. <- paste0("Top ", input$num_colors, " Colors")
   plot.theme <-
     theme(plot.title = element_text(hjust = 0.5, size = 15))

   #slightly different methods depending on whether a single or multiple plots are produced
   if(length(pal) == 1) {

     #create grid of color values and add names
     grobs <- as.grob(
       ggplot(pal[[1]], aes(x=reorder(color, prop), y=prop*100, fill=color)) +
         ggtitle(ggtitle.) +
         theme_classic() +
         theme(
           panel.background = element_rect(fill="gray85"),
           legend.title = element_text(size = 14.5, face = "bold"),
           legend.text = element_text(size = 14),
           plot.title = element_text(face = "bold", size = 17.5, hjust = 0.5),
           #strip.text.x = element_text(size = 15),
           #strip.text.y = element_text(size = 15),
           axis.text = element_text(size = 14),
           axis.title = element_text(size = 17)
         ) +
         geom_bar(stat = "identity", color="gray30", size=1) +
         scale_fill_identity() +
         coord_flip() +
         labs(y="% of Page", x="")
     )

   } else{ #case to run methods for multiple plots

     allPals <- do.call("rbind", pal) %>% data.frame
     allPals$site = factor(urls[rep(1:length(pal), each = nrow(allPals)/length(pal))], levels = urls)

     #create grid of color values and add names
     grobs <- as.grob(
       ggplot(allPals, aes(x=reorder(color, prop), y=prop*100, fill=color)) +
         ggtitle(ggtitle.) +
         theme_classic() +
         theme(
           panel.background = element_rect(fill="gray85"),
           legend.title = element_text(size = 14.5, face = "bold"),
           legend.text = element_text(size = 14),
           plot.title = element_text(face = "bold", size = 17.5, hjust = 0.5),
           strip.text.x = element_text(size = 15),
           strip.text.y = element_text(size = 15),
           axis.text = element_text(size = 14),
           axis.title = element_text(size = 17)
         ) +
         geom_bar(stat = "identity", color="gray30", size=0.5) +
         scale_fill_identity() +
         coord_flip() +
         labs(y="% of Page", x="") +
         facet_wrap(.~site, scales = "free", ncol = length(pal))
     )
   }

   progress$set(value = 3)

   #write file to temp directory for export later
   R.devices::suppressGraphics({
     png("temp/webcolors_plt.png", width = (2+3.5*N_sites()), height = 5, units = "in", res=72)
     grid.newpage()
     grid.draw(grobs)
     dev.off()
   })

   grid.newpage()
   grid.draw(grobs)

 }

 webcolors_out <- eventReactive(input$submit_wc, webcolors_fun())

 google_serp <- eventReactive(input$search2, {

   if(input$query==""){
     stop("Error: your search query must not be empty.")
   }

   progress <- Progress$new(session, min=1, max=3)
   on.exit(progress$close())
   progress$set(message = 'Querying Google.com',
                detail = '')
   progress$set(value = 1)

   # platform. <- ifelse(input$platform,"mobile","desktop")
   dt = get_top_N(input$query, num_results=input$num_results, platform="desktop")

   progress$set(message = 'Gathering top search results',
                detail = '')
   progress$set(value = 2)

   dt2 = cbind(as.numeric(rownames(dt)),dt)
   names(dt2) = c("Rank","Title","URL")
   url_parse. = url_parse(dt2$URL)
   dt2$Domain = with(url_parse., paste(scheme,server,sep="://"))
   dt2$URL <- paste0("<a href='",dt2$URL,"'>",dt2$URL,"</a>")
   rownames(dt2) = NULL
   # write.csv(dt2, "temp/google_top10.csv", row.names = FALSE)

   progress$set(message = 'Formatting table',
                detail = '')
   progress$set(value = 3)

   return(dt2)

 })

 google_serp_out <- eventReactive(input$search2, google_serp())

 gtrends_fun = function() { #add functions to build output data.frame.

   progress <- Progress$new(session, min=1, max=5)
   on.exit(progress$close())
   progress$set(message = 'Submitting keywords to Google trends.',
                detail = '')
   progress$set(value = 1)

   #Check keyword phrases
   kws <- c(input$keyword1, input$keyword2, input$keyword3, input$keyword4, input$keyword5)
   kws <- kws[kws!=""]
   kws <- sapply(kws, function(x) str_trim(x, side="both") )

   #Check that there is atleast one keyword phrase
   if(length(kws)<1){
     stop("You must enter atleast one keyword phrase.")
   }
   #Check that there is atleast one country selected for geographic interest
   if(length(input$country)==0){
     stop("You must select one country of interest.")
   }

   #query = c("occidental tool belt","leather tool belt","diamondback tool belt","klein tool belt","atlas 46 tool belt")
   #Pull data from Google trends
   to. = as.Date(Sys.time())
   from. = to. - input$from.*(365/12)
   date_range = paste(from., to.)
   date_range_title = paste(from., to., sep=" to ")

   #need to iterate across keyword OR countries or 'gtrends' will spit back an error
   g_raw <- lapply(input$country, function(country) {
     #Convert selected country names to ISO codes
     country. <- with(codes, country_code[label==country])
     d <- gtrends(
       keyword = kws,
       time = date_range,
       geo = country.,
       gprop = "web",
       onlyInterest = FALSE
     )
     if(!is.null(d$interest_over_time)){
      d$interest_over_time$country <- country
      return(d)
     }

   })
   names(g_raw) <- input$country

   g <- lapply(g_raw, function(g) g$interest_over_time) %>%
     do.call("rbind",.)
   if(is.null(g)){
     stop("Google Trends: No data returned. Try a different keyword phrase or geographic origin.")
   }

   #check that there is data for all countries
   totalHitsByCountry <- with(g, tapply(hits, list(geo), sum))
   if(any(totalHitsByCountry==0)){
     #logical vector indicating which rows to keep (FALSE) or remove (TRUE)
     countriesToRemove <- names(which(totalHitsByCountry == 0))
     g <- g[!g$geo %in% countriesToRemove,]
   }
   #After filtering, how many unique geo IDs remaining?
   n_unique_geo <- length(unique(g$geo))

   progress$set(message = 'Plotting the results (1/2).',
                detail = '')
   progress$set(value = 2)

   plot_theme <- theme(
     legend.position = "bottom",
     legend.title = element_text(size = 14.5, face = "bold"),
     legend.text = element_text(size = 14),
     title = element_text(face = "bold", size = 17.5, hjust = 0.5),
     strip.text.x = element_text(size = 15),
     strip.text.y = element_text(size = 15),
     axis.text = element_text(size = 16),
     axis.title = element_text(size = 17)
   )

   if(n_unique_geo == 1){
     #make plots
     p1 = (ggplot(g,
                  mapping=aes(x=as.Date(date), y=hits, group=keyword, color=keyword, fill=keyword)
     ) +
       ggtitle(paste("Keyword Searches Over Time\n", date_range_title)) +
       theme_minimal() +
       plot_theme +
       guides(color=guide_legend(nrow = 2, ncol = 3)) +
       scale_x_date("Date") +
       labs(y="Hits") +
       geom_line(size=1) +
       scale_color_viridis_d(direction = 1, begin = 0, end = 0.9)
     )

     g = g[order(g$keyword),] %>%
       as_tibble
     g$cumsum_hits = with(g, tapply(hits, list(keyword), cumsum)) %>%
       unlist

     progress$set(message = 'Plotting the results (2/2).',
                  detail = '')
     progress$set(value = 3)

     p2 = (ggplot(g,
                  mapping=aes(x=as.Date(date), y=cumsum_hits, group=keyword, color=keyword, fill=keyword)
     ) +
       ggtitle(paste("Cumulative Keyword Searches Over Time\n", date_range_title)) +
       theme_minimal() +
       plot_theme +
       guides(color=guide_legend(nrow = 2, ncol = 3)) +
       scale_x_date("Date") +
       ylab("Total Hits") +
       geom_line(size=1) +
       scale_color_viridis_d(direction = 1, begin = 0, end = 0.9) )

     grobs <- list(as.grob(p1), as.grob(p2))
     grobs2 <- arrangeGrob(grobs=grobs, ncol=2)

   } else{

     #make plots
     p1 = (ggplot(g,
                  mapping=aes(x=as.Date(date), y=hits, group=keyword, color=keyword, fill=keyword)
     ) +
       ggtitle(paste("Keyword Searches Over Time\n", date_range_title)) +
       theme_bw() +
       plot_theme +
       guides(color=guide_legend(nrow = 2, ncol = 3)) +
       scale_x_date("Date") +
       labs(y="Hits") +
       geom_line(size=1) +
       scale_color_viridis_d(direction = 1, begin = 0, end = 0.9) +
       facet_grid(country~., switch = "y")
     )

     g = g[order(g$geo, g$keyword),] %>%
       as_tibble
     g$cumsum_hits = with(g, tapply(hits, list(keyword,geo), cumsum)) %>%
       unlist

     progress$set(message = 'Plotting the results (2/2).',
                  detail = '')
     progress$set(value = 3)

     p2 = (ggplot(g,
                  mapping=aes(x=as.Date(date), y=cumsum_hits, group=keyword, color=keyword, fill=keyword)
     ) +
       ggtitle(paste("Cumulative Keyword Searches Over Time\n", date_range_title)) +
       theme_bw() +
       plot_theme +
       guides(color=guide_legend(nrow = 2, ncol = 3)) +
       scale_x_date("Date") +
       ylab("Total Hits") +
       geom_line(size=1) +
       scale_color_viridis_d(direction = 1, begin = 0, end = 0.9) +
       facet_grid(country~., switch = "y")
     )

     grobs <- list(as.grob(p1), as.grob(p2))
     grobs2 <- arrangeGrob(grobs=grobs, ncol=2)

   }

   progress$set(message = 'Creating geographic plots of keyword interest.',
                detail = '')
   progress$set(value = 5)

   if(any(input$country == "United States")){

     #need to match states with data with rows in the polygon data frame
     g2 <- g_raw[["United States"]]
     hits = g2$interest_by_region %>%
       as_tibble %>%
       filter(!is.na(g2$interest_by_region$hits)) %>%
       with(., tapply(hits, list(location, keyword), sum)) %>%
       reshape::melt(.) %>%
       filter(complete.cases(.)) %>%
       rename(., state=X1, keyword=X2)

     ii = match(hits$state, centroids$names)
     hits$xy = centroids[ii, c("x","y")]

     #plot geographic variation in searches (USA only)
     p3 = (ggplot(map48) +
       ggtitle("Search Volume by U.S. State") +
       xlab("") + ylab("") +
       plot_theme +
       theme(axis.text=element_blank(),
             axis.ticks=element_blank(),
             panel.background = element_rect(fill=NULL, color="black")) +
       guides(color=guide_legend(nrow = 2, ncol = 3)) +
       geom_sf(fill = "white") +
       geom_point(
         data = hits,
         mapping = aes(
           x = xy$x,
           y = xy$y,
           size = value,
           color = keyword
         ),
         alpha = 0.5,
         position = position_jitterdodge(
           seed = 222,
           dodge.width = 1,
           jitter.width = 0.5,
           jitter.height = 0.5
         )
       ) +
       guides(color = guide_legend(override.aes = list(size = 5))) +
       scale_color_viridis_d("keyword", begin = 0, end = 0.9) +
       scale_size_continuous("interest", range = c(0.25, 5))
       )
    }

   #draw all plots
   grobs <-
     arrangeGrob(
       grobs = list(as.grob(p1), as.grob(p2), as.grob(p3)),
       ncol = 2,
       layout_matrix = matrix(c(1, NA, 1, 3, 2, 3, 2, NA), nrow=2)
     )
   grid.newpage()
   grid.draw(grobs)

   # grid.newpage()
   # grid.draw(as.grob(p3))

   ## Save plots to local storage for output later on
   progress$set(message = 'Saving plots for output.',
                detail = '')
   progress$set(value = 5)

   #write file to temp directory for export later
   R.devices::suppressGraphics({
     png("temp/gtrends_plt1.png", width = 7, height = 5*n_unique_geo, units = "in", res=72)
       plt <- as.grob(p1)
       grid.newpage()
       grid.draw(plt)
     dev.off()
   })

   R.devices::suppressGraphics({
     png("temp/gtrends_plt2.png", width = 7, height = 5*n_unique_geo, units = "in", res=72)
       plt <- as.grob(p2)
       grid.newpage()
       grid.draw(plt)
     dev.off()
   })

   R.devices::suppressGraphics({
     png("temp/gtrends_plt3.png", width = 7, height = 5, units = "in", res=72)
       plt <- as.grob(p3)
       grid.newpage()
       grid.draw(plt)
     dev.off()
   })

 }

  gtrends_out = eventReactive(input$submit_gt, gtrends_fun())

  #Outputs Titles
  output$TextTitle = renderText({""})

  #Outputs Render
  output$WordCloud <- renderPlot({wordcloud_out()})
  output$Webcolors <- renderPlot({webcolors_out()})
  output$Valence <- renderPlot({text_valence()})
  output$Htags <- renderTable({htags_out()}, bordered = TRUE, align = "c", spacing = "s")
  output$SERP <- DT::renderDataTable({google_serp_out()}, rownames=FALSE, escape=FALSE, options = list(
    lengthMenu = list(c(10, -1), c('10', 'All')),
    pageLength = -1
  ))
  output$serp_warnings <- renderText("WARNING: Too many searches in a short time will cause Google to temporarily block the IP address.")
  output$gtrends_plots = renderPlot(gtrends_out(), width=1000, height = 800)

  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      value <- raster::extract(webcolors_out(), cbind(e$x,e$y))
      return(value)
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })

  #Outputs save plots
  output$dl_wordcloud <- downloadHandler(
    filename = "word_cloud.pdf",
    content=function(file){
      ras <- png::readPNG("temp/wc_plt.png") %>%
        as.raster()
      pdf(file, width = (2+3*N_sites()), height = 5)
        #grid.newpage()
        plot(ras)
      dev.off()
    }
  )
  output$dl_textvalence <- downloadHandler(
    filename = "text_valence.pdf",
    content=function(file){
      ras <- png::readPNG("temp/tv_plt.png") %>%
        as.raster()
      pdf(file, width = (2+3*N_sites()), height = 5)
      #grid.newpage()
      plot(ras)
      dev.off()
    }
  )
  output$dl_webcolors <- downloadHandler(
    filename = "webpage_color_palettes.pdf",
    content=function(file){
      ras <- png::readPNG("temp/webcolors_plt.png") %>%
        as.raster()
      pdf(file, width = (2+3*N_sites()), height = 5)
        #grid.newpage()
        plot(ras)
      dev.off()
    }
  )
  output$dl_gtrends1 <- downloadHandler(
    filename = "google_trends_1.pdf",
    content=function(file){
      ras <- png::readPNG("temp/gtrends_plt1.png") %>%
        as.raster()
      pdf(file, width = 7, height = 5)
        plot(ras)
      dev.off()
    }
  )
  output$dl_gtrends2 <- downloadHandler(
    filename = "google_trends_2_cumulative.pdf",
    content=function(file){
      ras <- png::readPNG("temp/gtrends_plt2.png") %>%
        as.raster()
      pdf(file, width = 7, height = 5)
      plot(ras)
      dev.off()
    }
  )
  output$dl_google_top10 <- downloadHandler(
    filename = function(){"google_top10.csv"},
    content = function(file){
      data <- read.csv("temp/google_top10.csv")
      write.csv(data, file)
    }
  )

}

## Create the app
shinyApp(ui, server)
