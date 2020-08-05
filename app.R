## Install packages from GitHub repositories
#devtools::install_github("Leszek-Sieminski/pagespeedParseR")
#devtools::install_github("nik01010/dashboardthemes")

## Load R Packages
#Shiny-related
library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(DT)
#Web-interface
library(xml2)
library(rvest)
library(httr)
#Plotting
library(ggplot2)
#Misc
library(gtrendsR)
library(tidyr)
library(assertthat)
library(stringr)
library(jsonlite)

## Set page variables
#Read-in api key JSON file 'api_keys.json'
#The file should contain a simple list with key names "semrush_api_key" and "google_lighthouse_api_key"
#See the "api_keys_example.json" for a template
attach(jsonlite::read_json("api_keys.json"))

#Get US states name for column menu options
data(state)


## Create 'temp' directory if one does not exist
if(!dir.exists("temp")){
  dir.create("temp")
  dir.create("temp/get")
  dir.create("temp/greviews")
}

## Create Python virtual environment
reticulate::virtualenv_create(envname = "python_environment", python = "python3")
reticulate::virtualenv_install(envname = "python_environment", packages = c("pandas","numpy","bs4","requests"), ignore_installed = TRUE)
reticulate::use_virtualenv("python_environment", required = TRUE)
reticulate::source_python("Python/webfonts.py")

## Check if need to install PhantomJS
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}

## Load ISO country codes for use in later menu options
#data(countries)
#
# codes <- countries %>%
#   dplyr::filter(sub_code == "") %>%
#   dplyr::mutate(label = stringr::str_to_title(name)) %>%
#   dplyr::mutate(country_code = as.character(country_code))
# codes <- codes[order(codes$label),]
# write.csv(codes, "data/gtrends_geo_codes.csv", row.names = FALSE)
codes <-
  read.csv("data/gtrends_geo_codes.csv", stringsAsFactors = FALSE)

## Load map data for use in some plots
map48 = sf::st_read("data/contiguous48.shp")
# centroids = map48 %>%
#   sf::as_Spatial() %>%
#   geosphere::centroid() %>%
#   tibble::as_tibble(.)
# names(centroids) <- c("x","y")
# centroids$names <- map48$NAME_1 %>%
#   as.character
# saveRDS(centroids,"data/state_centroids.rds")
centroids = readRDS("data/state_centroids.rds")


## Load functions in current working directory
src_files <- list.files("R/", full.names = TRUE)
sapply(src_files, function(x) source(x))


## Create sidebar for app
sidebar <- dashboardSidebar(
  tags$head(tags$style(
    ".wrapper {overflow: visible !important;}"
  )),
  width = 300,
  # App title  w/ company logo in top right corner----
  titlePanel(div(
    img(src = "index.jpg", height = 96.33), style = ""
  )),
  #Text box input to define Domains for comparison
  h4(
    HTML(
      '<p style="color:white;margin-left:35px">Compare up to 5 websites</p>'
    ),
    .noWS = "outside"
  ),
  textInput(
    "site1",
    label = NULL,
    value = "",
    placeholder = "https://www.example.com"
  ),
  textInput(
    "site2",
    label = NULL,
    value = "",
    placeholder = "https://www.example.com"
  ),
  textInput(
    "site3",
    label = NULL,
    value = "",
    placeholder = "https://www.example.com"
  ),
  textInput(
    "site4",
    label = NULL,
    value = "",
    placeholder = "https://www.example.com"
  ),
  textInput(
    "site5",
    label = NULL,
    value = "",
    placeholder = "https://www.example.com"
  )
  
)

## Create body of dashboard page
body <- dashboardBody(
  style = "height:100%;margin-left:1%;margin-right:1%;margin-top:0%",
  #changing theme
  shinyDashboardThemes(theme = "blue_gradient"),
  fluidRow(tags$head(
    tags$style(".shiny-output-error{color:blue; font-size: 17px}")
  )),
  fluidRow(
    style = "height:1000px",
    tabBox(
      title = NULL,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      type = "pills",
      height = "100%",
      width = "100%",
      #First Tab - Google SERP Results
      tabPanel(title = "Google Search",
               fluidPage(
                 height = "100%",
                 column(
                   12,
                   align = "center",
                   h3(HTML("<b>Google Search Engine</b>")),
                   h5(
                     "Use the Google search engine to identify keywords, competitors, and other relevant information."
                   ),
                   splitLayout(
                     cellWidths = c("25%","0%", "8%","0%"),
                     textInput("query", "Enter Keywords"),
                     tags$style(type="text/css", "#query {text-align:center}"),
                     numericInput(
                       "num_results",
                       "Max. Results",
                       min = 1,
                       max = 20,
                       value = 10,
                       step = 1,
                       width = "100%"
                     ),
                     tags$style(type="text/css", "#num_results {text-align:center}")
                   ), 
                   actionButton("search2", "Search Google", icon("refresh")),
                   h5(
                     "Get the top results from the Google search engine for any kewyword phrase. The table below will show the title, URL, and domain of top rated sites."
                   ),
                   br(),
                   br(),
                   dataTableOutput("SERP", width = "85%"),
                   br(),
                   br(),
                   h6(textOutput("serp_warnings"))
                 )
               )),
      #Second Tab - Google Trends
      tabPanel(title = "Google Trends",
               fluidPage(
                 height = "100%",
                 column(
                   12,
                   align = "center",
                   h3(HTML("<b>Google Trends</b>")),
                   h5(
                     "Download the latest results from Google Trends. Create data visualizations of keyword trends over time and geographic analysis of results."
                   ),
                   br(),
                   h5(HTML("<b>Enter keyword(s)</b>")),
                   textInputRow(
                     inputId = "keyword1",
                     label = "",
                     value = ""
                   ),
                   textInputRow(
                     inputId = "keyword2",
                     label = "",
                     value = ""
                   ),
                   textInputRow(
                     inputId = "keyword3",
                     label = "",
                     value = ""
                   ),
                   textInputRow(
                     inputId = "keyword4",
                     label = "",
                     value = ""
                   ),
                   textInputRow(
                     inputId = "keyword5",
                     label = "",
                     value = ""
                   ),
                   br(),
                   br(),
                   selectInput(
                     inputId = "country",
                     label = "Select geographic interest(s)",
                     choices = codes$label,
                     multiple = TRUE,
                     selected = "United States",
                     width = "25%"
                   ),
                   sliderInput(
                     "from.",
                     "How many prior months for comparisons?: ",
                     0,
                     60,
                     12,
                     1,
                     round = 0,
                     width = "25%"
                   ),
                   actionButton("submit_gt", "Get Trends", icon = icon("refresh")),
                   br(),
                   br(),
                   downloadButton("dl_gtrends1", label = "Save Plot1.pdf"),
                   downloadButton("dl_gtrends2", label = "Save Plot2.pdf"),
                   downloadButton("dl_gtrends3", label = "Save Plot3.pdf"),
                   plotOutput("gtrends_plots", height = "1000px")
                 )
               )),
      #Third Tab - Google Business Reviews
      tabPanel(
        # addClass(selector = "body", class = "sidebar-collapse"),
        title = "Google Business Reviews",
        fluidPage(
          column(12, align='center',
                 h3(HTML("<b>Google Business Reviews Analytics</b>")),
                 h5(
                   "Query the DataForSEO database to retrieve tabulated reports of the content of Google Business Reviews, including user information, text, and user rating. Report requests may take several minutes to process and complete. All entries are case-sensitive."
                 ),
                 br(),
                 splitLayout(
                   tags$head(tags$style(
                     HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 text-align:center;
                 }
                 ")
                   )),
                   cellWidths = c("0%","20%","0%", "12%","0%", "12%", "7.5%","3%","0%"),
                   textInput(inputId = "dfseo_keyword", "Business Name", placeholder = "TIV Branding"), 
                   tags$style(type="text/css", "#dfseo_keyword {text-align:center}"),
                   textInput(inputId = "dfseo_city", "Location - City", placeholder = "Santa Rosa"),
                   tags$style(type="text/css", "#dfseo_city {text-align:center}"),
                   selectInput(inputId = "dfseo_state", "Location - State", choices = state.name, selected = "California"), 
                   numericInput(
                     "dfseo_depth",
                     label = "Max. Results",
                     min = 10,
                     max = 1000,
                     value = 50,
                     step = 10
                   ),
                   tags$style(type="text/css", "#dfseo_depth {text-align:center}")
                 ),
                 actionButton("dfseo_submit", "Request", width = "10%", icon("bar-chart-o"))
          ),
          br(),
          br(),
          br(),
          br(),
          fluidRow(
            column(12, align="center",
                   plotOutput(
                     "dfseo_plots"#, width = "auto", height = "auto"
                   ),
                   br(),
                   downloadButton("dl_dfseo_plots", "Save .pdf")
            )
          ),
          fluidRow(
            column(12, align="center",
                   dataTableOutput(
                     "dfseo_table", width = "85%", height = "auto"
                   ),
                   br(),
                   downloadButton("dl_dfseo_table", "Save .csv")
            )
          ),
          br()
        )
      ),
      #Fourth Tab - Wordcloud and Text Valence
      tabPanel(title = "Text Analysis",
               fluidPage(
                 column(
                   12,
                   align = "center",
                   h3(HTML("<b>Word Cloud and Text Sentiment</b>")),
                   height = "100%",
                   h4(textOutput("TextTitle")),
                   h5(
                     "Analyze the word content from the landing pages of each site domain appearing in the sidebar. Change the 'seed' value to generate different worldcloud placement and rotations."
                   ),
                   br(),
                   splitLayout(
                     cellWidths = c("15%","1.5%","10%","0%","10%","0%","10%","0%"),
                     sliderInput(
                       "min_freq",
                       "Min. Word Frequency",
                       min = 1,
                       max = 10,
                       value = 3,
                       step = 1
                     ),
                     br(),
                     numericInput(
                       "scale_min",
                       label = "Min. Word Size",
                       min = 0.3,
                       max = 3,
                       value = 0.7,
                       step = 0.1
                     ),
                     tags$style(type="text/css", "#scale_min {text-align:center}"),
                     numericInput(
                       "scale_max",
                       label =  "Max. Word Size",
                       min = 5,
                       max = 15,
                       value = 8,
                       step = 0.5
                     ),
                     tags$style(type="text/css", "#scale_max {text-align:center}"),
                     numericInput(
                       "seed",
                       "Set Seed",
                       value = 123,
                       min = 0,
                       max = Inf
                     ),
                     tags$style(type="text/css", "#seed {text-align:center}")
                   ),
                   actionButton("submit_ta", "Analyze", width = "10%", icon("bar-chart-o")),
                   h6(plotOutput(
                     "WordCloud", width = "auto", height = "auto"
                   )),
                   downloadButton("dl_wordcloud", "Save as .pdf"),
                   h6(plotOutput(
                     "Valence", width = "auto", height = "auto"
                   )),
                   downloadButton("dl_textvalence", "Save as .pdf"),
                   br()
                 )
               )),
      #Fifth Tab - Website Colors
      tabPanel("Website Color Palettes",
               fluidPage(
                 height = "100%",
                 column(
                   12,
                   align = "center",
                   h3(HTML("<b>Website Color Analysis</b>")),
                   h5(
                     "Isloate the color palettes from the landing pages of each site domain appearing in the sidebar\n. This operation may be slow initially, but website images will be cached for up to seven days to speed up repeated use of the same domains."
                   ),
                   splitLayout(
                     cellWidths = c("7%", "0%", "15%", "0%"),
                     selectInput(
                       "num_colors",
                       label = "# Colors",
                       choices = c(2:5),
                       selectize = FALSE,
                       size = 1,
                       width = "95%"
                     ),
                     tags$style(type="text/css", "#num_colors {text-align:center}"),
                     selectInput(
                       "zoom",
                       label =  "Webshot Resolution",
                       choices = c("Low", "Medium", "High"),
                       width = "95%",
                       selectize = FALSE,
                       size = 1
                     ),
                     tags$style(type="text/css", "#zoom {text-align:center}")
                   ),
                   checkboxInput(
                     "color_names",
                     label = HTML("<b>Use Color Names</b>"),
                     value = FALSE,
                     width = "15%"
                   ),
                   actionButton("submit_wc", "Analyze", width = "10%", icon("bar-chart-o")),
                   br(),
                   br(),
                   plotOutput("Webcolors", width = "auto", height = "auto"),
                   downloadButton("dl_webcolors", "Save as .pdf"),
                   downloadButton("dl_webshots", "Save Webshots")
                 )
               )),
      #Sixth Tab - Fonts
      tabPanel("Website Fonts",
               "",
               fluidPage(
                 height = "100%",
                 column(
                   12,
                   align = "center",
                   h3(HTML("<b>Website Font Analysis</b>")),
                   h4(""),
                   h5(
                     "Search webpage HTML and CSS coding for font specificiations. If found, they will displayed in the table appearing below."
                   ),
                   actionButton(
                     "submit_wf",
                     "Get Fonts",
                     width = "10%",
                     icon = icon("refresh")
                   ),
                   br(),
                   br(),
                   dataTableOutput("Webfonts", width = "85%")
                 )
               )),
      #Seventh Tab - Header Tags
      tabPanel(title = "Page Header Tags",
               "",
               fluidPage(
                 column(
                   12,
                   align = "center",
                   height = "100%",
                   h3(HTML("<b>Page Header Tags</b>")),
                   h5(
                     "Gather the page header text from the landing pages of each site domain appearing in the sidebar."
                   ),
                   actionButton(
                     "submit_ht",
                     "Get Headers",
                     width = "13%",
                     icon = icon("refresh")
                   ),
                   h4(textOutput("PageHeaderTagsTitle")),
                   h6(tableOutput("Htags"))
                 )
               ))
    )
  ),
  fluidRow(infoBoxOutput("tabset1Selected"))
)

## UI
ui = dashboardPage(dashboardHeader(title = "Brand Research Tools", titleWidth =
                                     298),
                   sidebar,
                   body)

## Server logic
server = function(input, output, session) {
  #Function that returns the number of sites (URLs) entered in the sidebar
  N_sites <- function() {
    urls = input_urls(input)
    if (length(urls) < 1) {
      shiny::validate(FALSE, "You must enter atleast one valid URL.")
    }
    return(length(urls))
    
  }
  
  #Wordcloud plotting function
  wordcloud_fun <- function() {
    #Iterate over websites
    urls = input_urls(input)
    if (length(urls) < 1) {
      shiny::validate(FALSE, "You must enter atleast one valid URL.")
    }
    
    progress <-
      Progress$new(session, min = 1, max = 3 * length(urls))
    on.exit(progress$close())
    
    wc = list()
    for (site in 1:length(urls)) {
      progress$set(message = 'Creating wordcloud from site text',
                   detail = 'Reading text from URLs')
      progress$set(value = 3 * site - 2)
      
      #Isolate and clean page text
      site_text <- get_text_body(urls[site]) %>%
        stringr::str_split(" ") %>%
        unlist %>%
        as_tibble() %>%
        dplyr::filter(. != "")
      
      progress$set(message = 'Creating wordcloud from site text',
                   detail = 'Formatting text for output')
      progress$set(value = 3 * site - 2)
      
      site_text_words <- sapply(site_text$value, function(word) {
        paste0(unlist(stringr::str_extract_all(word, "[a-z]")), collapse = "")
        
      }) %>%
        table %>%
        reshape::melt(.) %>%
        as_tibble
      names(site_text_words) <- c("words", "freq")
      
      # progress$set(message = 'Creating wordcloud from site text',
      #              detail = 'Creating plots')
      # progress$set(value = 3*site-2)
      
      set.seed(input$seed)
      wc[[site]] <-
        ggplotify::as.grob(
          ggwordcloud::ggwordcloud(
            min.freq = input$min_freq,
            scale = c(10, 2),
            site_text_words$words,
            site_text_words$freq,
            colors = viridis::viridis(8)
          ) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 19)) +
            ggtitle(formatDomain(urls[site]))
        )
      
    }
    grobs <-
      gridExtra::arrangeGrob(grobs = wc, ncol = length(wc))
    plot(grobs)
    
    saveRDS(grobs, "temp/wc_plt.rds")
    
  }
  
  #Wordcloud Reactive
  wordcloud_out <-
    eventReactive(input$submit_ta, wordcloud_fun())
  
  #Text Valence Plotting Function
  text_valence_fun <- function() {
    #Iterate over websites
    urls = input_urls(input)
    if (length(urls) < 1) {
      shiny::validate(FALSE, "You must enter atleast one valid URL.")
    }
    #check for transfer protocol prefix
    l <-
      sapply(urls, function(x)
        grepl(pattern = "^https?://", x = x))
    if (any(!l)) {
      urls[!l] <- paste0("https://", urls[!l])
    }
    
    # progress <- Progress$new(session, min = 1, max = 3*length(urls))
    # on.exit(progress$close())
    
    sites_text <- list()
    sites_text <-
      lapply(urls, function(url) {
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
        
        ## Sentiment scores
        #getting emotions using in-built function
        sentiment <- syuzhet::get_nrc_sentiment(site_text)
        #aggregated sentinment from all tweets
        sentiment_scores = data.frame(colSums(sentiment))
        names(sentiment_scores) <- "Score"
        sentiment_scores$sentiment = rownames(sentiment_scores)
        return(sentiment_scores)
        
      })
    
    sites <- reshape::melt(sites_text)
    names(sites) <- c("sentiment", "variable", "value", "url")
    sites$site <- formatDomain(urls[sites$url])
    sites$site <-  factor(sites$site, formatDomain(urls))
    
    plt <- ggplotify::as.grob(
      ggplot(data = sites, aes(x = sentiment, y = value)) +
        geom_bar(
          aes(fill = sentiment),
          color = "gray40",
          size = 0.5,
          stat = "identity"
        ) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          title = element_text(
            face = "bold",
            size = 17.5,
            hjust = 0.5
          ),
          strip.text.x = element_text(size = 17),
          axis.text = element_text(size = 16),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 17)
        ) +
        xlab("Sentiment Category") +
        ylab("Emotional Valence Score") +
        scale_fill_viridis_d("", direction = -1) +
        facet_wrap(. ~ site, scales = "free", ncol = max(sites$url))
    )
    
    #write file to temp directory for export later
    saveRDS(plt, "temp/tv_plt.rds")
    
    #display plot
    grid::grid.newpage()
    grid::grid.draw(plt)
    
  }
  
  #Text Valence Reactive
  text_valence <-
    eventReactive(input$submit_ta, {
      text_valence_fun()
    })
  
  #Header Tags Table Display Reactive Function
  htags_out <- eventReactive(input$submit_ht, {
    #add functions to build output data.frame.
    
    progress <- Progress$new(session, min = 1, max = 3)
    on.exit(progress$close())
    progress$set(message = 'Gathering header tags', detail = '')
    progress$set(value = 1)
    
    #Iterate over websites
    urls = input_urls(input)
    if (length(urls) < 1) {
      shiny::validate(FALSE, "You must enter atleast one valid URL.")
    }
    
    progress$set(value = 2)
    
    out = lapply(urls, function(url)
      get_H_tags(url)) %>%
      do.call("cbind", .) %>%
      data.frame
    names(out) = sapply(urls, function(url)
      formatDomain(url))
    out = cbind(data.frame(Tag = paste0("h", 1:6)), out)
    return(out)
    
    progress$set(value = 3)
    
  })
  
  #Website Colors Table Display Function
  webcolors_fun <- function() {
    progress <- Progress$new(session, min = 1, max = 4)
    on.exit(progress$close())
    progress$set(message = 'Analyzing webpage color palettes',
                 detail = 'This may take several moments, depending on the number of sites analyzed and choice of resolution.')
    progress$set(value = 1)
    
    #Iterate over websites
    urls = input_urls(input)
    if (length(urls) < 1) {
      shiny::validate(FALSE, "You must enter atleast one valid URL.")
    }
    
    progress$set(value = 2)
    
    pal <-
      lapply(urls, function(url)
        web_colors(
          url,
          color_names = input$color_names,
          num_colors = input$num_colors,
          zoom = input$zoom
        )) #returns data for the color palette from URL(s)
    
    #define plot theme and variables
    ggtitle. <-
      paste0("Top ", input$num_colors, " Color Centers")
    plot.theme <-
      theme(plot.title = element_text(hjust = 0.5, size = 15))
    
    allPals <- do.call("rbind", pal) %>%
      data.frame
    allPals$site = factor(urls[rep(1:length(pal), each = nrow(allPals) /
                                     length(pal))], levels = urls)
    ncol. = ifelse(N_sites() < 4, N_sites(), ifelse(N_sites() == 4, 2, 3))
    nrow. = ifelse(N_sites() >= 4, 2, 1)
    
    #create grid of color values and add names
    grobs <- ggplotify::as.grob(
      ggplot(allPals, aes(
        x = reorder(color, prop),
        y = prop * 100,
        fill = color
      )) +
        ggtitle(ggtitle.) +
        theme_classic() +
        theme(
          panel.background = element_rect(fill = "gray85"),
          legend.title = element_text(size = 14.5, face = "bold"),
          legend.text = element_text(size = 14),
          plot.title = element_text(
            face = "bold",
            size = 17.5,
            hjust = 0.5
          ),
          strip.text.x = element_text(size = 15),
          strip.text.y = element_text(size = 15),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 17)
        ) +
        geom_bar(
          stat = "identity",
          color = "gray30",
          size = 0.5
        ) +
        scale_fill_identity() +
        coord_flip() +
        labs(y = "% of Page", x = "") +
        facet_wrap(
          . ~ site,
          scales = "free",
          ncol = ncol.,
          nrow = nrow.
        )
    )
    
    progress$set(value = 3)
    
    #write file to temp directory for export later
    saveRDS(grobs, "temp/webcolors_plt.rds")
    
    #display plot
    grid::grid.newpage()
    grid::grid.draw(grobs)
    
  }
  
  #Website Colors Reactive
  webcolors_out <-
    eventReactive(input$submit_wc, webcolors_fun())
  
  #Website Fonts Display Function
  webfonts_fun <- function() {
    progress <- Progress$new(session, min = 1, max = 4)
    on.exit(progress$close())
    progress$set(message = 'Preparing list of URLs',
                 detail = '')
    progress$set(value = 1)
    
    urls = input_urls(input)
    
    progress$set(message = 'Searching HTML and CSS stylesheets for fonts encodings.',
                 detail = '')
    progress$set(value = 2)
    
    outputs <-
      lapply(urls, function(x) {
        progress$set(message = 'Gathering links to examples.',
                     detail = '')
        progress$set(value = 3)
        w <- WebFonts(x) #initialize object
        tryCatch(
          w$get_fonts(),
          error = function(e) {
            w$fonts <- NaN
          }
        )
        fonts <- w$fonts
        lapply(fonts, function(z) {
          if (is.nan(z)) {
            return(data.frame(
              URL = x,
              Fonts = "No font(s) found",
              Example = NA
            ))
          } else{
            return(font_function(z, x))
          }
        })
      })
    
    progress$set(message = 'Formatting output table.',
                 detail = '')
    progress$set(value = 4)
    
    output <- lapply(outputs, function(x)
      do.call("rbind", x)) %>%
      do.call("rbind", .)
    return(output)
  }
  #Website Fonts Reactive
  webfonts_out <-
    eventReactive(input$submit_wf, webfonts_fun())
  
  #Google SERP Display Reactive Function
  google_serp_out <- eventReactive(input$search2, {
    if (input$query == "") {
      shiny::validate(FALSE, "Error: your search query must not be empty.")
    }
    
    progress <- Progress$new(session, min = 1, max = 3)
    on.exit(progress$close())
    progress$set(message = 'Querying Google.com',
                 detail = '')
    progress$set(value = 1)
    
    #get Google results page by calling python code.
    dt = get_top_google_results(input$query, num_results = input$num_results)
    
    progress$set(message = 'Gathering top search results',
                 detail = '')
    progress$set(value = 2)
    
    #format results data.frame for export and display
    dt2 = cbind(as.numeric(rownames(dt)), dt)
    names(dt2) = c("Rank", "Title", "URL")
    url_parse. = xml2::url_parse(dt2$URL)
    dt2$Domain = with(url_parse., paste(scheme, server, sep = "://"))
    dt2$URL <- paste0("<a href='", dt2$URL, "'>", dt2$URL, "</a>")
    rownames(dt2) = NULL
    
    progress$set(message = 'Formatting table',
                 detail = '')
    progress$set(value = 3)
    
    return(dt2)
    
  })
  
  #Google Trends API Plotting Function
  gtrends_fun = function() {
    #add functions to build output data.frame.
    
    progress <- Progress$new(session, min = 1, max = 5)
    on.exit(progress$close())
    progress$set(message = 'Submitting keywords to Google trends.',
                 detail = '')
    progress$set(value = 1)
    
    #Check keyword phrases
    kws <-
      c(input$keyword1,
        input$keyword2,
        input$keyword3,
        input$keyword4,
        input$keyword5)
    kws <- kws[kws != ""]
    kws <- sapply(kws, function(x)
      str_trim(x, side = "both"))
    
    #Check that there is atleast one keyword phrase
    if (length(kws) < 1) {
      shiny::validate(FALSE, "You must enter atleast one keyword phrase.")
    }
    #Check that there is atleast one country selected for geographic interest
    if (length(input$country) == 0) {
      shiny::validate(FALSE, "You must select one country of interest.")
    }
    
    #query = c("occidental tool belt","leather tool belt","diamondback tool belt","klein tool belt","atlas 46 tool belt")
    #Pull data from Google trends
    to. = as.Date(Sys.time())
    from. = to. - input$from. * (365 / 12)
    date_range = paste(from., to.)
    date_range_title = paste(from., to., sep = " to ")
    
    #need to iterate across keyword OR countries or 'gtrends' will spit back an error
    g_raw <- lapply(input$country, function(country) {
      #Convert selected country names to ISO codes
      country. <- with(codes, country_code[label == country])
      d <- gtrends(
        keyword = kws,
        time = date_range,
        geo = country.,
        gprop = "web",
        onlyInterest = FALSE
      )
      if (!is.null(d$interest_over_time)) {
        d$interest_over_time$country <- country
        return(d)
      }
      
    })
    names(g_raw) <- input$country
    
    g <- lapply(g_raw, function(g)
      g$interest_over_time) %>%
      do.call("rbind", .)
    if (is.null(g)) {
      shiny::validate(
        FALSE,
        "Google Trends: No data returned. Try a different keyword phrase or geographic origin."
      )
    }
    
    #check that there is data for all countries
    totalHitsByCountry <- with(g, tapply(hits, list(geo), sum))
    if (any(totalHitsByCountry == 0)) {
      #logical vector indicating which rows to keep (FALSE) or remove (TRUE)
      countriesToRemove <- names(which(totalHitsByCountry == 0))
      g <- g[!g$geo %in% countriesToRemove, ]
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
      title = element_text(
        face = "bold",
        size = 17.5,
        hjust = 0.5
      ),
      strip.text.x = element_text(size = 15),
      strip.text.y = element_text(size = 15),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 17)
    )
    
    if (n_unique_geo == 1) {
      #make plots
      p1 = (
        ggplot(
          g,
          mapping = aes(
            x = as.Date(date),
            y = hits,
            group = keyword,
            color = keyword,
            fill = keyword
          )
        ) +
          ggtitle(paste(
            "Keyword Searches Over Time\n", date_range_title
          )) +
          theme_minimal() +
          plot_theme +
          guides(color = guide_legend(nrow = 2, ncol = 3)) +
          scale_x_date("Date") +
          labs(y = "Interest") +
          geom_line(size = 1) +
          scale_color_viridis_d(
            direction = 1,
            begin = 0,
            end = 0.9
          )
      )
      
      g = g[order(g$keyword), ] %>%
        as_tibble
      g$cumsum_hits = with(g, tapply(hits, list(keyword), cumsum)) %>%
        unlist
      
      progress$set(message = 'Plotting the results (2/2).',
                   detail = '')
      progress$set(value = 3)
      
      p2 = (
        ggplot(
          g,
          mapping = aes(
            x = as.Date(date),
            y = cumsum_hits,
            group = keyword,
            color = keyword,
            fill = keyword
          )
        ) +
          ggtitle(
            paste(
              "Cumulative Keyword Interest Over Time\n",
              date_range_title
            )
          ) +
          theme_minimal() +
          plot_theme +
          guides(color = guide_legend(nrow = 2, ncol = 3)) +
          scale_x_date("Date") +
          ylab("Total Interest") +
          geom_line(size = 1) +
          scale_color_viridis_d(
            direction = 1,
            begin = 0,
            end = 0.9
          )
      )
      
      grobs <-
        list(ggplotify::as.grob(p1), ggplotify::as.grob(p2))
      grobs2 <- gridExtra::arrangeGrob(grobs = grobs, ncol = 2)
      
    } else{
      #make plots
      p1 = (
        ggplot(
          g,
          mapping = aes(
            x = as.Date(date),
            y = hits,
            group = keyword,
            color = keyword,
            fill = keyword
          )
        ) +
          ggtitle(paste(
            "Keyword Searches Over Time\n", date_range_title
          )) +
          theme_bw() +
          plot_theme +
          guides(color = guide_legend(nrow = 2, ncol = 3)) +
          scale_x_date("Date") +
          labs(y = "Hits") +
          geom_line(size = 1) +
          scale_color_viridis_d(
            direction = 1,
            begin = 0,
            end = 0.9
          ) +
          facet_grid(country ~ ., switch = "y")
      )
      
      g = g[order(g$geo, g$keyword), ] %>%
        as_tibble
      g$cumsum_hits = with(g, tapply(hits, list(keyword, geo), cumsum)) %>%
        unlist
      
      progress$set(message = 'Plotting the results (2/2).',
                   detail = '')
      progress$set(value = 3)
      
      p2 = (
        ggplot(
          g,
          mapping = aes(
            x = as.Date(date),
            y = cumsum_hits,
            group = keyword,
            color = keyword,
            fill = keyword
          )
        ) +
          ggtitle(
            paste(
              "Cumulative Keyword Searches Over Time\n",
              date_range_title
            )
          ) +
          theme_bw() +
          plot_theme +
          guides(color = guide_legend(nrow = 2, ncol = 3)) +
          scale_x_date("Date") +
          ylab("Total Hits") +
          geom_line(size = 1) +
          scale_color_viridis_d(
            direction = 1,
            begin = 0,
            end = 0.9
          ) +
          facet_grid(country ~ ., switch = "y")
      )
      
      grobs <-
        list(ggplotify::as.grob(p1), ggplotify::as.grob(p2))
      grobs2 <- gridExtra::arrangeGrob(grobs = grobs, ncol = 2)
      
    }
    
    progress$set(message = 'Creating geographic plots of keyword interest.',
                 detail = '')
    progress$set(value = 4)
    
    if (any(input$country == "United States")) {
      #need to match states with data with rows in the polygon data frame
      g2 <- g_raw[["United States"]]
      hits = g2$interest_by_region %>%
        as_tibble() %>%
        dplyr::filter(!is.na(g2$interest_by_region$hits)) %>%
        with(., tapply(hits, list(location, keyword), sum)) %>%
        reshape::melt(.) %>%
        dplyr::filter(complete.cases(.)) %>%
        dplyr::rename(., state = X1, keyword = X2)
      
      ii = match(hits$state, centroids$names)
      hits$xy = centroids[ii, c("x", "y")]
      
      #plot geographic variation in searches (USA only)
      p3 = (
        ggplot(map48) + #map48
          ggtitle("Search Volume by U.S. State") +
          xlab("") + ylab("") +
          plot_theme +
          theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.box = "vertical",
            panel.background = element_rect(fill = NULL, color =
                                              "black")
          ) +
          guides(color = guide_legend(nrow = 2, ncol = 3)) +
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
      gridExtra::arrangeGrob(
        grobs = list(
          ggplotify::as.grob(p1),
          ggplotify::as.grob(p2),
          ggplotify::as.grob(p3)
        ),
        ncol = 2,
        layout_matrix = matrix(c(1, NA, 1, 3, 2, 3, 2, NA), nrow = 2)
      )
    grid::grid.newpage()
    grid::grid.draw(grobs)
    
    ## Save plots to local storage for output later on
    progress$set(message = 'Saving plots for output.',
                 detail = '')
    progress$set(value = 5)
    
    #write files to temp directory for export later
    saveRDS(p1, "temp/gtrends_plt1.rds")
    saveRDS(p2, "temp/gtrends_plt2.rds")
    saveRDS(p3, "temp/gtrends_plt3.rds")
    
  }
  
  #Google Trends Plotting Reactive
  gtrends_out = eventReactive(input$submit_gt, gtrends_fun())
  
  #Google Business Reviews Function
  google_reviews_fun <- function(){
    
    if(file.exists("temp/greviews/greviews_data.rds")){
      file.remove("temp/greviews/greviews_data.rds")
    }
    
    progress <- Progress$new(session, min=1, max=3)
    on.exit(progress$close())
    progress$set(message = '1) POST Request',
                 detail = 'Sending customized task request to API server.')
    progress$set(value = 1)
    
    progress$set(message = '2) GET Request',
                 detail = 'Waiting for response to task request. This may take several minutes.')
    progress$set(value = 2)
    
    report <-
      dataforseo_google_reviews(
        keyword = input$dfseo_keyword,
        city = input$dfseo_city,
        state = input$dfseo_state,
        depth = input$dfseo_depth,
        usn = dataforseo$usn,
        passw = dataforseo$passw
      )
    
    progress$set(message = '3) Data Received',
                 detail = 'Preparing visulizations.')
    progress$set(value = 3)
    
    
    p1 <- ggplotify::as.grob(report$hist_plot)
    p2 <- ggplotify::as.grob(report$trend_plot)
    d <- report$data
    
    #draw all plots
    grobs <-
      gridExtra::arrangeGrob(
        grobs = list(
          p1,
          p2
        ),
        ncol = 2
      )
    
    pdf("temp/greviews/greviews_plot1.pdf")
    grid::grid.newpage()
    grid::grid.draw(p1)
    grid::grid.text(
      sprintf("Source: Data4SEO - %s", as.Date(Sys.time())),
      x = unit(0.8, "npc"),
      y = unit(0.025, "npc")
    )
    dev.off()
    
    pdf("temp/greviews/greviews_plot2.pdf")
    grid::grid.newpage()
    grid::grid.draw(p2)
    grid::grid.text(
      sprintf("Source: Data4SEO - %s", as.Date(Sys.time())),
      x = unit(0.8, "npc"),
      y = unit(0.025, "npc")
    )
    dev.off()
    
    saveRDS(report, "temp/greviews/greviews_data.rds")
    
    grid::grid.newpage()
    grid::grid.draw(grobs)
    
  }
  
  
  #Google Reviews Plotting Reactive
  google_reviews_plots = eventReactive(input$dfseo_submit, google_reviews_fun())
  
  #Google Business Reviews - plots
  output$dfseo_plots <-
    renderPlot(google_reviews_plots(), width = 850)
  
  #Google Reviews Table Reactive
  google_reviews_table = eventReactive(input$dfseo_submit, {
    tab = readRDS("temp/greviews/greviews_data.rds")
    return(tab$data)
  })
  
  #Google Business Reviews - table
  output$dfseo_table <- DT::renderDataTable({
    google_reviews_table()
  }, rownames = FALSE, escape = FALSE, options = list(
    scrollX = TRUE,
    scrollCollapse = TRUE,
    lengthMenu = list(c(10, 20, -1), c('10','20', 'All')),
    pageLength = 10
  ))
  
  
  ## Render plots and tables for display
  #Wordcloud plots
  output$WordCloud <- renderPlot({
    wordcloud_out()
  },
  height = function()
    plf_wc(N_sites())[1],
  width = function()
    plf_wc(N_sites())[2])
  
  #Webcolors search and plots
  output$Webcolors <- renderPlot({
    webcolors_out()
  },
  height = function()
    plf_wc(N_sites())[1],
  width = function()
    plf_wc(N_sites())[2])
  
  #Text valence plots
  output$Valence <- renderPlot({
    text_valence()
  },
  height = function()
    plf_wc(N_sites())[1],
  width = function()
    plf_wc(N_sites())[2])
  
  #Scrape and display header tags
  output$Htags <-
    renderTable({
      htags_out()
    }, bordered = TRUE, align = "c", spacing = "s")
  
  #Dispay results of Google search
  output$SERP <-
    DT::renderDataTable({
      google_serp_out()
    }, rownames = FALSE, escape = FALSE, options = list(lengthMenu = list(c(10,-1), c('10', 'All')),
                                                        pageLength = -1))
  #-create warning for SERP display page
  output$serp_warnings <-
    renderText(
      "WARNING: Too many searches in a short time will cause Google to temporarily block the IP address."
    )
  
  #Display fonts from HTML scrape
  output$Webfonts <-
    DT::renderDataTable({
      webfonts_out()
    }, escape = FALSE, rownames = FALSE, options = list(lengthMenu = list(c(-1, 10, 20), c('All', '10', '20')),
                                                        pageLength = -1))
  
  #Display plots from Google Trends
  output$gtrends_plots <-
    renderPlot(gtrends_out(),
               width = 1000,
               height = 800)
  
  
  ## Export saved plots and data
  output$dl_wordcloud <- downloadHandler(
    filename = "word_cloud.pdf",
    content = function(file) {
      ras <- readRDS("temp/wc_plt.rds")
      pdf(file, width = (2 + 3 * N_sites()), height = 5)
      plot(ras)
      dev.off()
    }
  )
  
  output$dl_textvalence <- downloadHandler(
    filename = "text_valence.pdf",
    content = function(file) {
      ras <- readRDS("temp/tv_plt.rds")
      pdf(file, width = (2 + 3 * N_sites()), height = 5)
      #grid.newpage()
      plot(ras)
      dev.off()
    }
  )
  
  output$dl_webcolors <- downloadHandler(
    filename = "webpage_color_palettes.pdf",
    content = function(file) {
      ras <- readRDS("temp/webcolors_plt.rds")
      dims = plf_wc(N_sites()) / 65
      pdf(file, height = dims[1], width = dims[2])
      #grid.newpage()
      plot(ras)
      dev.off()
    }
  )
  
  output$dl_webshots <- downloadHandler(
    filename = 'webshots.zip',
    content = function(file) {
      setwd("temp/webshot/")
      urls = input_urls(input)
      
      #location of temp file for website screenshot
      fs = paste0(
        paste(
          xml2::url_parse(urls)$server,
          xml2::url_parse(urls)$path,
          input$zoom,
          sep = "-"
        ),
        ".jpeg"
      )
      zip(zipfile = file, files = fs)
      if (file.exists(paste0(file, ".zip"))) {
        file.rename(paste0(file, ".zip"), file)
      }
    },
    contentType = "application/zip"
  )
  
  output$dl_gtrends1 <- downloadHandler(
    filename = "google_trends_1.pdf",
    content = function(file) {
      ras <- readRDS("temp/gtrends_plt1.rds")
      pdf(file, width = 7, height = 5.5)
      plot(ras)
      dev.off()
    }
  )
  
  output$dl_gtrends2 <- downloadHandler(
    filename = "google_trends_2_cumulative.pdf",
    content = function(file) {
      ras <- readRDS("temp/gtrends_plt2.rds")
      pdf(file, width = 7, height = 5.5)
      plot(ras)
      dev.off()
    }
  )
  
  output$dl_gtrends3 <- downloadHandler(
    filename = "google_regional_interest.pdf",
    content = function(file) {
      ras <- readRDS("temp/gtrends_plt3.rds")
      pdf(file, width = 7.5, height = 5.5)
      plot(ras)
      dev.off()
    }
  )
  
  #Download google reviews plots
  output$dl_dfseo_plots <- downloadHandler(
    filename = 'google_reviews_fig.zip',
    content = function(file) {
      #location of temp file for website screenshot
      fs = list.files("temp/greviews/", pattern = ".pdf", full.names = TRUE)
      zip(zipfile = file, files = fs)
      if (file.exists(paste0(file, ".zip"))) {
        file.rename(paste0(file, ".zip"), file)
      }
    },
    contentType = "application/zip"
  )
  
}

shinyApp(ui = ui, server = server)
