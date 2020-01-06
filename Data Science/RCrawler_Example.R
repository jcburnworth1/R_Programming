## Load required libraries
library(Rcrawler)
library(dplyr)
library(tidyr)
library(listviewer)
library(stringr)

## Keywords to look for - This is optional
keywords = as.character(c('Specialty','Specialties','Degree','Credentials','Education',
                          'Specializes','Field','Experience','School','Trained'))

## Also create uppercase and lowercase versions
keywords_UPPERCASE= toupper(keywords)
keywords_lowercase= tolower(keywords)

## Run the crawl -
practitioner_crawler <- lapply( 
  "www.gecc.org",
  Rcrawler,
  no_cores = 4,
  no_conn = 4,
  MaxDepth = 5, 
  DIR = "~/Documents/Code/R/Code_Snippets/Crawler_Outputs/",
  # KeywordsFilter = c(keywords, keywords_UPPERCASE,
  #                    keywords_lowercase),
  NetworkData = TRUE)

INDEX_key = INDEX %>% separate(Url, sep="/", into=c("URL_ROOT","URL_2","URL3","URL4","URL5","URL6","URL7"))

##### Cleaning portion
## Grab directory paths for sites webcrawler scraped
siteList <- list.dirs('Centene/State_of_WA/Crawler_Outputs')
if(length(siteList) > 1) {
  ## Remove the root directory
  siteList <- siteList[2:length(siteList)]}

## Load HTML files into a list
DataHTML.CHCSNO_list <- lapply(siteList, function(x) {
  print(x)  # status in case something breaks
  if(length(list.files(x)) > 0) # Only load if there are files
    LoadHTMLFiles(x, type='list') # else, nothing
})

DataHTML_list_names <- list.dirs('Centene/State_of_WA/Crawler_Outputs', full.names = FALSE)
if(length(DataHTML_list_names) > 1) {
  DataHTML_list_names <- DataHTML_list_names[2:length(DataHTML_list_names)]} # remove root directory

DataHTML_list_names <- str_replace(DataHTML_list_names, '-\\d+$', '') # replace numbers at end of directory name to get root url
DataHTML_list_names
# Name the list with root url
names(DataHTML.CHCSNO_list) <- DataHTML_list_names # name the list

## Function Nick found online to get rid of HTML and leave text only, slightly modified
htmlToText <- function(input, ...) {
  require(RCurl)
  require(XML)
  #--- LOCAL FUNCTIONS ---#
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE) | grepl("</div>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}

## Maybe use sapply instead of the inner lapply? Meh, who cares.
## List of lists of character
txt_list <- lapply(DataHTML.CHCSNO_list, function(x){ 
  lapply(x, function(y) {
    htmlToText(y)
  })
})

## Remove extra spacing and linebreaks to make parsing phone, address, etc. easier
txt_list_clean <- lapply(txt_list, function(site){
  sapply(site, function(y){
    gsub('(\r\n[ ]?){3,}', '\r\n\r\n', # final swoop
         gsub('•', '',
              gsub('(\n ){2,}', '\n ',
                   gsub('(\r){2,}', '\r', # special case of odd carriage return   
                        gsub('(\r\n[ ]?){2,}', '\r\n\r\n ', # Collapse triple+ space to doublespace
                             gsub('[ ]{2,}', ' ', # get rid of extra space
                                  gsub('\t', ' ', # no more tabs
                                       gsub('(\t){2,}', '\t', # multiple tabs collapse
                                            y)))))))
    )
  })
})

## Use txt_list_clean$yvfwc.com[140] as an example
## Regex - looks for patterns like Abbb Cddd, Ph.D. and Dr. Haoxin Sun; Middle Inits allowed (required when titles are missing)
## Titles at most 4 characters?
## Not perfect, but meh
## (?<=\\n\\n )[\\w\\s[:punct:]]+?(?=\\n)
prac_re <- "\\b(Dr[.] [A-Z][[:alpha:]]+ ([A-Z][.] ){0,2}[A-Z][[:alpha:]]+(, [A-Z]([-.[:alpha:].]){1,4}){0,2}|[A-Z][[:alpha:]]+ ([A-Z][.] ){0,2}[A-Z][[:alpha:]]+(, [A-Z]([-[:alpha:].]){1,4}){1,3}|[A-Z][[:alpha:]]+ ([A-Z][.]){1,2} [A-Z][[:alpha:]]+)(\\s?\\n){2,3}.*"
## Filter out Inc. etc. afterwards; how to filter out things like 'Walla Walla, WA'? Will do a check against valid degrees later
practitioners <- lapply(txt_list_clean, function(x) str_extract_all(x, prac_re))
jsonedit(practitioners) # lots of false positives (, Ste and City, ST)
## Apply filter - commonly observed false positives, State Names, State Abbrevs that are not also acceptable degrees... more filtering later
practitioners_filtered <- lapply(practitioners, function(x) lapply(x, function(y) y[!grepl(paste0(', (All|WA|Wa|Suite|Ste|Inc|INC|LLC|', paste(state.name, collapse='|'), ')[.]?$'), y)]))
jsonedit(practitioners_filtered) # lots of false positives still

## Flatten list results
prac_scraped_df <- NULL
for(url in DataHTML_list_names){
  prac_raw <- unique(unlist(practitioners_filtered[url]))
  if(length(prac_raw) > 0){
    tmp <- data.frame(root_url = url, prac_raw_str = prac_raw, stringsAsFactors=FALSE)
    prac_scraped_df <- prac_scraped_df %>% bind_rows(tmp)
  }
}

## Save parsed out results - duplicates still present, e.g. when 'Dr. Nick Mikulec' appears with 'Nick Mikulec, M.D.'
## NOTE: Get list of accepted degrees (500-600) for filtering
prac_scraped_df <- prac_scraped_df %>% mutate(fname = str_extract(str_replace(prac_raw_str, 'Dr[.] ', ''), '.+?(?=[[:space:]])'),
                                              mname = str_extract(prac_raw_str, '(?<=[:alpha:] )([A-Z][.][ ]?){1,2}(?=[A-Z])'),
                                              lname = str_extract(str_replace(prac_scraped_df$prac_raw_str, ', .*$', ''), "(?<=[[:space:]])['[:alpha:]]+?$"),
                                              deg = toupper(gsub('[.]', '', str_extract(prac_raw_str, '(?<=, ).*$'))), # removes periods, not '-' though, all caps
                                              prefix = str_extract(prac_raw_str, '^Dr[.] '),
                                              specialty = )

### Read in common degree types - 1546 rows including empty
accepted_degrees <- read.csv('~/Desktop/degrees.csv', stringsAsFactors=FALSE)
## Filter to those appearing 50+ times - top 164 combos including blank - not perfect but who g.a.f.
accepted_degrees <- accepted_degrees %>% filter(DEG_COUNT >= 50)
## Parse out and choose unique - KEEP THIS LINE, BECKY STOP DELETING IT GEEZ
accepted_degrees <- accepted_degrees %>% mutate(degree_vec = str_split(DEGREE, ', ')) %>% select(degree_vec) %>% unlist() %>% toupper() %>% unique()
str(accepted_degrees) # 159
## Still need to cleanse a little, worry about that later

## Check if ANY of captured degrees is in the list of accepted degrees
prac_scraped_filtered_df <- prac_scraped_df %>% rowwise() %>% do({
  result = as_data_frame(.)
  result$degree_vec = str_split(result$deg, ', ') # returns a vector
  result$valid_degrees = any(unlist(result$degree_vec) %in% accepted_degrees) | is.na(result$deg) # NA degrees when Dr. or a fullname like Drazen N. Mikulec
  result
}) %>% ungroup() %>% filter(valid_degrees)

View(prac_scraped_filtered_df)