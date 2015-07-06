#' Retrieve bibtex citation from google scholar, given the unique reference code
#'
#' @param cite Google scholar reference code
#' @importFrom magrittr extract
#' @importFrom magrittr extract2
#' @importFrom magrittr %>%
#' @importFrom rvest follow_link
#' @importFrom rvest html_session
#' @importFrom httr add_headers
#' @importFrom httr set_cookies
getBibtex <- function(cite){
  userAgent <- c("user-agent"="Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36")

  # Read google scholar cookie authorization to set bibtex as the default
  load("data/Cookie.Rdata")

  citation <- cite %>% sapply(
    FUN=function(i) {
      if(is.na(i)) return(NA)
      html_session(
        sprintf("https://scholar.google.com/scholar?q=info:%s:scholar.google.com/&output=cite&scirp=0&hl=en", i),
        add_headers(userAgent),
        set_cookies(newcookie)
      ) %>%
      follow_link("BibTeX") %>%
      extract2('response') %>%
      as.character() %>%
      extract(1) %>%
      as.character()
    })
}

#' Assemble the data from a page of google search responses
#'
#' @param URL link to google search responses
#' @importFrom magrittr extract
#' @importFrom magrittr extract2
#' @importFrom magrittr %>%
#' @importFrom rvest follow_link
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_session
#' @importFrom rvest html_children
#' @importFrom rvest html_name
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom httr add_headers
#' @importFrom httr set_cookies
scraper_internal <- function(URL) {
  doc <- read_html(URL)
  res <- html_nodes(doc, xpath="//div[@class='gs_ri']")

  # citation:
  citekey <- html_nodes(doc, xpath="//*/a[@class='gs_nph'][@onclick]") %>%
    html_attr("onclick") %>%
    str_extract("\\'[\\w-]{12}\\'") %>%
    str_replace_all("\\'", "")

  # Pull out db rows
  cite <- citekey %>% getBibtex()
  type <- str_extract(cite, "@(\\w{1,})") %>%
    str_replace("@", "")
  title <- str_extract(cite, "title=\\{.*?\\},?\\n") %>%
    str_replace("title=\\{(.*)?\\},?\\n", "\\1")
  booktitle <- str_extract(cite, "booktitle=\\{.*?\\},?\\n") %>%
    str_replace("booktitle=\\{(.*)?\\},?\\n", "\\1")
  authors <- str_extract(cite, "author=\\{.*?\\},?\\n") %>%
    str_replace("author=\\{(.*)?\\},?\\n", "\\1")
  journal <- str_extract(cite, "journal=\\{.*?\\},?\\n") %>%
    str_replace("journal=\\{(.*)?\\},?\\n", "\\1")
  publisher <- str_extract(cite, "publisher=\\{.*?\\},?\\n") %>%
    str_replace("publisher=\\{(.*)?\\},?\\n", "\\1")
  year <- str_extract(cite, "year=\\{.*?\\},?\\n") %>%
    str_replace("year=\\{(.*)?\\},?\\n", "\\1") %>%
    as.numeric()
  # link:
  link <- html_nodes(doc, xpath="//div[@class='gs_ri']/h3[@class='gs_rt']") %>%
    lapply(FUN=function(i){
      tmp <- html_children(i)
      linkidx <- which(tmp %>% html_name()=='a')
      if(length(linkidx)==0) return(NA)
      tmp %>% extract2(linkidx[1]) %>%
        html_attr('href') %>%
        extract(1)
    }) %>% unlist()

  # summaries are truncated, and thus wont be used..
  # abst <- xpathSApply(doc, '//div[@class='gs_rs']', xmlValue)
  # ..to be extended for individual needs
  options(warn=(-1))
  dat <- data.frame(
    citekey = citekey,
    type = type,
    title = title,
    authors = authors,
    journal = journal,
    booktitle = booktitle,
    publisher = publisher,
    year = year,
    link = link,
    cite = cite
  )
  options(warn=0)
  return(dat)
}

#' Function to search google scholar by author and/or institution
#'
#' @param author Author's name. This works best in "lastname, firstname" format but any format is accepted (search results may vary)
#' @param institution Institution name, defaults to ""
#' @param write Write results to table? Can be TRUE or the name of the output file. Defaults to FALSE, which returns the results.
#' @export
#' @importFrom digest digest
#' @importFrom stringr str_sub
#' @importFrom stringr str_replace_all
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom magrittr %>%
#'
author_search <- function(author, institution="", write = F) {


  googleScholarUrl <- "http://scholar.google.com"

  # fake google id (looks like it is a 16 elements hex)
  googleId <- sample(1000000:9999999, 1) %>% digest::digest(algo="md5") %>% stringr::str_sub(start=1, end=15)
  # "7e6cc990821af63"

  # Set User-Agent to appear not to be a bot
  oldUserAgent <- options("HTTPUserAgent")
  options("HTTPUserAgent"="Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36")

  # putting together the search-URL:
  query <- sprintf("author:'%s' %s", author, institution)
  resultsURL <- paste0(
    "http://scholar.google.com/scholar?q=",
    query %>%
      str_replace_all(pattern=" ", replacement="%20"))

  message("The URL used is: ", "\n----\n", resultsURL)

  # get content and parse it:
  doc <- read_html(resultsURL)

  # number of hits:
  h1 <- doc %>%
    html_nodes(xpath="//div[@id='gs_ab_md']") %>%
    html_text()
  h2 <- unlist(strsplit(h1, "\\s"))
  # in splitted string it is the second element which contains digits,
  # grab it and remove decimal signs and convert to integer
  num <- as.integer(gsub("[[:punct:]]", "", h2[grep("\\d", h2)[1]]))

  message(paste0("\n\nNumber of hits: ", num, "\n----\n", "If this number is far from the returned results\nsomething might have gone wrong..\n\n", sep = ""))

  # If there are no results, stop and throw an error message:
  if (num == 0 | is.na(num)) {
    stop("\n\n...There is no result for the submitted search string!")
  }

  pages.max <- ceiling(num/20)

  # 'start' as used in URL:
  start <- 20 * 1:pages.max - 20

  # Collect URLs as list:
  URLs <- paste0(resultsURL, sprintf("&start=%d", start))

  result <- do.call("rbind", lapply(URLs, scraper_internal))

  # Reset userAgent to previous value
  options("HTTPUserAgent"=oldUserAgent)

  if (write || is.character(write)) {
    if(is.character(write)){
      write.table(result, write, sep = ";",
                  row.names = F, quote = F)
    } else {
      write.table(result, "GScholar_Output.csv")
    }
  } else {
    return(result)
  }
}
