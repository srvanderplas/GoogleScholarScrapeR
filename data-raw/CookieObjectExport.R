cookie.bibtex <- read.table("data-raw/googlescholarCookies.tsv", stringsAsFactors = F)
newcookie <- cookie.bibtex[,2]
names(newcookie) <- cookie.bibtex[,1]

userAgent <- c("user-agent"="Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36")
devtools::use_data(newcookie, userAgent, pkg=".", internal=TRUE, overwrite=T)
