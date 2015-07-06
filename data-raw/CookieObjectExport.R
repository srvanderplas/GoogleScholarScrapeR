cookie.bibtex <- read.table("data-raw/googlescholarCookies.tsv", stringsAsFactors = F)
newcookie <- cookie.bibtex[,2]
names(newcookie) <- cookie.bibtex[,1]

devtools::use_data(newcookie, pkg=".", internal=TRUE)
