library(rvest)

iso_codes <- matrix(nrow = 249, ncol = 2)
site <- session("https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3")
for (i in seq_len(nrow(iso_codes))) {
  code <- site %>%
    html_nodes(paste0('#mw-content-text > div.mw-parser-output > div:nth-child(11) > div > ul > li:nth-child(',
                      i,
                      ') > span')) %>%
    html_text()
  country_name <- site %>%
    html_nodes(paste0('#mw-content-text > div.mw-parser-output > div:nth-child(11) > div > ul > li:nth-child(',
                      i,
                      ') > a')) %>%
    html_text()
  iso_codes[i, ] <- c(code, country_name[1])
}
