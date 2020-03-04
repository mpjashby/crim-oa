# This script queries Sci-Hub via Tor to determine whether a paper is available.
# Sci-Hub appears to 

library("httr")

# get current Sci-Hub base URL, which changes periodically
sci_hub_url <- pluck(GET("https://whereisscihub.now.sh/go"), "url")

sci_hub_articles <- articles_data %>% 
  select(doi) %>% 
  sample_n(100) %>% 
  mutate(
    sci_hub_text = map(doi, function (doi) {
      message(glue::glue("Checking {doi}"))
      response <- GET(glue::glue("{sci_hub_url}{doi}"))
      list(headers = headers(response), content = content(response, "text"))
      Sys.sleep(2)
    })
  )

sci_hub_urls <- map(head(sci_hub_articles$sci_hub_text), function (obj) {
  str(obj)
  # xml2::read_html(obj$content) %>% 
  #   rvest::html_node("div#article iframe") %>% 
  #   rvest::html_attr("src")
})

