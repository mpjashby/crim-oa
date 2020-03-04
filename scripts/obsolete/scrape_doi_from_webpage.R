# define function for http GET that backs off before repeating and fails quietly
safe_retry <- safely(httr::RETRY)

# extract DOIs from web pages where necessary
bjc_articles$doi <- map2_chr(bjc_articles$link, bjc_articles$doi, function (link, doi) {
  
  link <- str_replace_all(link, "&amp;", "&")
  l <- xml2::url_parse(link)
  
  # AIC, CEPOL and Home Office publications, and articles in Perspectives on
  # Terrorism and the Journal of Criminal Law and Criminology don't have DOIs,
  # so there's no point checking. Maklu RSS feeds link to an issue not an
  # article, so no point checking those, either. Equinox journals have DOIs but
  # the awful HTML on their site means they are easily retrievable. OUP and APA
  # journals have DOIs but use HTTP redirects that make scraping difficult.
  
  if (is.na(doi) & !l$server %in% 
      c("www.gov.uk", "bulletin.cepol.europa.eu", "www.terrorismanalysts.com", 
        "www.maklu-online.eu", "journals.equinoxpub.com", "psycnet.apa.org",
        "scholarlycommons.law.northwestern.edu", "aic.gov.au", "www.fbi.gov")) {
    
    Sys.sleep(5)
    
    cat("\n\n", l$server, l$path, 
        ifelse(str_length(l$query) > 0, 
               paste("?", str_sub(l$query, 0, 20), " ...", sep = ""), ""), 
        "\n", sep = "")
    
    p <- safe_retry("GET", link, times = 2)
    
    if (!is.null(p$error)) {
      
      cat("\ncurl error message:", p$error$message)
      doi_web <- NA_character_
      
    } else if (p$result$status_code != 200) {
      
      cat("\nHTTP response status", p$result$status_code)
      doi_web <- NA_character_
      
    } else if (l$server %in% 
               c("onlinelibrary.wiley.com", "rss.sciencedirect.com")) {
      
      page <- xml2::read_html(content(p$result, "text"))
      
      doi_web <- page %>% 
        rvest::html_node("meta[name=citation_doi]") %>% 
        rvest::html_attr("content")
      
    } else if (l$server == "digitalcommons.uri.edu") {
      
      page <- xml2::read_html(content(p$result, "text"))
      
      doi_web <- page %>% 
        rvest::html_node("meta[name=bepress_citation_doi]") %>% 
        rvest::html_attr("content")
      
    } else if (l$server == "www.ingentaconnect.com") {
      
      page <- xml2::read_html(content(p$result, "text"))
      
      doi_web <- page %>% 
        rvest::html_node("meta[name=DC\\.identifier]") %>% 
        rvest::html_attr("content") %>% 
        str_remove("^info:doi/")
      
    } else if (l$server == "brill.com") {
      
      page <- xml2::read_html(content(p$result, "text"))
      
      doi_web <- page %>% 
        rvest::html_node("meta[name=dc\\.identifier]") %>% 
        rvest::html_attr("content")
      
    } else if (l$server == "academic.oup.com") {
      
      page <- xml2::read_html(content(p$result, "text"))
      
      doi_web <- page %>% 
        rvest::html_node(".ww-citation-primary a") %>% 
        rvest::html_text() %>% 
        str_remove("^https://doi.org/")
      
    } else if (l$server == "psycnet.apa.org") {
      
      page <- xml2::read_html(content(p$result, "text"))
      
      doi_web <- page %>% 
        rvest::html_node(".citation-text a") %>% 
        rvest::html_text() %>% 
        str_remove("^https://doi.org/")
      
    } else {
      
      doi_web <- NA_character_
      
    }
    
  } else {
    
    doi_web <- doi
    
  }
  
  doi_web
  
})

