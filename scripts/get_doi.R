# load packages
library("httr")
library("tidyverse")

# The SQL used to generate articles.csv is:
# SELECT `articles`.`id`, `articles`.`title`, `articles`.`author`, 
#   `journals`.`name`, `articles`.`link`, `articles`.`date`, 
#   `articles`.`timestamp`, `articles`.`clicks` 
# FROM `articles`, `journals` 
# WHERE `articles`.`excluded` = 0 
# AND `articles`.`timestamp` BETWEEN "2017-01-01 00:00:00" 
#   AND "2019-12-31 23:59:59"
# AND `journals`.`active` = 1 
# AND `articles`.`journal` = `journals`.`id`

# load data
articles <- here::here("original_data/articles.csv.gz") %>% 
  read_csv() %>% 
  filter(
    between(timestamp, lubridate::ymd("2017-01-01"), 
            lubridate::ymd_hms("2019-12-31 23:59:59")),
    # exclude sources that don't assign DOIs to articles
    !name %in% c(
      "European Law Enforcement Research Bulletin", 
      "Journal of Criminal Law and Criminology", 
      "Journal of Criminological Research, Policy and Practice",
      "Perspectives on Terrorism", 
      "Research and Analysis publications", 
      "Science and Justice"
    )
  ) %>% 
  rename(journal = name) %>% 
  # remove any duplicate rows
  group_by(link) %>% 
  summarise_all(first)

# find journals that were added to CrimPapers on or after 1 Jan 2018
new_journals <- articles %>% 
  group_by(journal) %>% 
  summarise(first_date = min(timestamp)) %>% 
  filter(first_date >= lubridate::ymd("2018-01-01")) %>% 
  pull("journal")

# exclude those new journals
articles <- filter(articles, !journal %in% new_journals)

# extract DOIs from URL paths where possible
articles <- articles %>% 
  mutate(
    doi = map_chr(link, function (x) {
      l <- xml2::url_parse(x)
      case_when(
        # doi is in query string
        l$server == "onlinelibrary.wiley.com" & l$path == "/resolve/doi" ~ 
          str_extract(l$query, "\\b10\\..+$"),
        # doi is at the end of the URL path
        l$server %in% 
          c("booksandjournals.brillonline.com", "journals.sagepub.com", 
            "link.springer.com", "www.tandfonline.com", "www.annualreviews.org",
            "onlinelibrary.wiley.com", "www.emeraldinsight.com", 
            "www.journals.uchicago.edu", "emeraldinsight.com", 
            "tandfonline.com") | 
          str_detect(l$server, "springeropen.com") | 
          str_detect(l$server, "biomedcentral.com") |
          str_detect(x, "oup.com/policing") ~ 
          str_extract(l$path, "\\b10\\..+$"),
        TRUE ~ NA_character_
      )
    }),
    # strip the final part from the apparent DOI of OUP articles
    doi = ifelse(str_detect(link, "oup.com") & !is.na(doi), 
                 str_remove(doi, "\\/\\d+$"), doi),
    doi = ifelse(
      str_detect(doi, "^10.1093/police/"),
      str_remove(str_extract(doi, "^10.1093\\/police\\/.+?\\/"), "\\/$"), 
      doi
    ),
    # unencode / in DOIs
    doi = str_replace_all(doi, "%2F", "/")
  )

# define function for http GET that backs off before repeating and fails quietly
safe_retry <- safely(httr::RETRY)

# lookup DOIs in the CrossRef API if not already found
cr_doi_tbl <- articles %>%
  mutate(domain = xml2::url_parse(link)$server) %>%
  filter(
    is.na(doi),
    !domain %in% c("www.maklu-online.eu")
  ) %>%
  pluck("title") %>% 
  textclean::replace_non_ascii() %>% 
  map(function (title) {
    
    message("Finding DOI for", title)
    
    Sys.sleep(0.25)
    
    api_res <- title %>%
      urltools::url_encode() %>% {
        str_glue("https://api.crossref.org/works?query=", .,
                 "&mailto=matthew.ashby@ucl.ac.uk&filter=type:journal-article")
      } %>% {
        safe_retry("GET", ., times = 2)
      }
    
    if (is.null(api_res$error)) {
      
      cr_data <- api_res$result %>% 
        content(as = "parsed") %>% 
        pluck("message", "items")
      
      if (length(cr_data) > 0) {

        list(
          title = title,
          cr_doi = cr_data[[1]]$DOI, 
          cr_title = cr_data[[1]]$title[[1]],
          cr_lead_author = cr_data[[1]]$author[[1]]$family,
          cr_journal = cr_data[[1]]$`container-title`[[1]]
        )

      }
      
    }
    
  }) %>% 
  # the CrossRef API returns NULLs if items are missing, which must be replaced
  # with NA before the results can be collapsed into a tibble
  map_depth(2, ~ ifelse(is.null(.), NA, .)) %>% 
  map(as_tibble) %>% 
  bind_rows() %>% 
  # the CrossRef API matches even with low similarity, so we should compare how
  # similar the title returned by CrossRef is to the original title. Titles
  # should not be expected to be identical due to variations in formatting and
  # the possibility of non-ASCII characters being filtered out by
  # textclean::replace_non_ascii() above.
  mutate(
    cr_lead_author_ascii = 
      str_to_lower(textclean::replace_non_ascii(cr_lead_author)),
    cr_journal_ascii = str_replace_all(str_to_lower(
      textclean::replace_non_ascii(cr_journal)), " & ", " and "),
    title_diff = stringdist::stringdist(
      str_to_lower(title), str_to_lower(cr_title), method = "jw")
  ) %>% 
  filter(!str_detect(str_to_lower(cr_title), "supplemental material"),
         title_diff < 0.2)

# DOIs could not be identified for some British Journal of Criminology articles
# despite all articles having them, so these have been manually copied from the
# BJC website and recorded in a separate file
bjc_articles <- read_csv(here::here("output_data/bjc_article_dois.csv"))

articles_with_cr <- articles %>% 
  mutate(
    author_ascii = str_to_lower(textclean::replace_non_ascii(author)),
    title_ascii = textclean::replace_non_ascii(title),
    journal_ascii = str_to_lower(textclean::replace_non_ascii(journal))
  ) %>% 
  # add crossref DOIs
  left_join(
    cr_doi_tbl, 
    by = c("title_ascii" = "title", "journal_ascii" = "cr_journal_ascii")
  ) %>% 
  # add BJC DOIs
  left_join(bjc_articles, by = "link") %>% 
  mutate(
    doi = ifelse(is.na(doi), cr_doi, doi),
    doi = ifelse(is.na(doi) & !is.na(bjc_doi), bjc_doi, doi)
  ) %>% 
  # remove rows with duplicate DOIs caused by some Taylor & Francis articles
  # being in the database twice
  group_by(doi) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  mutate(is_duplicate = row > 1) %>% 
  select(-row) %>% 
  filter(is_duplicate == FALSE)

# save articles with DOI
articles_with_cr %>% 
  select(title, author, journal, link, doi, date, timestamp, clicks) %>% 
  write_csv(here::here("output_data/articles_with_doi.csv.gz"))

