# This script measures the attrition of articles through the process of getting
# the DOI and then the OA status

# load packages
library("tidyverse")
library("tidylog", warn.conflicts = FALSE)

# load data
articles_original <- here::here("original_data/articles.csv.gz") %>% 
  read_csv() %>% 
  filter(
    # exclude sources that aren't journals
    !name %in% c(
      "European Law Enforcement Research Bulletin", 
      "Research and Analysis publications"
    )
  )
articles_active <- articles_original %>% 
  filter(
    # exclude sources that don't assign DOIs to articles
    !name %in% c(
      "Journal of Criminal Law and Criminology", 
      "Journal of Criminological Research, Policy and Practice",
      "Perspectives on Terrorism", 
      "Science and Justice"
    )
  )
articles_doi <- read_csv(here::here("output_data/articles_with_doi.csv.gz"))
articles_final <- read_rds(here::here("output_data/articles_with_oa.rds"))

# export counts of attrition through the data wrangling process
attrition_table <- tribble(
  ~status, ~count,
  "all articles", nrow(articles_original),
  "articles in journals that don't use DOIs", nrow(articles_original) - nrow(articles_active),
  "articles for which a DOI could not be identified", nrow(articles_active) - sum(!is.na(articles_doi$doi)),
  # "articles with duplicate DOIs", nrow(duplicate_dois) + sum(articles_doi$is_duplicate),
  "articles for which OA status could not be determined", sum(!is.na(articles_doi$doi)) - nrow(articles_final),
  "articles with OA status", sum(!is.na(articles_final$oa_status))
) %>% 
  mutate(
    perc = count / first(count),
    remain = first(count) * 2 - cumsum(count)
  ) %>% 
  write_csv(here::here("output_data/attrition_counts.csv"))
  

article_counts <- bind_rows(
  "active" = rename(count(articles_active, name), journal = name),
  "with_doi" = count(articles_doi, journal),
  "with_oa" = rename(count(filter(articles_final, !is.na(oa_status)), journal_name), journal = journal_name),
  .id = "type"
) %>% 
  mutate(
    journal = recode(
      str_remove(str_replace_all(journal, " & ", " and "), "^The "),
      "Corrections" = "Corrections: Policy, Practice and Research",
      "Crime, Media, Culture" = 
        "Crime, Media, Culture: An International Journal",
      "Criminal Justice and Behaviour" = "Criminal Justice and Behavior",
      "Dynamics of Asymmetric Conflict" = 
        "Dynamics of Asymmetric Conflict: Pathways toward terrorism and genocide",
      "Howard Journal of Criminal Justice" = 
        "Howard Journal of Crime and Justice",
      "Police Journal" = "Police Journal: Theory, Practice and Principles",
      "Policing: A Journal of Policy and Practice" = 
        "Policing: a Journal of Policy and Practice",
      "Policing: A Journal of Policy and Practice" = 
        "Policing: a Journal of Policy and Practice",
      "Policing: A Journal Of Policy And Practice" = 
        "Policing: a Journal of Policy and Practice",
      "Policing" = "Policing: a Journal of Policy and Practice",
      "Sexual Abuse" = "Sexual Abuse: A Journal of Research and Treatment",
      "Sexual Abuse: Journal of Research and Treatment" = 
        "Sexual Abuse: A Journal of Research and Treatment",
      "Trauma, Violence, and Abuse" = "Trauma, Violence and Abuse"
    )
  ) %>% 
  group_by(type, journal) %>% 
  summarise_if(is.numeric, sum) %>% 
  pivot_wider(names_from = "type", values_from = "n") %>% 
  replace_na(list("active" = 0, "with_doi" = 0, "with_oa" = 0)) %>% 
  mutate(
    missing_doi = active - with_doi,
    missing_oa = with_doi - with_oa,
    perc_missing = round((missing_doi + missing_oa) / active, 2)
  ) %>% 
  arrange(journal)
