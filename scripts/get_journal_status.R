# load packages
library("rromeo")
library("tidyverse")

# load data as one line per journal
journals <- here::here("output_data/articles_with_oa.rds") %>% 
  read_rds() %>% 
  mutate(journal_name = recode(
    journal_name,
    "British Journal Of Criminology" = "British Journal of Criminology",
    "The British Journal of Criminology" = "British Journal of Criminology",
    "Crime, Media, Culture: An International Journal" = "Crime, Media, Culture",
    "Criminal Behaviour and Mental Health" = 
      "Criminal Behavior and Mental Health",
    "Policing: A Journal of Policy and Practice" = "Policing",
    "Policing: A Journal Of Policy And Practice" = "Policing",
    "Sexual Abuse: A Journal of Research and Treatment" = "Sexual Abuse",
    "The Police Journal: Theory, Practice and Principles" = "The Police Journal"
  )) %>% 
  group_by(journal_name) %>% 
  summarise(journal_issn_l = first(journal_issn_l), articles = n())

# rr_journal_issn() throws an error if an ISSN doesn't match an entry in the
# ROMEO database, so we use safely() to catch any errors
safe_rr_journal_issn <- safely(rr_journal_issn)
safe_rr_journal_name <- safely(rr_journal_name)

# get journal details based on ISSN
journals_issn <- journals %>% 
  mutate(
    romeo = map(journal_issn_l, 
                ~ pluck(safe_rr_journal_issn(., key = "M2BGs3pL72c"), "result"))
  ) %>% 
  unnest("romeo") %>% 
  filter(!is.na(romeocolour))

# some journals may not be matched by ISBN, so search ROMEO for these by name
journals_name <- journals %>% 
  filter(!journal_name %in% journals_issn$journal_name) %>% 
  mutate(
    romeo = map(journal_name, ~ pluck(
      safe_rr_journal_name(., key = "M2BGs3pL72c"), "result")
    )
  ) %>% 
  unnest("romeo")

# add journals manually if not present in RoMEO
# except where noted, these are sourced from 
# https://authorservices.taylorandfrancis.com/publishing-open-access/oa-options-finder/
journals_manual <- tribble(
  ~journal_name, ~man_preprint, ~man_postprint, ~man_pdf,
  "Cambridge Journal of Evidence-Based Policing", "can", "can", "cannot", # https://www.springer.com/gp/open-access/publication-policies/self-archiving-policy
  "Corrections", "can", "can", "cannot",
  "Crime Psychology Review", "can", "can", "cannot",
  "Current Issues in Criminal Justice", "can", "can", "cannot",
  "Dignity: A Journal on Sexual Exploitation and Violence", "can", "can", "can",
  "The Howard Journal of Crime and Justice", "can", "restricted", "cannot", # https://authorservices.wiley.com/author-resources/Journal-Authors/open-access/author-compliance-tool.html
  "Justice Evaluation Journal", "can", "can", "cannot",
  "Justice Research and Policy", "can", "can", "cannot", # https://uk.sagepub.com/en-gb/eur/journal-author-archiving-policies-and-re-use
  "Journal of Human Trafficking", "can", "can", "cannot"
)

# combine data into complete information for each journal
journals_both <- journals %>% 
  select(journal_name, articles) %>% 
  left_join(select(journals_issn, journal_name, preprint, postprint, pdf), by = "journal_name") %>% 
  left_join(select(journals_name, journal_name, preprint, postprint, pdf), by = "journal_name") %>% 
  left_join(journals_manual, by = "journal_name") %>% 
  mutate(
    # prefer data dervied from ISSN to data dervied from the journal name
    preprint = ifelse(is.na(preprint.x), preprint.y, preprint.x),
    postprint = ifelse(is.na(postprint.x), postprint.y, postprint.x),
    pdf = ifelse(is.na(pdf.x), pdf.y, pdf.x),
    # prefer data derived from RoMEO to manual data
    preprint = ifelse(is.na(preprint), man_preprint, preprint),
    postprint = ifelse(is.na(postprint), man_postprint, postprint),
    pdf = ifelse(is.na(pdf), man_pdf, pdf)
  ) %>% 
  select(journal_name, articles, preprint, postprint, pdf)
  
# get currency exchange rates as of 01 Jan 2020
aud_to_usd <- pluck(
  tidyquant::tq_get("AUD/USD", get = "exchange.rates", from = "2020-01-01"), 
  "exchange.rate", 
  1
)
eur_to_usd <- pluck(
  tidyquant::tq_get("EUR/USD", get = "exchange.rates", from = "2020-01-01"), 
  "exchange.rate", 
  1
)
gbp_to_usd <- pluck(
  tidyquant::tq_get("GBP/USD", get = "exchange.rates", from = "2020-01-01"), 
  "exchange.rate", 
  1
)

# add price for an annual online journal subscription, in USD
# journal abbreviations from https://www.ncbi.nlm.nih.gov/nlmcatalog/
journal_prices <- tribble(
  ~journal_name, ~abbreviation, ~price, ~price_source, ~price_note, ~society_price, ~institutional_price,
  "Aggression and Violent Behavior", "Aggress Violent Behav", 176, "https://www.elsevier.com/journals/personal/aggression-and-violent-behavior/1359-1789", "", FALSE, FALSE,
  "American Journal of Criminal Justice", "Am J Crim Justice", 99, "https://www.springer.com/journal/12103", "", FALSE, FALSE,
  "Annual Review of Criminology", "Annu Rev Criminol", 112, "https://www.annualreviews.org/action/ecommerce", "", FALSE, FALSE,
  "Asian Journal of Criminology", "Asian J Criminol", 99, "https://link.springer.com/journal/11417", "", FALSE, FALSE,
  "Australian & New Zealand Journal of Criminology", "Aust N Z J Criminol", 155 * aud_to_usd, "https://anzsoc.org/membership/", "via ANZSOC membershp", FALSE, FALSE,
  "Behavioral Sciences of Terrorism and Political Aggression", "Behav Sci Terror Polit Aggress", 71 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rirt20", "print", FALSE, FALSE,
  "British Journal of Criminology", "Br J Criminol", 88.5 * gbp_to_usd / 2, "https://www.britsoccrim.org/membership/", "combined subscription via BSC membership", TRUE, FALSE,
  "Cambridge Journal of Evidence-Based Policing", "Cambridge J Evid Based Policing", NA, "", "hybrid journal but all articles gold OA", FALSE, FALSE,
  "Child Abuse & Neglect", "Child Abuse Negl", 389, "https://www.elsevier.com/journals/personal/child-abuse-and-neglect/0145-2134", "", FALSE, FALSE,
  "Contemporary Justice Review", "Contemp Justice Rev", 173 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/gcjr20", "print", FALSE, FALSE,
  "Corrections", "Corrections", 77 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/ucor20", "print", FALSE, FALSE,
  "Crime & Delinquency", "Crime Delinq", 231, "https://us.sagepub.com/en-us/nam/crime-delinquency/journal200959#subscribe", "", FALSE, FALSE,
  "Crime and Justice", "Crime Justice", 68, "https://www.press.uchicago.edu/ucp/journals/subscribe/cj.html", "", FALSE, FALSE,
  "Crime Prevention and Community Safety", "Crime Prev Community Saf", 209, "https://link.springer.com/journal/41300", "", FALSE, FALSE,
  "Crime Psychology Review", "Crime Psychol Rev", NA, "", "journal has ceased publishing", FALSE, FALSE,
  "Crime Science", "Crime Sci", 0, "", "gold OA journal", FALSE, FALSE,
  "Crime, Law and Social Change", "Crime Law Soc Change", 99, "https://www.springer.com/journal/10611", "", FALSE, FALSE,
  "Crime, Media, Culture", "Crime Media Cult", 104, "https://us.sagepub.com/en-us/nam/journal/crime-media-culture#subscribe", "print", FALSE, FALSE,
  "Criminal Behaviour and Mental Health", "Crim Behav Ment Health", 1037, "https://ordering.onlinelibrary.wiley.com/subs.asp?ref=1471-2857", "institutional", FALSE, TRUE,
  "Criminal Justice and Behavior", "Crim Justice Behav", 75, "https://us.sagepub.com/en-us/nam/journal/criminal-justice-and-behavior#subscribe", "only available via society membership", FALSE, FALSE,
  "Criminal Justice Ethics", "Crim Justice Ethics", 40 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rcre20", "", FALSE, FALSE,
  "Criminal Justice Matters", "Crim Justice Matters", 0, "", "free", FALSE, FALSE,
  "Criminal Justice Policy Review", "Crim Justice Policy Rev", 147, "https://us.sagepub.com/en-us/nam/criminal-justice-policy-review/journal201357#subscribe", "", FALSE, FALSE,
  "Criminal Justice Review", "Crim Justice Rev", 50, "https://us.sagepub.com/en-us/nam/criminal-justice-review/journal201726#subscribe", "", FALSE, FALSE,
  "Criminal Justice Studies", "Crim Justice Stud", 151 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/gjup20", "print", FALSE, FALSE,
  "Criminology & Criminal Justice", "Criminol Crim Justice", 88.5 * gbp_to_usd / 2, "https://www.britsoccrim.org/membership/", "combined subscription via BSC membership", TRUE, FALSE,
  "Criminology & Public Policy", "Criminol Public Policy", 105 / 2, "https://ordering.onlinelibrary.wiley.com/subs.asp?ref=1745-9133", "combined subscription via ASC membership", TRUE, FALSE,
  "Criminology", "Criminology", 105 / 2, "", "combined subscription via ASC membership", TRUE, FALSE,
  "Critical Criminology", "Crit Criminol", 99, "https://link.springer.com/journal/10612", "", FALSE, FALSE,
  "Critical Studies on Terrorism", "Crit Stud Terror", 383 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rter20", "institutional", FALSE, TRUE,
  "Current Issues in Criminal Justice", "Curr Issues Crim Justice", 125 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rcic20", "", FALSE, FALSE,
  "Deviant Behavior", "Deviant Behav", 125 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rcic20", "", FALSE, FALSE,
  "Dignity: A Journal on Sexual Exploitation and Violence", "Dignity", 0, "", "gold OA journal", FALSE, FALSE,
  "Dynamics of Asymmetric Conflict", "Dyn Asymmetric Confl", 63 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rdac20", "", FALSE, FALSE,
  "European Journal for Security Research", "Eur J Secur Res", NA, "", "no individual subscriptions available", FALSE, FALSE,
  "European Journal of Crime, Criminal Law and Criminal Justice", "Eur J Crime Crim Law Crim Justice", 213, "https://brill.com/view/journals/eccl/eccl-overview.xml", "", FALSE, FALSE,
  "European Journal of Criminology", "Eur J Criminol", 75 * eur_to_usd, "https://www.esc-eurocrim.org/index.php/membership", "via ESC membership", FALSE, FALSE,
  "European Journal of Probation", "Eur J Probat", 67, "https://us.sagepub.com/en-us/nam/journal/european-journal-probation#subscribe", "", FALSE, FALSE,
  "European Journal on Criminal Policy and Research", "Eur J Crim Pol Res", 99, "https://www.springer.com/journal/10610", "", FALSE, FALSE,
  "Feminist Criminology", "Fem Criminol", 186, "https://us.sagepub.com/en-us/nam/journal/feminist-criminology#subscribe", "", FALSE, FALSE,
  "Global Crime", "Glob Crime", 97 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/fglc20", "", FALSE, FALSE,
  "Health & Justice", "Health Justice", 0, "", "gold OA journal", FALSE, FALSE,
  "Homicide Studies", "Homicide Stud", 173, "https://us.sagepub.com/en-us/nam/journal/homicide-studies#subscribe", "", FALSE, FALSE,
  "International Criminal Justice Review", "Int Crim Justice Rev", 50, "https://us.sagepub.com/en-us/nam/international-criminal-justice-review/journal201727#subscribe", "", FALSE, FALSE,
  "International Journal of Comparative and Applied Criminal Justice", "Int J Comp Appl Crim Justice", 63 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rcac20", "", FALSE, FALSE,
  "International Journal of Law, Crime and Justice", "Int J Law Crime Justice", 127, "https://www.elsevier.com/journals/personal/international-journal-of-law-crime-and-justice/1756-0616", "", FALSE, FALSE,
  "International Journal of Offender Therapy and Comparative Criminology", "Int J Offender Ther Comp Criminol", 151, "https://us.sagepub.com/en-us/nam/international-journal-of-offender-therapy-and-comparative-criminology/journal200930#subscribe", "", FALSE, FALSE,
  "International Journal of Police Science & Management", "Int J Police Sci Manag", 133, "https://us.sagepub.com/en-us/nam/journal/international-journal-police-science-management#subscribe", "", FALSE, FALSE,
  "International Journal of Speech Language and the Law", "Int J Speech Lang Law", 105, "https://journals.equinoxpub.com/IJSLL/about/subscriptions", "", FALSE, FALSE,
  "International Review of Victimology", "Int Rev Vict", 174, "https://us.sagepub.com/en-us/nam/journal/international-review-victimology#subscribe", "", FALSE, FALSE,
  "Journal of Aggression, Maltreatment & Trauma", "J Aggress Maltreat Trauma", 313 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/wamt20", "", FALSE, FALSE,
  "Journal of Applied Security Research", "J Appl Secur Res", 114 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/wasr20", "", FALSE, FALSE,
  "Journal of Child Sexual Abuse", "J Child Sex Abus", 173 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/wcsa20", "", FALSE, FALSE,
  "Journal of Contemporary Criminal Justice", "J Contemp Crim Justice", 126, "https://us.sagepub.com/en-us/nam/journal-of-contemporary-criminal-justice/journal200831#subscribe", "", FALSE, FALSE,
  "Journal of Crime and Justice", "J Crime Justice", 340 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rjcj20", "", FALSE, TRUE,
  "Journal of Criminal Justice", "J Crim Justice", 317, "https://www.elsevier.com/journals/personal/journal-of-criminal-justice/0047-2352", "", FALSE, FALSE,
  "Journal of Criminal Justice Education", "J Crim Justice Educ", 85 / 3, "https://www.acjs.org/page/MembershipInfo", "via ACJS membership", TRUE, FALSE,
  "Journal of Developmental and Life-Course Criminology", "J Dev Life Course Criminol", 99, "https://www.springer.com/journal/40865", "", FALSE, FALSE,
  "Journal of Ethnicity in Criminal Justice", "J Ethn Crim Justice", 176 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/wecj20", "", FALSE, FALSE,
  "Journal of Experimental Criminology", "J Exp Criminol", 99, "https://www.springer.com/journal/11292", "", FALSE, FALSE,
  "Journal of Family Violence", "J Fam Violence", 99, "https://www.springer.com/journal/10896", "", FALSE, FALSE,
  "Journal of Gender-Based Violence", "J Gend Based Viol", 156, "https://bristoluniversitypress.co.uk/asset/7832/2020-usd-journal-price-list.pdf", "", FALSE, FALSE,
  "Journal of Human Trafficking", "J Hum Traffick", 60 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/uhmt20", "", FALSE, FALSE,
  "Journal of Interpersonal Violence", "J Interpers Violence", 391, "https://us.sagepub.com/en-us/nam/journal/journal-interpersonal-violence#subscribe", "", FALSE, FALSE,
  "Journal of Investigative Psychology and Offender Profiling", "J Investig Psychol Offender Profiling", 110, "https://ordering.onlinelibrary.wiley.com/subs.asp?ref=1544-4767", "", FALSE, FALSE,
  "Journal of Offender Rehabilitation", "J Offender Rehabil", 294 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/wjor20", "", FALSE, FALSE,
  "Journal of Police and Criminal Psychology", "J Police Crim Psychol", 99, "https://www.springer.com/journal/11896", "", FALSE, FALSE,
  "Journal of Policing, Intelligence and Counter Terrorism", "J Policing Intell Counter Terror", 95 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/rpic20", "", FALSE, FALSE,
  "Journal of Quantitative Criminology", "J Quant Criminol", 99, "https://www.springer.com/journal/10940", "", FALSE, FALSE,
  "Journal of Research in Crime and Delinquency", "J Res Crime Delinq", 186, "https://us.sagepub.com/en-us/nam/journal-of-research-in-crime-and-delinquency/journal200893#subscribe", "", FALSE, FALSE,
  "Journal of Scandinavian Studies in Criminology and Crime Prevention", "J Scand Stud Criminol Crime Prev", 59 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/scri21", "", FALSE, FALSE,
  "Journal of School Violence", "J Sch Violence", 167 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/wjsv20", "", FALSE, FALSE,
  "Journal of Sexual Aggression", "J Sex Aggress", 272 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/tjsa20", "", FALSE, FALSE,
  "Justice Evaluation Journal", "Justice Eval J", 85 / 3, "Justice Eval J", "via ACJS membership", TRUE, FALSE,
  "Justice Quarterly", "Justice Q", 85 / 3, "Justice Q", "via ACJS membership", TRUE, FALSE,
  "Justice Research and Policy", "Justice Res Policy", 49, "https://uk.sagepub.com/en-gb/eur/justice-research-and-policy/journal202417#subscribe", "", FALSE, FALSE,
  "Justice System Journal", "Justice Syst J", 38 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/ujsj20", "", FALSE, FALSE,
  "Legal and Criminological Psychology", "Legal Criminol Psychol", 106, "https://ordering.onlinelibrary.wiley.com/subs.asp?ref=2044-8333", "", FALSE, FALSE,
  "Police Practice and Research", "Police Pract Res", 156 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/gppr20", "", FALSE, FALSE,
  "Police Quarterly", "Police Q", 84, "https://uk.sagepub.com/en-gb/eur/journal/police-quarterly#subscribe", "", FALSE, FALSE,
  "Policing", "Policing", 372, "https://academic.oup.com/policing/subscribe", "", FALSE, FALSE,
  "Policing and Society", "Policing Soc", 548 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/gpas20", "", FALSE, FALSE,
  "Psychiatry, Psychology and Law", "Psychiatr Psychol Law", 287 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/tppl20", "", FALSE, FALSE,
  "Psychology, Crime & Law", "Psychol Crime Law", 421 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/gpcl20", "", FALSE, FALSE,
  "Psychology of Violence", "Psychol Violence", 196, "https://www.apa.org/pubs/journals/vio/pricing", "", FALSE, FALSE,
  "Punishment & Society", "Punishm Soc", 73, "https://uk.sagepub.com/en-gb/eur/punishment-society/journal200845#subscribe", "", FALSE, FALSE,
  "Race and Justice", "Race Justice", 87, "https://uk.sagepub.com/en-gb/eur/journal/race-and-justice#subscribe", "", FALSE, FALSE,
  "Security Journal", "Secur J", 199, "https://www.palgrave.com/us/journal/41284", "", FALSE, FALSE,
  "Sexual Abuse", "Sex Abuse", 181, "https://us.sagepub.com/en-us/nam/sexual-abuse/journal201888#subscribe", "", FALSE, FALSE,
  "Social & Legal Studies", "Soc Leg Stud", 133, "https://us.sagepub.com/en-us/nam/journal/social-legal-studies#subscribe", "", FALSE, FALSE,
  "Studies in Conflict & Terrorism", "Stud Confl Terror", 458 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/uter20", "", FALSE, FALSE,
  "Terrorism and Political Violence", "Terror Polit Viol", 273 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/ftpv20", "", FALSE, FALSE,
  "The Howard Journal of Crime and Justice", "Howard J Crime Justice" , 231, "https://ordering.onlinelibrary.wiley.com/subs.asp?ref=2059-1101", "", FALSE, FALSE,
  "The Police Journal", "Police J", 130, "https://us.sagepub.com/en-us/nam/the-police-journal/journal202314#subscribe", "", FALSE, FALSE,
  "The Prison Journal", "Prison J", 149, "https://us.sagepub.com/en-us/nam/journal/prison-journal#subscribe", "", FALSE, FALSE,
  "Theoretical Criminology", "Theor Criminol", 122, "https://us.sagepub.com/en-us/nam/journal/theoretical-criminology#subscribe", "", FALSE, FALSE,
  "Trauma, Violence, & Abuse", "Trauma Violence Abuse", 153, "https://us.sagepub.com/en-us/nam/journal/trauma-violence-abuse#subscribe", "", FALSE, FALSE,
  "Trends in Organized Crime", "Trends Organ Crime", 99, "https://www.springer.com/journal/12117", "", FALSE, FALSE,
  "Victims & Offenders", "Vict Offender", 214 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/uvao20", "", FALSE, FALSE,
  "Violence Against Women", "Violence Against Women", 369, "https://us.sagepub.com/en-us/nam/journal/violence-against-women#subscribe", "", FALSE, FALSE,
  "Violence and Victims", "Violence Vict", 75, "https://www.springerpub.com/violence-and-victims.html", "", FALSE, FALSE,
  "Women & Criminal Justice", "Women Crim Justice", 165 * gbp_to_usd, "https://www.tandfonline.com/pricing/journal/wwcj20", "", FALSE, FALSE,
  "Youth Violence and Juvenile Justice", "Youth Violence Juv Justice", 145, "https://us.sagepub.com/en-us/nam/journal/youth-violence-and-juvenile-justice#subscribe", "",  FALSE, FALSE
)

# combine prices with other data and save
journals_final <- journals_both %>% 
  left_join(journal_prices, by = "journal_name") %>% 
  arrange(journal_name) %>% 
  write_csv(here::here("output_data/journals_final.csv"))
