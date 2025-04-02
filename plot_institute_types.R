pacman::p_load(countrycode, dplyr, ggplot2, janitor, rio, tidyverse)

active_participants <- import("SummaryParticipation - Sheet1.csv") %>%
  rename(title = `Title (e.g. Mr, Ms, Dr)`) %>%
  rename(institution = `Current institution`) %>%
  mutate(institution_type = case_when(
    grepl("university", institution, ignore.case = TRUE) ~ "academic",
    grepl("universitas", institution, ignore.case = TRUE) ~ "academic",
    grepl("Institut Teknologi Bandung", institution) ~ "academic",
    grepl("Universidade", institution) ~ "academic",
    grepl("School of Tropical Medicine", institution) ~ "academic",
    grepl("LaPathER", institution) ~ "academic",
    grepl("Institute for Lung Diseases and Tuberculosis", institution) ~ "academic",
    grepl("London School", institution) ~ "academic",
    grepl("Wits Health Consortium", institution) ~ "academic",
    grepl("UKHD", institution) ~ "academic",
    grepl("Biomedical Research and Training Institute", institution) ~ "academic",
    grepl("NRL", institution) ~ "public health",
    grepl("Reference Laboratory", institution, ignore.case = TRUE) ~ "public health",
    grepl("public health", institution, ignore.case = TRUE) ~ "public health",
    grepl("Instituto Nacional de Saúde", institution) ~ "public health",
    grepl("Centre for Disease Control", institution, ignore.case = TRUE) ~ "public health",
    grepl("Rwanda Biomedical Centre", institution, ignore.case = TRUE) ~ "public health",
    grepl("Institute for Communicable Disease", institution) ~ "public health",
    grepl("NTRL", institution) ~ "public health",
    grepl("ISP de Chile", institution) ~ "public health",
    grepl("INEI", institution) ~ "public health",
    grepl("National tuberculosis Program", institution) ~ "public health",  # is this Ministry ?
    grepl("National TB Reference", institution) ~ "public health",
    grepl("Laboratorium Prof Sri Oemijati", institution) ~ "public health",
    grepl("Instituto Adolfo Lutz", institution) ~ "public health",
    grepl("NCDC", institution) ~ "public health",
    grepl("Instituto de Salud Pública", institution) ~ "public health",
    grepl("SRL Milan Network", institution) ~ "public health",  # Instituto de Diagnostico y Referencia Epidemiologicos
    grepl("Research Institute", institution, ignore.case = TRUE) ~ "research institute",
    grepl("MRC", institution) ~ "research institute",
    grepl("Institut Pasteur", institution) ~ "research institute",
    grepl("Razi Institute", institution) ~ "research institute",
    grepl("Ifakara health institute", institution, ignore.case = TRUE) ~ "research institute",
    grepl("Institute of tropical Medicine", institution, ignore.case = TRUE) ~ "research institute",
    grepl("Istituto Zooprofilattico Sperimentale della Lombardia e dell'Emilia Romagna", institution) ~ "research institute",
    grepl("Botswana Harvard Partnership", institution) ~ "research institute",
    grepl("Institute for Medical Research", institution) ~ "research institute",
    grepl("National Institute of Health", institution) ~ "research institute",
    grepl("Ccmb", institution) ~ "research institute",
    grepl("Institute of Medical Research", institution, ignore.case = TRUE) ~ "research institute",
    grepl("CERMEL", institution) ~ "research institute",
    grepl("Higher Institute for Scientific and Medical Research", institution) ~ "research institute",
    grepl("Guangzhou Institute of Biomedicine", institution) ~ "research institute",
    grepl("NIMR", institution) ~ "research institute",
    grepl("CISM", institution) ~ "research institute",
    grepl("International Center for Diarrhoeal Disease Research", institution) ~ "research institute",
    grepl("National Institute of  Health", institution) ~ "research institute",
    grepl("Guangzhou Institutes of Biomedicine and Health, Chinese Academy of Sciences, Guangzhou, China", institution) ~ "research institute",
    grepl("Centre Pasteur du Cameroun", institution) ~ "research institute",  # this serves both research and public health functions
    grepl("Moh", institution) ~ "health ministry",
    grepl("NTP", institution) ~ "health ministry",  # National TB Programme - is it Minister or Public Health institute?
    grepl("National TB Control Program", institution) ~ "health ministry",
    grepl("Hamad Medical Corporation", institution) ~ "hospital",
    grepl("medical centre", institution, ignore.case = TRUE) ~ "hospital",
    grepl("Grande Ospedale Metropolitano Niguarda", institution) ~ "hospital",
    grepl("PATH", institution) ~ "nonprofit",
    grepl("MDH", institution) ~ "nonprofit",
    grepl("WHO", institution) ~ "WHO",
    grepl("Elitechgroup S.p.A", institution) ~ "company",
    TRUE ~ "unknown"
  )) %>%
  mutate(Country = replace(Country, Country == "Mocambique", "Mozambique")) %>%
  mutate(Country = replace(Country, Country == "US", "USA")) %>%
  mutate(Country = replace(Country, Country == "Philipines", "Philippines")) %>%
  mutate(Country = replace(Country, Country == "?", NA)) %>%
  mutate(Country = replace(Country, Country == "Cameroun", "Cameroon")) %>%
  mutate(continent = countrycode(sourcevar = Country,
                                 origin = "country.name", 
                                 destination = "continent",
                                 custom_match = c("Kosovo" = "Europe"))) %>%
  mutate(continent = replace(continent, Country %in% c("Argentina", "Chile", "Brazil"), "South America")) %>%
  mutate(continent = replace(continent, Country %in% c("Mexico", "USA", "Canada"), "North America")) %>%
  arrange(factor(Date, levels = c("March2022", 
                                  "January2023",
                                  "October 2023",
                                  "June2024")))

unknown <- active_participants %>% filter(institution_type == "unknown") %>% select(institution, Country)

by_date <- active_participants %>% group_by(Date, institution_type) %>% count() %>%
  arrange(factor(Date, levels = c("March2022", 
                                  "January2023",
                                  "October 2023",
                                  "June2024")))

by_country = active_participants %>% group_by(Date, Country) %>% count() %>%
  arrange(factor(Date, levels = c("March2022", 
                                  "January2023",
                                  "October 2023",
                                  "June2024")))

by_continent = active_participants %>% group_by(Date, continent) %>% count() %>%
  arrange(factor(Date, levels = c("March2022", 
                                  "January2023",
                                  "October 2023",
                                  "June2024")))

ggplot(by_date, aes(x = fct_inorder(Date), y = n, fill=institution_type)) +
  geom_bar(stat = "identity") + 
  labs(x = "Course", y = "Count",
       fill = "Institution Type",
       title = "Active Participants by Institution Type")
  
ggplot(by_country, aes(x = fct_inorder(Date), y = n, fill=Country)) +
  geom_bar(stat = "identity") + 
  labs(x = "Course", y = "Count",
       fill = "Country",
       title = "Active Participants by Country")

ggplot(by_continent, aes(x = fct_inorder(Date), y = n, fill=continent)) +
  geom_bar(stat = "identity") + 
  labs(x = "Course", y = "Count",
       fill = "Continent",
       title = "Active Participants by Continent")
