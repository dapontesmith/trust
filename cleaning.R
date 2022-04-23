library(estimatr)
library(modelsummary)
library(tidyverse)
#setwd("C:/Users/nod086/Downloads/")

setwd("C:/Users/dapon/Dropbox/Harvard/trust/data/survey")

# RUN CENSUS_IMPORT.R BEFORE RUNNING THIS FILE - 
# MERGE WITH CENSUS DATA REQURIES CENSUS_OUT OBJECT FROM CENSUS_IMPORT.R

# write function to read in data and clean the trust variables 
read_and_clean_trust <- function(filename){
  # clean up the generalized trust variables 
  filepath <- paste(filename, ".csv", sep = "")
  
  # handle different initial rows in 2020 and 2021 csvs 
  if (str_detect(filepath, "2021-recontact") == TRUE){ 
    df <- read_csv(filepath) 
  } else if(str_detect(filepath, "2021") == TRUE) {
    df <- read_csv(filepath) %>% slice(-(1:2))
  } else {
    df <- read_csv(filepath) %>% slice(-1)
  }
  
  # clean the general and neighbor trust variables 
  out <- df %>% 
    mutate(General_advantage = as.numeric(ifelse(
      General_advantage == "Most people would try to take advantage\n1", 1, 
      ifelse(General_advantage == "Most people would try to be fair\n10",
             10, General_advantage))
    ), General_helpful = 
      as.numeric(ifelse(General_helpful == "People mostly try to be helpful\n10", 10,
             ifelse(General_helpful == "People mostly look out for themselves\n1",
                    1, General_helpful))),
    General_trust = as.numeric(ifelse(
      General_trust == "You can't be too careful\n1\n", 1, 
      ifelse(General_trust == "Most people can be trusted\n10\n",
             10, General_trust))),
    Neighbor_advantage = as.numeric(ifelse(
      Neighbor_advantage == "Most people would try to take advantage\n1", 1, 
      ifelse(Neighbor_advantage == "Most people would try to be fair\n10",
             10, Neighbor_advantage))
    ), Neighbor_helpful = 
      as.numeric(ifelse(Neighbor_helpful == "People mostly try to be helpful\n10", 10,
             ifelse(Neighbor_helpful == "People mostly look out for themselves\n1",
                    1, Neighbor_helpful))),
    Neighbor_trust = as.numeric(ifelse(
      Neighbor_trust == "You can't be too careful\n1\n", 1, 
      ifelse(Neighbor_trust == "Most people can be trusted\n10\n",
             10, Neighbor_trust)))
    )

 # check to make sure everything is the same 
   if( sum(out$General_advantage == 10, na.rm = TRUE) != 
       sum(df$General_advantage == "Most people would try to be fair\n10", na.rm = TRUE)) {
     print("Cleaning error!")
   }
  
  # create column that is mean of the three general trust columns, as per denisen measure 
  out <- out %>% 
    mutate(denisen_general = 
             (General_advantage + General_helpful + General_trust)/3, 
  # aggregate neighobor trusts 
           denisen_neighbor = 
             (Neighbor_advantage + Neighbor_helpful + Neighbor_trust)/3)
  
  # clean Republican and Democrat trust variables 
  if ("Republican_trust" %in% names(out)){
    out <- out %>% 
      mutate(Republican_trust = as.numeric(ifelse(
        Republican_trust == "You can't be too careful\n1\n", 1, 
        ifelse(Republican_trust == "Most people can be trusted\n10\n",
               10, Republican_trust))),
        
        Democrat_trust = as.numeric(ifelse(
          Democrat_trust == "You can't be too careful\n1\n", 1, 
          ifelse(Democrat_trust == "Most people can be trusted\n10\n",
                 10, Democrat_trust)))
      )
  }
  return(out)
}

# df <- read_csv("2020-survey.csv") %>% slice(-1)

df2020 <- suppressWarnings(read_and_clean_trust("2020-survey"))
df2021_2 <- suppressWarnings(read_and_clean_trust("VoterFileSurvey2021_2"))
df2021_newsample2 <- suppressWarnings(read_and_clean_trust(
  "VoterFileSurvey2021_NewSample - Copy_April 11, 2022_16.06"
))
df2021_newsample1 <- suppressWarnings(read_and_clean_trust(
  "VoterFileSurvey2021_NewSample_April 11, 2022_14.05"
))
df2021 <- suppressWarnings(read_and_clean_trust(
  "2021-recontact-with-seg-variables"
)) 

# clean some demographic variables in recontact data 
df2021_clean <- df2021 %>% 
  # make indicator for college degree 
  mutate(college = case_when(
    education %in% c("Bachelors degree",
                     "Post-graduate or professional degree (MBA, MD, PhD, etc)") ~ 1, 
    education %in% c("Attended college but did not graduate",
                     "Associates degree",
                     "High school graduate",
                     'Did not graduate from high school') ~ 0, 
    TRUE ~ as.numeric(education)
  ), #make education ordinal 
  edlevel = case_when(
    education == "Did not graduate from high school" ~ 1, 
    education == "High school graduate" ~ 2, 
    education =="Attended college but did not graduate" ~ 3, 
    education == "Associates degree" ~ 4, 
    education == "Bachelors degree" ~ 5, 
    education == "Post-graduate or professional degree (MBA, MD, PhD, etc)" ~ 6, 
    TRUE ~ as.numeric(education)
  ), # make race variables 
  white_non_hispanic = case_when(
    race == "White or Caucasian" & hispanic == "No" ~ 1, 
    race != "White or Caucasian" & !is.na(race) ~ 0, 
    TRUE ~ as.numeric(race)
  ), # make income ordinal variable 
  income_ordinal = case_when(
    income == "$0-$4,999" ~ 0,
    income == "$5,000-$7,499" ~ 1,
    income == "$7,500~$9,999" ~ 2,
    income == "$10,000-$12.499" ~ 3,
    income == "$12,500-$14,999" ~ 4,
    income == "$15,000-$19,999" ~ 5,
    income == "$20,000-$24,999" ~ 6,
    income == "$25,000-$29,999" ~ 7,
    income == "$30,000-$34,999"~ 8,
    income == "$35,000-$39,999" ~ 9,
    income == "$40,000-$49,999"~10,
    income == "$50,000-$59,999" ~ 11,
    income == "$60,000-$74,999"~12,
    income == "$75,000-$99,999"~13,
    income == "$100,000-$149,999"~14,
    income == "$150,000 or more" ~15,
    TRUE ~ as.numeric(income)
  ), # make age varaible 
  age = 2021-year, 
  # clean block group code variables to merge with census - 
  # this requires finagling with fips codes a little
  census_id_char = as.character(vb.tsmart_census_id),
           
  # first 9 states will have one-character fips codes 
  census_id_add = case_when(
    nchar(census_id_char) == 14 ~ paste("0",census_id_char, sep = ""), 
    TRUE ~ census_id_char 
    ), 
  blockgroup_code = substr(census_id_add, 1, 12),
  democrat_id_twoparty = ifelse(
    party_3 == 'Democrat', 1, 
    ifelse(party_3 == "Republican", 0, NA)
  ), 
  party_3_factor = factor(party_3, 
                          levels=c("Republican","Independent","Democrat",
                                   "Other party (please specify)"))
  ) # this will be merging variable for ACS data 
  
# make party ID variablds  
 



# MERGE SURVEY DATASET WITH census data 
full <- df2021_clean %>% 
  # cut codes for merging with GEOID
  mutate(census_id_char = as.character(vb.tsmart_census_id),
         
         # first 9 states will have one-character fips codes 
         census_id_add = case_when(
           nchar(census_id_char) == 14 ~ paste("0",census_id_char, sep = ""), 
           TRUE ~ census_id_char 
         ),
         blockgroup_code = substr(census_id_add, 1, 12)) %>% 
  # perform the merge 
  left_join(., census_out, 
            by = c("blockgroup_code" = "GEOID"))

setwd("C:/Users/dapon/Dropbox/Harvard/trust/data/")
write.csv(full, "survey_clean/2021-recontact-with-seg-variables.csv")
           
      
