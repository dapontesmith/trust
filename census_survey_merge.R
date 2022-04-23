# RUN CENSUS_IMPORT.R AND CLEANING.R BEFORE RUNNING THIS FILE 
library(tidyverse)
library(estimatr)
library(modelsummary)


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


  
# get only trump and biden voters
df_twoparty <- full %>% 
    filter(pres_vote_2020 %in% c("Donald Trump","Joe Biden")) %>% 
    mutate(vote_biden = ifelse(pres_vote_2020 == "Joe Biden", 1, 0))
  
  # democratic exposure, general trust, pres vote choice 
mod_general_dem_pres_county <- lm_robust(data = df_twoparty, 
                                          denisen_general ~ w.mean.d.post*vote_biden + 
                                            gender + edlevel + white_non_hispanic + age + income_ordinal + 
                                          non_white_prop + poverty_rate + median_income,
                                          fixed_effects = vb.vf_county_code,
                                         clusters = vb.vf_county_code)
  # democratic exposure, neighbors trust, pres vote choice 
mod_neighbors_dem_pres_county <- lm_robust(
    data = df_twoparty, denisen_neighbor ~ 
      w.mean.d.post*vote_biden + 
      gender + edlevel + white_non_hispanic + age + income_ordinal + 
      non_white_prop + poverty_rate + median_income, 
    fixed_effects = vb.vf_county_code,
    clusters = vb.vf_county_code)

# democratic exposure, general trust, two-party id 
mod_general_dem_partyid_county <- lm_robust(
  data = full, 
  denisen_general ~ w.mean.d.post*democrat_id_twoparty + 
    gender + edlevel + white_non_hispanic + age + income_ordinal + 
    non_white_prop + poverty_rate + median_income, 
  fixed_effects = vb.vf_county_code, 
  clusters = vb.vf_county_code
)
# democratic exposure, neighbors trust, two-party id 
mod_neighbors_dem_partyid_county <- lm_robust(
  data = full, 
  denisen_neighbor ~ w.mean.d.post*democrat_id_twoparty + 
    gender + edlevel + white_non_hispanic + age + income_ordinal + 
    non_white_prop + poverty_rate + median_income, 
  fixed_effects = vb.vf_county_code, 
  clusters = vb.vf_county_code
)

# democratic exposure, general trust, party id (with independents)
mod_general_dem_threepartyid_county <- lm_robust(
  data = full %>% filter(party_3 %in% c("Democrat","Republican","Independent")), 
  denisen_general ~ w.mean.d.post*party_3_factor + 
    gender + edlevel + white_non_hispanic + age + income_ordinal + 
    non_white_prop + poverty_rate + median_income, 
  fixed_effects = vb.vf_county_code, 
  clusters = vb.vf_county_code
)
# democratic exposure, neighbors trust, party id (with independents)
mod_neighbors_dem_threepartyid_county <- lm_robust( 
  data = full %>% filter(party_3 %in% c("Democrat","Republican","Independent")), 
  denisen_neighbor ~ w.mean.d.post*party_3_factor + 
    gender + edlevel + white_non_hispanic + age + income_ordinal + 
    non_white_prop + poverty_rate + median_income, 
  fixed_effects = vb.vf_county_code, 
  clusters = vb.vf_county_code
)
  

# report the pres-vote results 
modelsummary(
    list("General trust" = mod_general_dem_pres_county, 
         "Neighbors trust" = mod_neighbors_dem_pres_county),
    stars = TRUE,
    coef_map = c(
      "w.mean.d.post" = "Democratic exposure (weighted)", 
      "vote_biden" = "Voted Biden",
      "w.mean.d.post:vote_biden" = "Dem exposure * Voted Biden",
      "genderMale" = "Gender Male",
      "genderOther/Prefer not to answer" = "Gender Other",
      "edlevel" = "Education (ordinal)",
      "white_non_hispanic" = "White (non-Hispanic)",
      "age" = "Age",
      "income_ordinal" = "Income (ordinal)",
      "non_white_prop" = "Pct. non-white (blockgroup)",
      "poverty_rate" = "Poverty rate (blockgroup)",
      "median_income" = "Median income (blockgroup)"
      
    ),
    add_rows = bind_rows(bind_cols("County fixed effects","Yes","Yes"),
                      bind_cols("Std. Error Clusters","County","County")),
    title = "General/neighbors trust, by presidential vote and partisan exposure",
  )

# report the party id  results 
modelsummary(
  list("General trust" = mod_general_dem_threepartyid_county, 
       "Neighbors trust" = mod_neighbors_dem_threepartyid_county),
  stars = TRUE,
  coef_map = c("w.mean.d.post" = "Democratic exposure (weighted)",
               "party_3_factor_Independent" = "Party ID = Independent", 
               "party_3_factorDemocrat" = "Party ID = Democrat",
               "w.mean.d.post:party_3_factorDemocrat" = "Dem exposure * Party ID = Dem", 
               "w.mean.d/post:party_3_factorIndependent" = "Dem exposure * Party ID = Ind", 
               "genderMale" = "Gender Male",
               "genderOther/Prefer not to answer" = "Gender Other",
               "edlevel" = "Education (ordinal)",
               "white_non_hispanic" = "White (non-Hispanic)",
               "age" = "Age",
               "income_ordinal" = "Income (ordinal)",
               "non_white_prop" = "Pct. non-white (blockgroup)",
               "poverty_rate" = "Poverty rate (blockgroup)",
               "median_income" = "Median income (blockgroup)"),
  add_rows = bind_rows(bind_cols("County fixed effects","Yes","Yes"),
                       bind_cols("Std. Error Clusters","County","County")),
  title = "General/neighbors trust, by three-party ID and partisan exposure",
)



# report three-par

  