library(tidyverse)
library(tidycensus)
library(easycensus)
census_api_key("02407c38f2abdabb3c239be2d2f0b4837cb2f01d")


# define function to get desired block-group data 

get_info <- function(variable, name,
                                geography = "block group",
                                year = 2019){
  # variable = variable code, as character
  # name = substantive name of variable, to name output column
  # year = year, as numeric
  out <- map_dfr(state.abb, function(x) get_acs(
    state = x, 
    variables = variable, 
    geography = geography,
    ouput = wide, 
    year = year
  )) %>% 
    select(GEOID, NAME, estimate) 
  names(out)[3] <- name
  
  return(out)
}

find_acs_table("income")

# run function over race variables 
total_pop <- get_blockgroup_info(variable = "B01003_001",
                                 name = "total_pop") 

white_pop <- get_blockgroup_info(variable = "B02009_001",
                                 name = "white_pop")

black_pop <- get_blockgroup_info(variable = "B02010_001")

asian_pop <- get_blockgroup_info(variable = "B02011_001")

# get non-white population totals 
non_white_pop <- get_info(variable = "B03002_001",
                      name = "total_pop") %>% 
  left_join(., get_info(variable = "B03002_003",
                        name = "white_alone"),
            by = c("GEOID","NAME")) %>% 
  mutate(non_white_pop = total_pop - white_alone, 
         non_white_prop = non_white_pop / total_pop)



# get poverty rate 
poverty <- get_info(variable = "C17002_002",
                 name = "pov1") %>% 
  left_join(., 
            get_info(variable = "C17002_003", 
            name = "pov2"), 
            by = c("GEOID","NAME")) %>% 
  left_join(., 
            get_info(variable = "C17002_001",
                     name = "population"),
            by = c("GEOID","NAME")) %>% 
  mutate(poverty_total = pov1 + pov2, 
         poverty_rate = poverty_total / population) 



# get average income by block group
income_blockgroup <- get_info(variable = "B19013_001E",
                                         name = "median_income") 

census_out <- left_join(non_white_pop, poverty, 
          by = c("GEOID","NAME")) %>% 
  left_join(., income_blockgroup, 
            by = c("GEOID","NAME")) %>% 
  select(GEOID, NAME, 
         total_pop, non_white_prop,
         poverty_rate,
         median_income)





load_variables(year = 2019, 
               dataset = "acs5") %>% 
  View()
