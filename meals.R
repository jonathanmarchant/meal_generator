library(readxl)
library(tidyverse)

meals <- read_excel("../meals.xlsx", sheet="meals") %>% 
  mutate(length = case_when(prepped_in_5 == "Y" ~ "prepped",
                            timing_min <= 15 ~ "short",
                            timing_min <= 35 ~ "medium",
                            TRUE ~ "long"))

ingredients <- read_excel("../meals.xlsx", sheet="ingredients") %>% 
  mutate(units = replace_na(units, ""))

check_sample <- function(sample, check_params) {
  num_long <- sample %>% 
    filter(length == "long") %>% 
    nrow()
  
  num_medlong <- sample %>% 
    filter(length %in% c("medium", "long")) %>% 
    nrow()
  
  num_same_meat <- sample %>% 
    summarise(freq = n(), .by=meat) %>% 
    summarise(max = max(freq)) %>% 
    pull("max")
  
  num_same_starch <- sample %>% 
    summarise(freq = n(), .by=starch) %>% 
    summarise(max = max(freq)) %>% 
    pull("max")
  
  specials <- sample %>% 
    filter(!is.na(special))
  
  if (nrow(specials) == 0)
  {
    num_same_special <- 0
  } else {
    num_same_special <- specials  %>% 
      summarise(freq = n(), .by=special) %>% 
      summarise(max = max(freq)) %>% 
      pull("max")
  }
  
  errors <- 0
  if (num_long / nrow(sample) > check_params$max_long) { errors <- errors + 1 }
  if (num_medlong / nrow(sample) > check_params$max_medlong) { errors <- errors + 1 }
  if (num_same_meat > check_params$same_meat_max) { errors <- errors + 1 }
  if (num_same_starch > check_params$same_starch_max) { errors <- errors + 1 }
  if (num_same_special > check_params$same_special_max) { errors <- errors + 1 }
  
  return(errors)
}

run_sample <- function(meals, meals_required, must_include = c()) {
  sample1 <- meals %>%
    filter(meal_id %in% must_include)

  sample2 <- meals %>%
    filter(!(meal_id %in% must_include)) %>%
    sample_n(meals_required - length(must_include))

  bind_rows(sample1, sample2)
}

find_menu <- function(meals,
                      must_include = c(),
                      meals_required = 4,
                      same_meat_max = 2,
                      same_starch_max = 2,
                      same_special_max = 1,
                      max_long = 0.35,
                      max_medlong = 0.5) {
  
  check_params <- list(
    same_meat_max = same_meat_max,
    same_starch_max = same_starch_max,
    same_special_max = same_special_max,
    max_long = max_long,
    max_medlong = max_medlong
  )
  
  if(length(must_include) > 0)
  {
    must_include <- must_include %>% 
      as_tibble_col(column_name="meal_name") %>%
      inner_join(meals) %>% 
      pull("meal_id")
  }
  
  errors <- 1
  attempts <- 0
  while (errors > 0 & attempts < 1000) {
    sample <- run_sample(meals, meals_required, must_include)
    errors <- check_sample(sample, check_params)
    attempts <- attempts + 1
  }
  
  if (errors > 0) { stop("valid menu not found") }
  
  sample %>% 
    mutate(meal_id = as.integer(meal_id),
           timing_min = as.integer(timing_min))
}
