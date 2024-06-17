library(tidyverse)
library(countrycode)

jmp_raw <- read_csv("data-raw/2022-10-18_jmp_sanitation_raw_data.csv")

# preliminary checks
unique(jmp_raw$type)
unique(jmp_raw$year)
min(jmp_raw$year, na.rm = TRUE)
max(jmp_raw$year, na.rm = TRUE)
unique(jmp_raw$var_long)

unique(jmp_raw$residence)

# add country names and remove data points on the national level
jmp_clean <- jmp_raw |> 
  mutate(country = countrycode(iso3, origin = 'iso3c', destination = 'country.name')) |> 
  filter(residence != "national")

# check again
unique(jmp_clean$type)
unique(jmp_clean$year)
min(jmp_clean$year, na.rm = TRUE)
max(jmp_clean$year, na.rm = TRUE)
unique(jmp_clean$var_long)

# First prototype of JMP estimate calculation --------------------------------------------------------------------

basic_df <- jmp_clean |> 
  filter(country == "New Zealand" & var_long == "Improved" & residence == "rural")

min_year <- min(basic_df$year)
max_year <- max(basic_df$year)

year_range <- max_year-min_year
print(year_range)

if(min_year-2 < 2000){
  
  min_year_extrap <- 2000
  
}else{
  
  min_year_extrap <- min_year-2
  
}

if(max_year+2 > 2024){
  
  max_year_extrap <- 2024
  
}else{
  
  max_year_extrap <- max_year+2
  
}

years_to_extrapolate <- data.frame(year = c(min_year_extrap, max_year_extrap))

n_rows <- nrow(basic_df)
print(n_rows)

if(year_range >= 5 & n_rows > 1){
  
  regression <- lm(formula = value ~ year, data = basic_df)
  
  extrapolations <- as.vector(predict(regression, years_to_extrapolate))
  
  extrapolation_df <- data.frame(source = "Prediction", 
                                 year = years_to_extrapolate,
                                 value = extrapolations)
  
}else{
  
  mean_value <- mean(basic_df$value)
  
  extrapolation_df <- data.frame(source = "Prediction", 
                                 year = years_to_extrapolate,
                                 value = rep(mean_value, times = nrow(years_to_extrapolate)))
  
}

min_years_extend <- c(min_year_extrap-2, min_year_extrap-4)
max_years_extend <- c(max_year_extrap+2, max_year_extrap+4)

years_to_extend <- c(min_years_extend, max_years_extend)

extension_df <- data.frame(source = "Extension", 
                           year = years_to_extend,
                           value = NA) 

final_df <- bind_rows(basic_df, extrapolation_df, extension_df) |> 
  filter(year >= 2000 & year <= 2024) |> 
  arrange(year) |> 
  fill(everything(), .direction = "downup")
