
# Section 3: Appendix B and Appendix C tables of the OLS results for the multivariate models. ----

library(dplyr)
library(tidyr)
library(readxl)
library(officer)
library(flextable)


data_long_orig<- read.csv("data/clean_data.csv")


## Section 3.1: Data prep ----

# Define dependent variables and countries
dep_vars <- c("diff_pp_area", "diff_room_temp", "diff_shower_freq", "diff_worry_dum", "diff_cold_dum")
countries <- c("Denmark", "France", "Germany", "Italy", "Latvia")


# creating energy source dummies
data_long <- data_long_orig %>%
  mutate(NG=ifelse(heat_fuel=="NG",1,0))%>%
  mutate(oil=ifelse(heat_fuel=="oil",1,0))%>%
  mutate(elec=ifelse(heat_fuel=="electricity",1,0))%>%
  mutate(district=ifelse(heat_fuel=="district",1,0))%>%
  mutate(wood=ifelse(heat_fuel=="wood",1,0))%>%
  mutate(other=ifelse(heat_fuel%in%c("other","solar","Biogas","LPG"),1,0))%>%
  # whether room temperature read from thermostat or not
  mutate(thermostat=case_when(H6==1~ 1,
                              H6==2~ 0,
                              T~NA))%>%
  # whether room temperature is under 18°C, i.e., actually unsafe
  mutate(room_temp_unsafe=ifelse(room_temp>18,0,1))

# Adding data for heated degree days
nrg_data <- readxl::read_xlsx("data/External data sources/nrg_chddr2_a_page_spreadsheet.xlsx", sheet="Sheet 1")

nrg_data<-nrg_data%>%
  rename(Region_nominal=Region)%>%
  mutate_at(vars(HDD_2022,HDD_2021), as.numeric)%>%
  mutate(HDD=HDD_2022-HDD_2021)%>%
  select(HDD, Region_nominal)

# Joining by region
data_long_HDD<- left_join(data_long,nrg_data, by = "Region_nominal")

## Section 3.2: Estimating the models

### Section 3.2.1:  R0 Main models ----

dep_vars <- c("diff_pp_area", "diff_room_temp", "diff_shower_freq", "diff_worry_dum", "diff_cold_dum")
countries <- c("Denmark", "France", "Germany", "Italy", "Latvia")



# empty list to store model results
lm_result <- list()
# numbering the first results
i <- 1

for (dep_var in dep_vars) {
  for (cntry in countries) {
    
    
    if (dep_var == "shower_freq") {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)%>%
        subset(HW1==1)
    } else {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)
    }
    
    
    # Estimating models for each outcome variable and country
    lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city, se_type = "HC3", data = data_sub))$coefficients)
    
    # Adding the country and outcome variable
    lm_result[[i]]<-lm_result[[i]]%>%
      mutate(country=cntry)%>%
      mutate(depvar=dep_var)
    
    # Adding variable names
    lm_result[[i]]$var <- rownames(lm_result[[i]])
    # Adding number of cases
    lm_result[[i]]$N <- nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city, se_type = "HC3", data = data_sub))
    
    i <- i + 1
  }
  
  
  
  if (dep_var == "shower_freq") {
    data_sub <- data_long %>%
      rename(depvar=dep_var)%>%
      subset(HW1==1)
  } else {
    data_sub <- data_long %>%
      rename(depvar=dep_var)
  }
  
  # Estimating models for each outcome variable for the pooled sample
  lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+DK+FR+IT+LV, se_type = "HC3", data = data_sub))$coefficients)
  
  # Adding the country and outcome variable
  lm_result[[i]]<-lm_result[[i]]%>%
    mutate(country="Pooled sample")%>%
    mutate(depvar=dep_var)
  
  # Adding variable names
  lm_result[[i]]$var <- rownames(lm_result[[i]])
  # Adding number of cases
  lm_result[[i]]$N <- nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+DK+FR+IT+LV, se_type = "HC3", data = data_sub))
  i <- i + 1
}


R0_combined_df_basic <- bind_rows(lm_result)
rownames(R0_combined_df_basic) <- NULL

### Section 3.2.2: R1 Robustness check: with energy carrier ----

i <- 1
lm_result <- list()


for (dep_var in dep_vars) {
  for (cntry in countries) {
    
    
    if (dep_var == "shower_freq") {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)%>%
        subset(HW1==1)
    } else {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)
    }
    

    
    lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+district+oil+elec+wood+other, se_type = "HC3", data = data_sub))$coefficients)
    
    lm_result[[i]]<-lm_result[[i]]%>%
      mutate(country=cntry)%>%
      mutate(depvar=dep_var)
    
    lm_result[[i]]$var <- rownames(lm_result[[i]])
    lm_result[[i]]$N <- nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+district+oil+elec+wood+other, se_type = "HC3", data = data_sub))
    
    i <- i + 1
  }
  
  
  
  if (dep_var == "shower_freq") {
    data_sub <- data_long %>%
      rename(depvar=dep_var)%>%
      subset(HW1==1)
  } else {
    data_sub <- data_long %>%
      rename(depvar=dep_var)
  }
  
  
  lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+DK+FR+IT+LV+district+oil+elec+wood+other, se_type = "HC3", data = data_sub))$coefficients)
  
  lm_result[[i]]<-lm_result[[i]]%>%
    mutate(country="Pooled sample")%>%
    mutate(depvar=dep_var)
  
  lm_result[[i]]$var <- rownames(lm_result[[i]])
  lm_result[[i]]$N <- nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+DK+FR+IT+LV+district+oil+elec+wood+other, se_type = "HC3", data = data_sub))
  i <- i + 1
}

R1_combined_df_basic <- bind_rows(lm_result)
rownames(R1_combined_df_basic) <- NULL



### Section 3.2.3: R2 Robustness check: controlling for room temperature estimation method----


i <- 1
lm_result <- list()


for (dep_var in c("diff_room_temp")) {
  for (cntry in countries) {
    
    
    if (dep_var == "shower_freq") {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)%>%
        subset(HW1==1)
    } else {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)
    }
    
    
    
    lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+ thermostat, se_type = "HC3", data = data_sub))$coefficients)
    
    lm_result[[i]]<-lm_result[[i]]%>%
      mutate(country=cntry)%>%
      mutate(depvar=dep_var)
    
    lm_result[[i]]$var <- rownames(lm_result[[i]])
    lm_result[[i]]$N<-nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+ thermostat, se_type = "HC3", data = data_sub))
    
    i <- i + 1
  }
  
  
  
  if (dep_var == "shower_freq") {
    data_sub <- data_long %>%
      rename(depvar=dep_var)%>%
      subset(HW1==1)
  } else {
    data_sub <- data_long %>%
      rename(depvar=dep_var)
  }
  
  
  lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+thermostat+DK+FR+IT+LV, se_type = "HC3", data = data_sub))$coefficients)
  
  lm_result[[i]]<-lm_result[[i]]%>%
    mutate(country="Pooled sample")%>%
    mutate(depvar=dep_var)
  
  lm_result[[i]]$var <- rownames(lm_result[[i]])
  lm_result[[i]]$N<-nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+ thermostat+DK+FR+IT+LV, se_type = "HC3", data = data_sub))
  i <- i + 1
}

R2_combined_df_basic <- bind_rows(lm_result)
rownames(R2_combined_df_basic) <- NULL




### Section 3.2.4: R3 cold with room temp and heat area ----



i <- 1
lm_result <- list()


for (dep_var in c("diff_cold_dum")) {
  for (cntry in countries) {
    
    
    if (dep_var == "shower_freq") {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)%>%
        subset(HW1==1)
    } else {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)
    }
    
    
    
    lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+room_temp_unsafe+room_temp+pp_area, se_type = "HC3", data = data_sub))$coefficients)
    
    lm_result[[i]]<-lm_result[[i]]%>%
      mutate(country=cntry)%>%
      mutate(depvar=dep_var)
    
    lm_result[[i]]$var <- rownames(lm_result[[i]])
    lm_result[[i]]$N<-nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+room_temp_unsafe+room_temp+pp_area, se_type = "HC3", data = data_sub))
    
    i <- i + 1
  }
  
  
  
  if (dep_var == "shower_freq") {
    data_sub <- data_long %>%
      rename(depvar=dep_var)%>%
      subset(HW1==1)
  } else {
    data_sub <- data_long %>%
      rename(depvar=dep_var)
  }
  
  
  lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+room_temp_unsafe+room_temp+pp_area+DK+FR+IT+LV, se_type = "HC3", data = data_sub))$coefficients)
  
  lm_result[[i]]<-lm_result[[i]]%>%
    mutate(country="Pooled sample")%>%
    mutate(depvar=dep_var)
  
  lm_result[[i]]$var <- rownames(lm_result[[i]])
  lm_result[[i]]$N<-nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+room_temp_unsafe+room_temp+pp_area+DK+FR+IT+LV, se_type = "HC3", data = data_sub))
  i <- i + 1
}

R3_combined_df_basic <- bind_rows(lm_result)
rownames(R3_combined_df_basic) <- NULL




### Section 3.2.5: R4 with region dummies ----



i <- 1
lm_result <- list()


for (dep_var in dep_vars) {
  for (cntry in countries) {
    
    
    if (dep_var == "shower_freq") {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)%>%
        subset(HW1==1)
    } else {
      data_sub <- data_long %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)
    }
    
    
    
    lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+ factor(Region_nominal), se_type = "HC3", data = data_sub))$coefficients)
    
    lm_result[[i]]<-lm_result[[i]]%>%
      mutate(country=cntry)%>%
      mutate(depvar=dep_var)
    
    lm_result[[i]]$var <- rownames(lm_result[[i]])
    lm_result[[i]]$N<-nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+ factor(Region_nominal), se_type = "HC3", data = data_sub))
    
    i <- i + 1
  }
  
  
  
  if (dep_var == "shower_freq") {
    data_sub <- data_long %>%
      rename(depvar=dep_var)%>%
      subset(HW1==1)
  } else {
    data_sub <- data_long %>%
      rename(depvar=dep_var)
  }

   data_sub <- within(data_sub, Region_nominal <- relevel(as.factor(Region_nominal), ref = "Capital Region of Denmark"))


  lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+ factor(Region_nominal), se_type = "HC3", data = data_sub))$coefficients)

  lm_result[[i]]<-lm_result[[i]]%>%
    mutate(country="Pooled sample")%>%
    mutate(depvar=dep_var)

  lm_result[[i]]$var <- rownames(lm_result[[i]])
  lm_result[[i]]$N<-nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+DK+FR+IT+LV+  factor(Region_nominal), se_type = "HC3", data = data_sub))
  i <- i + 1
}

R4_combined_df_basic <- bind_rows(lm_result)
rownames(R4_combined_df_basic) <- NULL


R4_summary_df <- R4_combined_df_basic%>%
  rename(p_value = `Pr(>|t|)`)%>%
  filter(!var%in%c("(Intercept)", "DE", "DK", "LV", "FR", "IT"))%>%
  subset(depvar!="diff_cold" & depvar!="diff_worry")%>%
  #subset(var=="big_price_incr")%>%
  mutate(sign=case_when(Estimate > 0 & p_value <0.1 ~ "positive", 
                        Estimate < 0 & p_value <0.1 ~ "negative",
                        T~"null"))%>%
  select(country, depvar, var)%>%
  arrange(factor(var, levels = c('big_price_incr', 'female', 'Age', 'income_pp_1000', 'high_educ', 'sfh', 'owner','city' )))%>%
  arrange(factor(depvar, levels = c('diff_room_temp', 'diff_pp_area', 'diff_shower_freq', 'diff_cold_dum', 'diff_worry_dum' )))%>%
  mutate(depvar=case_when (depvar=="diff_pp_area" ~"heated area per person",
                           depvar=="diff_room_temp" ~"room temperature",
                           depvar=="diff_shower_freq" ~"frequency of bathing & showering",
                           depvar=="diff_worry_dum" ~"worried about paying energy bill",
                           depvar=="diff_cold_dum" ~"room temperature perceived as unsafe",
                           T~depvar))%>%
  subset(!is.na(var))




### Section 3.2.6: R5 with heating degree day difference ----



i <- 1
lm_result <- list()


for (dep_var in dep_vars) {
  for (cntry in countries) {
    
    
    if (dep_var == "shower_freq") {
      data_sub <- data_long_HDD %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)%>%
        subset(HW1==1)
    } else {
      data_sub <- data_long_HDD %>%
        subset(country == cntry)%>%
        rename(depvar=dep_var)
    }
    
    
    
    lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+HDD, se_type = "HC3", data = data_sub))$coefficients)
    
    lm_result[[i]]<-lm_result[[i]]%>%
      mutate(country=cntry)%>%
      mutate(depvar=dep_var)
    
    lm_result[[i]]$var <- rownames(lm_result[[i]])
    lm_result[[i]]$N<-nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+HDD, se_type = "HC3", data = data_sub))
    
    i <- i + 1
  }
  
  
  
  if (dep_var == "shower_freq") {
    data_sub <- data_long_HDD %>%
      rename(depvar=dep_var)%>%
      subset(HW1==1)
  } else {
    data_sub <- data_long_HDD %>%
      rename(depvar=dep_var)
  }
  
  
  lm_result[[i]] <- as.data.frame(summary(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+HDD+DK+FR+IT+LV, se_type = "HC3", data = data_sub))$coefficients)
  
  lm_result[[i]]<-lm_result[[i]]%>%
    mutate(country="Pooled sample")%>%
    mutate(depvar=dep_var)
  
  lm_result[[i]]$var <- rownames(lm_result[[i]])
  lm_result[[i]]$N<-nobs(lm_robust(depvar~ big_price_incr+ female+ Age+ income_pp_1000+ high_educ+ sfh+owner+city+HDD+DK+FR+IT+LV, se_type = "HC3", data = data_sub))
  i <- i + 1
}

R5_combined_df_basic <- bind_rows(lm_result)
rownames(R5_combined_df_basic) <- NULL

R5_summary_df <- R5_combined_df_basic%>%
  rename(p_value = `Pr(>|t|)`)%>%
  filter(!var%in%c("(Intercept)", "DE", "DK", "LV", "FR", "IT"))%>%
  subset(depvar!="diff_cold" & depvar!="diff_worry")%>%
  select(country, depvar, var)%>%
  arrange(factor(var, levels = c('big_price_incr', 'female', 'Age', 'income_pp_1000', 'high_educ', 'sfh', 'owner','city' )))%>%
  arrange(factor(depvar, levels = c('diff_room_temp', 'diff_pp_area', 'diff_shower_freq', 'diff_cold_dum', 'diff_worry_dum' )))%>%
  mutate(depvar=case_when (depvar=="diff_pp_area" ~"heated area per person",
                           depvar=="diff_room_temp" ~"room temperature",
                           depvar=="diff_shower_freq" ~"frequency of bathing & showering",
                           depvar=="diff_worry_dum" ~"worried about paying energy bill",
                           depvar=="diff_cold_dum" ~"room temperature perceived as unsafe",
                           T~depvar))%>%
  subset(!is.na(var))


table(data_long$heat_fuel, data_long$country)

## Section 3.3: Creation of the tables in Word ----


### Section 3.3.1: Cleaning the model data for each model ----

# Function to clean data from each model 
clean_models <- function(unclean_model) {
  
clean_basic_model <- unclean_model %>%
  # Add stars for significance
  mutate(stars = case_when(
    `Pr(>|t|)` <= 0.01 ~ "***",
    `Pr(>|t|)` <= 0.05 ~ "**",
    `Pr(>|t|)` <= 0.1 ~ "*",
    TRUE ~ ""
  )) %>%
  # Create combined coefficient and standard error string
  mutate(coeff = paste0(
    sprintf('%.3f', Estimate), stars, "\n(", sprintf('%.3f', `Std. Error`), ")"
  )) %>%
  # Select and rename outcome variables
  mutate(depvar = case_when(
    depvar == "diff_pp_area" ~ "Heated area",
    depvar == "diff_room_temp" ~ "Room temperature",
    depvar == "diff_shower_freq" ~ "Frequency of bathing & showering",
    depvar == "diff_worry_dum" ~ "Worried about energy bill payments",
    depvar == "diff_cold_dum" ~ "Room temperature perceived as unsafe",
    TRUE ~ depvar
  )) %>%
  arrange(factor(depvar, levels = c('Room temperature', 'Heated area', 'Frequency of bathing & showering','Room temperature perceived as unsafe', 'Worried about energy bill payments' )))%>%
  # Rename covariates
  mutate(var = case_when(
    var == "big_price_incr" ~ "Significant price increase",
    var == "female" ~ "Female (vs. male)",
    var == "Age" ~ "Age",
    var == "income_pp_1000" ~ "Income per person (in €1000)",
    var == "high_educ" ~ "Academic degree (vs. no academic degree)",
    var == "sfh" ~ "Single-family home (vs. multi-family)",
    var == "owner" ~ "Homeowner (vs. tenant)",
    var == "city" ~ "Urban (vs. town/rural)",
    var == "DK" ~ "Denmark (vs. Germany)",
    var == "FR" ~ "France (vs. Germany)",
    var == "IT" ~ "Italy (vs. Germany)",
    var == "LV" ~ "Latvia (vs. Germany)",
    var == "district" ~ "District heating (vs. natural gas)",
    var == "elec" ~ "Electricity (vs. natural gas)",
    var == "oil" ~ "Heating oil (vs. natural gas)",
    var == "wood" ~ "Wood (vs. natural gas)",
    var == "other" ~ "Other heating source (vs. natural gas)",
    var == "(Intercept)" ~ "Constant",
    var == "pp_area" ~ "Heated area",
    var == "room_temp" ~ "Room temperature",
    var == "thermostat"~ "Thermostat (vs. estimate)",
    var == "room_temp_unsafe" ~ "Room temperature below 18°C",
    var == "HDD" ~ "Difference in heating degree days",
    TRUE ~ var
  ))%>%
  #creating regional dummies
  mutate(var = case_when(grepl("factor\\(Region_nominal\\)", var) & country %in% c("Germany") ~ paste(gsub("factor\\(Region_nominal\\)", "", var),"(vs. Baden-Württemberg)"),
                         grepl("factor\\(Region_nominal\\)", var) & country %in% c("Italy") ~ paste(gsub("factor\\(Region_nominal\\)", "", var),"(vs. Abruzzo)"),
                         grepl("factor\\(Region_nominal\\)", var) & country %in% c("France") ~ paste(gsub("factor\\(Region_nominal\\)", "", var),"(vs. Auvergne-Rhône-Alpes)"),
                         grepl("factor\\(Region_nominal\\)", var) & country %in% c("Denmark") ~ paste(gsub("factor\\(Region_nominal\\)", "", var),"(vs. Capital Region of Denmark)"),
                         grepl("factor\\(Region_nominal\\)", var) & country %in% c("Latvia") ~ paste(gsub("factor\\(Region_nominal\\)", "", var),"(vs. Kurzeme)"),
                         grepl("factor\\(Region_nominal\\)", var) & country %in% c("Pooled sample") ~ paste(gsub("factor\\(Region_nominal\\)", "", var),"(vs. Capital Region of Denmark)"),
                         T~var))


    clean_basic_model <- clean_basic_model %>%
      select(var, coeff, country, depvar, N) 
      
    n_rows_bas <- clean_basic_model %>%
      distinct(country, depvar, N) %>%
      mutate(var = "N", coeff = as.character(N)) %>%
      select(var, coeff, country, depvar)
    
    clean_basic_model <- bind_rows(clean_basic_model, n_rows_bas) %>%
      select(-N)
    
    data_subset<-clean_basic_model%>%
      mutate(title=paste(country, depvar, sep="__"))%>%
      arrange(country=factor(country, levels=c("Pooled sample", "Denmark", "France", "Germany","Italy", "Latvia")))



data_subset<-data_subset %>%
  arrange(var = factor(var, levels = c(
    "Significant price increase",
    "Female (vs. male)",
    "Age",
    "Above median age",
    "Income per person (in €1000)",
    "Income in lowest quartile",
    "Academic degree (vs. no academic degree)",
    "Single-family home (vs. multi-family)",
    "Homeowner (vs. tenant)",
    "Urban (vs. town/rural)",
    "Room temperature",
    "Heated area",
    "Thermostat (vs. estimate)",
    "Room temperature below 18°C",
    "Difference in heating degree days",
    "District heating (vs. natural gas)",
    "Electricity (vs. natural gas)",
    "Heating oil (vs. natural gas)",
    "Wood (vs. natural gas)",
    "Other heating source (vs. natural gas)",
    "Denmark (vs. Germany)",
    "France (vs. Germany)",
    "Italy (vs. Germany)",
    "Latvia (vs. Germany)",
    
    
    "Central Denmark Region (vs. Capital Region of Denmark)",
    "North Denmark Region (vs. Capital Region of Denmark)",
    "Zealand Region (vs. Capital Region of Denmark)",
    "Region of Southern Denmark (vs. Capital Region of Denmark)",
    
    
    "Burgundy Franche-Comté (vs. Auvergne-Rhône-Alpes)",
    "Brittany (vs. Auvergne-Rhône-Alpes)",
    "Centre-Val de Loire (vs. Auvergne-Rhône-Alpes)",
    "Corsica (vs. Auvergne-Rhône-Alpes)",
    "Grand Est (vs. Auvergne-Rhône-Alpes)",
    "Hauts-de-France (vs. Auvergne-Rhône-Alpes)",
    "Île-de-France (vs. Auvergne-Rhône-Alpes)",
    "Normandy (vs. Auvergne-Rhône-Alpes)",
    "Nouvelle Aquitaine (vs. Auvergne-Rhône-Alpes)",
    "Occitania (vs. Auvergne-Rhône-Alpes)",
    "Paris Region (vs. Auvergne-Rhône-Alpes)",
    "Pays de la Loire (vs. Auvergne-Rhône-Alpes)",
    "Provence-Alpes-Côte d'Azur (vs. Auvergne-Rhône-Alpes)",
    
    "Auvergne-Rhône-Alpes (vs. Capital Region of Denmark)",
    "Burgundy-Franche-Comté (vs. Capital Region of Denmark)",
    "Brittany (vs. Capital Region of Denmark)",
    "Centre-Val de Loire (vs. Capital Region of Denmark)",
    "Corsica (vs. Capital Region of Denmark)",
    "Grand Est (vs. Capital Region of Denmark)",
    "Hauts-de-France (vs. Capital Region of Denmark)",
    "Île-de-France (vs. Capital Region of Denmark)",
    "Normandy (vs. Capital Region of Denmark)",
    "Nouvelle Aquitaine (vs. Capital Region of Denmark)",
    "Occitanie (vs. Capital Region of Denmark)",
    "Paris Region (vs. Capital Region of Denmark)",
    "Pays de la Loire (vs. Capital Region of Denmark)",
    "Provence-Alpes-Côte d'Azur (vs. Capital Region of Denmark)",
    
    "Bavaria (vs. Baden-Württemberg)",
    "Berlin (vs. Baden-Württemberg)",
    "Brandenburg (vs. Baden-Württemberg)",
    "Bremen (vs. Baden-Württemberg)",
    "Hamburg (vs. Baden-Württemberg)",
    "Hesse (vs. Baden-Württemberg)",
    "Lower Saxony (vs. Baden-Württemberg)",
    "Mecklenburg-Western Pomerania (vs. Baden-Württemberg)",
    "North Rhine-Westphalia (vs. Baden-Württemberg)",
    "Rhineland-Palatinate (vs. Baden-Württemberg)",
    "Saarland (vs. Baden-Württemberg)",
    "Saxony (vs. Baden-Württemberg)",
    "Saxony-Anhalt (vs. Baden-Württemberg)",
    "Schleswig-Holstein (vs. Baden-Württemberg)",
    "Thuringia (vs. Baden-Württemberg)",
    
    "Baden-Württemberg (vs. Capital Region of Denmark)",
    "Bavaria (vs. Capital Region of Denmark)",
    "Berlin (vs. Capital Region of Denmark)",
    "Brandenburg (vs. Capital Region of Denmark)",
    "Bremen (vs. Capital Region of Denmark)",
    "Hamburg (vs. Capital Region of Denmark)",
    "Hesse (vs. Capital Region of Denmark)",
    "Lower Saxony (vs. Capital Region of Denmark)",
    "Mecklenburg-Western Pomerania (vs. Capital Region of Denmark)",
    "North Rhine-Westphalia (vs. Capital Region of Denmark)",
    "Rhineland-Palatinate (vs. Capital Region of Denmark)",
    "Saarland (vs. Capital Region of Denmark)",
    "Saxony (vs. Capital Region of Denmark)",
    "Saxony-Anhalt (vs. Capital Region of Denmark)",
    "Schleswig-Holstein (vs. Capital Region of Denmark)",
    "Thuringia (vs. Capital Region of Denmark)",
    
    "Apulia (vs. Abruzzo)",
    "Basilicata (vs. Abruzzo)",
    "Calabria (vs. Abruzzo)",
    "Campania (vs. Abruzzo)",
    "Emilia-Romagna (vs. Abruzzo)",
    "Friuli-Venezia Giulia (vs. Abruzzo)",
    "Lazio (vs. Abruzzo)",
    "Liguria (vs. Abruzzo)",
    "Lombardy (vs. Abruzzo)",
    "Marche (vs. Abruzzo)",
    "Molise (vs. Abruzzo)",
    "Piedmont (vs. Abruzzo)",
    "Sardinia (vs. Abruzzo)",
    "Sicily (vs. Abruzzo)",
    "Trentino - South Tyrol (vs. Abruzzo)",
    "Tuscany (vs. Abruzzo)",
    "Umbria (vs. Abruzzo)",
    "Veneto (vs. Abruzzo)",
    
    "Abruzzo (vs. Capital Region of Denmark)",
    "Apulia (vs. Capital Region of Denmark)",
    "Basilicata (vs. Capital Region of Denmark)",
    "Calabria (vs. Capital Region of Denmark)",
    "Campania (vs. Capital Region of Denmark)",
    "Emilia-Romagna (vs. Capital Region of Denmark)",
    "Friuli-Venezia Giulia (vs. Capital Region of Denmark)",
    "Lazio (vs. Capital Region of Denmark)",
    "Liguria (vs. Capital Region of Denmark)",
    "Lombardy (vs. Capital Region of Denmark)",
    "Marche (vs. Capital Region of Denmark)",
    "Molise (vs. Capital Region of Denmark)",
    "Piedmont (vs. Capital Region of Denmark)",
    "Sardinia (vs. Capital Region of Denmark)",
    "Sicily (vs. Capital Region of Denmark)",
    "Trentino - South Tyrol (vs. Capital Region of Denmark)",
    "Tuscany (vs. Capital Region of Denmark)",
    "Umbria (vs. Capital Region of Denmark)",
    "Veneto (vs. Capital Region of Denmark)",
    
    
    "Latgale (vs. Kurzeme)",
    "Pieriga (vs. Kurzeme)",
    "Riga (vs. Kurzeme)",
    "Vidzeme (vs. Kurzeme)",
    "Zemgale (vs. Kurzeme)",
    
    "Kurzeme (vs. Capital Region of Denmark)",
    "Latgale (vs. Capital Region of Denmark)",
    "Pieriga (vs. Capital Region of Denmark)",
    "Riga (vs. Capital Region of Denmark)",
    "Vidzeme (vs. Capital Region of Denmark)",
    "Zemgale (vs. Capital Region of Denmark)",
    
    "Constant",
    "N"
  )))

clean_basic_model<- return(data_subset)

}

R0_basic_data<-clean_models(R0_combined_df_basic)
R1_basic_data<-clean_models(R1_combined_df_basic)
R2_basic_data<-clean_models(R2_combined_df_basic)
R3_basic_data<-clean_models(R3_combined_df_basic)
R4_basic_data<-clean_models(R4_combined_df_basic)
R5_basic_data<-clean_models(R5_combined_df_basic)



### Section 3.3.2: Creation of the Word documents with the tables ----

# Define country groups
countries <- c("the pooled sample", "Denmark", "France", "Germany","Italy", "Latvia")

country_group <- list(c("Pooled sample", "Denmark"), 
                      c("France", "Germany"),
                      c("Italy", "Latvia"))


# Initialize Word document

model_results <- list(
  R0_basic_data = R0_basic_data, 
  R1_basic_data = R1_basic_data, 
  R2_basic_data = R2_basic_data, 
  R3_basic_data = R3_basic_data, 
  R4_basic_data = R4_basic_data, 
  R5_basic_data = R5_basic_data
)

doc_B <- read_docx()
doc_C <- read_docx()
  
for (i in seq_along(model_results)){
  result <- model_results[[i]]  
  result_name <- names(model_results[i])
  
  #Setting table name, Table B for main models, Table C for robustness tests
  if (result_name %in% c("R0_basic_data")) {
    table <- "Table B"
    numb <- 1
    extension <- ""
    
  } else {
    if (result_name %in% c("R1_basic_data")) {
      table <- "Table C"
      numb <- 1 
      extension <- ", with energy carrier"
    } else {  
      if (result_name %in% c("R2_basic_data")) {
        table <- "Table C"
        extension <- ", controlling for room temperature estimation method"
      } else {
        if (result_name %in% c("R3_basic_data")) {
          table <- "Table C"
          extension <- ", controlling for room temperature and heated area"
          
        } else {
          if (result_name %in% c("R4_basic_data")) {
            table <- "Table C"
            extension <- ", controlling for region"
            
          } else {
            if (result_name %in% c("R5_basic_data")){
              table <- "Table C"
              extension <- ", controlling for heating degree days"
            }
          }
        }
      }
    }
  }
  
  # country lists for the models with only one outcome variable
  if (any(grepl("R2", result_name), grepl("R3", result_name))) {
    country_group1 <- list(c("all types of samples"))
  } else {
    country_group1 <- list(c("Pooled sample", "Denmark"), 
                        c("France", "Germany"),
                        c("Italy", "Latvia"))
    } 
  
  model <- ": OLS results of the multivariate models"
  country_group2<-country_group
    
  
    for (cntry_grp in seq_along(country_group1)){
      
      if (country_group1[[cntry_grp]][1] == "Pooled sample") {
        country_group2[[cntry_grp]][1] <- "pooled sample"
      } 
      
      # Since only one outcome variable, all countries in one table for robustness tests R2 and R3
      if (any(grepl("R2", result_name), grepl("R3", result_name))) {
        
        data_subset <- result %>%
          subset(!is.na(coeff)) %>%
          dplyr::select(-country, -depvar) %>%
          pivot_wider(names_from = title, values_from = coeff)
        # Setting header row
        header_row <- c("", 
                        country_group[[1]][1], 
                        country_group[[1]][2], 
                        country_group[[2]][1], 
                        country_group[[2]][2], 
                        country_group[[3]][1], 
                        country_group[[3]][2])
        # specifying the country
        country_caption <- "all country samples"
        


      } else {
        
        data_subset <-result%>%
          subset(!is.na(coeff))%>%
          filter(country %in% country_group1[[cntry_grp]])%>%
          dplyr::select(-country, -depvar)%>%
          pivot_wider(names_from = title, values_from = coeff)
        # Setting header row
        header_row<-c("", country_group1[[cntry_grp]][1],"","","","", country_group1[[cntry_grp]][2],"","","","")
        # specifying the country
        country_caption <- paste(toString(country_group2[[cntry_grp]][1]),"and",toString(country_group2[[cntry_grp]][2]))
      }
     
      
      # Creating caption for the specific table
      caption <- as_paragraph(
        table, format(round(numb, digits = 0)), 
        model, extension, "—", country_caption,"."
      )
      
      
      # Column names as a row since they repeat themselves 
      new_row <- as.data.frame(t(gsub(".*__(.*).*", "\\1", names(data_subset ))))
      colnames(new_row) <- names(data_subset )
    
      # joining header row to model data
      data_subset <-rbind(header_row,new_row, data_subset )
      
      # Setting table charcteristics
    flextable_table_stan <- flextable(data_subset ) %>%
      set_table_properties(width = 1, layout = "autofit")%>%
      delete_rows(i=1, part = "header")%>% 
      labelizor(part = "body", labels = c("var" = ""))%>% #setting "var" to empty
      hline_top(border = fp_border(width = 1.2), part="body") %>%
      italic(j = 1, i=1:(nrow(data_subset )-2), part = "body") %>%
      set_caption(caption = caption, align_with_table = FALSE, fp_p = fp_par(text.align = "left")) %>%
      hline_top(border = fp_border(width = 1.2), part="body") %>%
      hline(i = 2, border = fp_border(width = 1.2)) %>%
      hline(i = nrow(data_subset ) - 1, border = fp_border(width = 1.2)) %>%
      hline(i = nrow(data_subset ), border = fp_border(width = 1.2, color = "black")) %>%
      align(align = "left", part = "all") %>%
      valign(valign = "top", part = "all")%>%
      add_footer_lines("Note: Robust standard errors in parentheses. ***p<0.01, **p<0.05, *p<0.01"
      )%>% 
      padding(padding = 1.8, part = "all")
    
    
      
    
    #Only merging cells for country if required
    if (any(grepl("R2", result_name), grepl("R3", result_name)))  {
      
      flextable_table_stan1<-flextable_table_stan 
      
    } else {
      
      flextable_table_stan1<-merge_at(flextable_table_stan, i=1, j=2:6)
      flextable_table_stan1<-merge_at(flextable_table_stan1, i=1, j=7:11)
    }
    
    #automated table counting
    numb <- numb + 1
    
    #adding tables to the documents
    if (result_name %in% c("R0_basic_data")) {
      doc_B <- doc_B %>%
        body_add_flextable(flextable_table_stan1)%>%
        body_add_break()
      
    } else {
        doc_C <- doc_C %>%
          body_add_flextable(flextable_table_stan1)%>%
          body_add_break()
       
      }
      
    }
    
}


# Exporting tables as word documents
print(doc_B, target = "Results/Appendix B.docx")
print(doc_C, target = "Results/Appendix C.docx")
  


