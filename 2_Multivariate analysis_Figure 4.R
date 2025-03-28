
# Section 2: Figure 4 OLS results for the multivariate models. ----


library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


data_long<- read.csv("data/clean_data.csv")



## Section 2.1: Estimating the OLS models ----

# Setting dependent variables and countries

dep_vars <- c("diff_pp_area", "diff_room_temp", "diff_shower_freq", "diff_worry_dum", "diff_cold_dum")
countries <- c("Denmark", "France", "Germany", "Italy", "Latvia")

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

#combining all model results
combined_df_basic <- bind_rows(lm_result)
# Removing row names
rownames(combined_df_basic) <- NULL


## Section 2.2: Creating Figure 4 ----

# Renaming outcome variables
combined_df_basic1<-combined_df_basic%>%
  mutate(dep_var=case_when (depvar=="diff_pp_area" ~"Heated area",
                            depvar=="diff_room_temp" ~"Room temperature",
                            depvar=="diff_shower_freq" ~"Frequency of\nbathing & showering",
                            depvar=="diff_worry" ~"Difference in\nWorried about\nenergy bill payments",
                            depvar=="diff_cold" ~"Difference in\nexperiencing\nunsafe temperature",
                            depvar=="diff_worry_dum" ~"Worried about\nenergy bill payments",
                            depvar=="diff_cold_dum" ~"Room temperature\nperceived as unsafe",
                            T~depvar))

# creating the figure
(Figure_4 <- combined_df_basic1 %>%
    mutate(country=ifelse(country=="the pooled sample","the pooled sample", country))%>%
    subset(!dep_var %in% c("Difference in\nWorried about\nenergy bill payments","Difference in\nexperiencing\nunsafe temperature"))%>%
    subset(!var%in%c("(Intercept)", "DK", "FR","DE","IT", "LV"))%>%
    mutate(significance = ifelse(`Pr(>|t|)` >= 0.1, "insignificant", "significant"),
           color_code = case_when(Estimate > 0 & significance =="significant"~ "#E69F00", 
                                  Estimate < 0 & significance =="significant"~"#56B4E9",
                                  T~"insignificant")) %>%
    mutate(var=case_when(var=="(Intercept)"~"Constant",
                         var=="big_price_incr"~"Significant price increase",
                         var=="female"~"Female (vs. male)",
                         var=="Age"~"Age",
                         var=="income_pp_1000"~"Income per person (in 1000€)",
                         var=="high_educ"~"Academic degree",
                         var=="sfh"~"Single-family home (vs. multi-family)",
                         var=="owner"~"Homeowner (vs. tenant)",
                         var=="city"~"Urban (vs. town/rural)",
                         var=="NG_elec"~"Natural gas/electricity (vs. other)",
                         T~var))%>%
    mutate(facet_var=paste(country, dep_var, sep="\n"))%>%
    mutate(country=ifelse(country=="the pooled sample", "Pooled sample",country))%>%
    mutate(facet_var=factor(facet_var, levels=c( "Pooled sample\nRoom temperature", 
                                                 "Denmark\nRoom temperature", 
                                                 "France\nRoom temperature", 
                                                 "Germany\nRoom temperature",
                                                 "Italy\nRoom temperature", 
                                                 "Latvia\nRoom temperature",
                                                 
                                                 "Pooled sample\nHeated area", 
                                                 "Denmark\nHeated area",
                                                 "France\nHeated area", 
                                                 "Germany\nHeated area",
                                                 "Italy\nHeated area", 
                                                 "Latvia\nHeated area",
                                                 
                                                 "Pooled sample\nFrequency of\nbathing & showering", 
                                                 "Denmark\nFrequency of\nbathing & showering", 
                                                 "France\nFrequency of\nbathing & showering", 
                                                 "Germany\nFrequency of\nbathing & showering",
                                                 "Italy\nFrequency of\nbathing & showering", 
                                                 "Latvia\nFrequency of\nbathing & showering",
                                                 
                                                 "Pooled sample\nRoom temperature\nperceived as unsafe", 
                                                 "Denmark\nRoom temperature\nperceived as unsafe",
                                                 "France\nRoom temperature\nperceived as unsafe",
                                                 "Germany\nRoom temperature\nperceived as unsafe",
                                                 "Italy\nRoom temperature\nperceived as unsafe",
                                                 "Latvia\nRoom temperature\nperceived as unsafe",
                                                 
                                                 "Pooled sample\nWorried about\nenergy bill payments", 
                                                 "Denmark\nWorried about\nenergy bill payments",
                                                 "France\nWorried about\nenergy bill payments",
                                                 "Germany\nWorried about\nenergy bill payments",
                                                 "Italy\nWorried about\nenergy bill payments",
                                                 "Latvia\nWorried about\nenergy bill payments"
                                                 
                                                 
    )))%>%
    ggplot(aes(x = Estimate, y = factor(var, levels=c("Natural gas/electricity (vs. other)", "Urban (vs. town/rural)", 
                                                      "Homeowner (vs. tenant)", "Single-family home (vs. multi-family)", "Academic degree", 
                                                      "Income per person (in 1000€)", "Age", "Female (vs. male)", 
                                                      "Significant price increase"
    )))) +
    facet_wrap(~facet_var, nrow=5, scales = "free_x") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth=0.5) +
    geom_point(aes(color=color_code), size=0.9) + # added color based on Estimate
    geom_errorbar(aes(
      xmin = Estimate - `Std. Error` * 1.645,
      xmax = Estimate + `Std. Error` * 1.645,
      color=color_code,
      size = color_code,
    ), width = 0
    ) +
    labs(x = "Marginal and discrete effects", y = "",
         # caption = "Note: Blue (orange) lines indicate the CIs when the point estimate is negative (positive) and the 𝑝-value is below 0.1. Grey lines indicate CIs when the 𝑝-value is equal to or above 0.1."
    ) +
    scale_size_manual(values = c("#E69F00" = 0.75, "#56B4E9" = 0.75, "insignificant" = 0.4))+ 
    scale_color_manual(values = c("#E69F00" = "#E69F00", "#56B4E9" = "#56B4E9", "insignificant" = "grey50")) +
    theme_minimal()%>%
    theme(
      line = element_line(colour = "black", linetype = 1, lineend = "butt"),
      rect = element_rect(fill = "white", colour = "black",  linetype = 1),
      # axis.text.y = element_text(size = 25, colour = "grey30", lineheight = 0.3),
      axis.text.x = element_text( colour = "grey30", lineheight = 0.4,angle = 45),
      # axis.title = element_text(size = 25),
      axis.title.x = element_text(margin = margin(t = 2, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(angle = 90, margin = margin(t = 0, r = 2, b = 0, l = 0)),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "grey92"),
      panel.grid.minor = element_line(colour = "grey92"),
      panel.spacing = unit(1, "lines"),
      panel.spacing.y = unit(-0.5, "lines"),
      strip.background = element_rect(fill = "white", colour = "white"),
      strip.text = element_text( face = "bold"),
      plot.background = element_rect(colour = NA),
      # plot.title = element_text(size = rel(1.2)),
      plot.margin = margin(1, 10, 1, 0),
      panel.spacing.x = unit(3, "mm"),
      legend.position="none"
    )
)

# Saving Figure 4
ggsave("images/Figure_4.png",Figure_4, width = 10.5, height=11 )

