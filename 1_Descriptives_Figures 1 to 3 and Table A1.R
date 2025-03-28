

# Section 1: Descriptive figures and tables ----

library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(reshape2)


fulfill_data <- read.csv("data/clean_data.csv")



## Section 1.1: Descriptive plots. ----

### Section 1.1.1: Preparing data for Figure 1 and Figure 2 ----

# Define dependent variables
dep_vars <- c("diff_pp_area", "diff_room_temp", "diff_shower_freq", "diff_worry", "diff_cold",
              "diff_worry_dum", "diff_cold_dum")

plot_data <- fulfill_data %>%
  select(country, all_of(dep_vars))%>%
  group_by(country) %>%
  summarize(across(all_of(dep_vars), list(
    below_0_xn = ~ sum(. < 0, na.rm = TRUE),
    equal_0_xn = ~ sum(. == 0, na.rm = TRUE),
    above_0_xn = ~ sum(. > 0, na.rm = TRUE),
    below_0_xperc = ~ sum(. < 0, na.rm = TRUE) / sum(!is.na(.)) * 100,
    equal_0_xperc = ~ sum(. == 0, na.rm = TRUE) / sum(!is.na(.)) * 100,
    above_0_xperc = ~ sum(. > 0, na.rm = TRUE) / sum(!is.na(.)) * 100
  ), .names = "{.col}_x{.fn}"), .groups = "drop")%>%
  bind_rows(
    fulfill_data %>%
      subset(year==2021)%>%
      select(all_of(dep_vars)) %>%
      summarize(across(all_of(dep_vars), list(
        below_0_xn = ~ sum(. < 0, na.rm = TRUE),
        equal_0_xn = ~ sum(. == 0, na.rm = TRUE),
        above_0_xn = ~ sum(. > 0, na.rm = TRUE),
        below_0_xperc = ~ sum(. < 0, na.rm = TRUE) / sum(!is.na(.)) * 100,
        equal_0_xperc = ~ sum(. == 0, na.rm = TRUE) / sum(!is.na(.)) * 100,
        above_0_xperc = ~ sum(. > 0, na.rm = TRUE) / sum(!is.na(.)) * 100
      ), .names = "{.col}_x{.fn}")) %>%
      mutate(country = "Pooled sample")
  )

plot_data1<- plot_data %>%
  pivot_longer(-country, names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("dep_var", "category", "type"), sep = "_x", extra = "merge") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(category = factor(category, levels = c("below_0", "equal_0", "above_0"),
                           labels = c("Lower in 2022 than in 2021", "Same in 2022 and in 2021", "Higher in 2022 than in 2021")))%>%
  mutate(category=case_when((dep_var=="diff_worry_dum"| dep_var=="diff_cold_dum") & category=="Same in 2022 and in 2021"~"Lower in 2022 than in 2021 or\nSame in 2022 and in 2021", 
                            T~category))%>%
  mutate(dep_var=case_when (dep_var=="diff_pp_area" ~"Heated area",
                            dep_var=="diff_room_temp" ~"Room temperature",
                            dep_var=="diff_shower_freq" ~"Frequency of bathing & showering",
                            dep_var=="diff_worry" ~"Worried about energy bill payments - difference",
                            dep_var=="diff_cold" ~"Room temperature perceived as unsafe - difference",
                            dep_var=="diff_worry_dum" ~"Worried about energy bill payments",
                            dep_var=="diff_cold_dum" ~"Room temperature perceived as unsafe",
                            T~NA))



### Section 1.1.2: Figure 1 Overview of change in heating behaviour outcome variables between 2021 and 2022 by country. ----


cbPalette <- c( "#CC79A7","#E69F00", "#56B4E9", "#009E73", "#F0E442" )

(Figure_1<-plot_data1%>%
    subset(!is.na(dep_var))%>%
    filter(!dep_var%in%c("Worried about energy bill payments","Room temperature perceived as unsafe","Room temperature perceived as unsafe - difference","Worried about energy bill payments - difference"))%>%
    ggplot(aes(x = factor(country, levels=c("Latvia","Italy","Germany","France","Denmark", "Pooled sample")), y = perc, fill=factor(category, levels=c( "Higher in 2022 than in 2021","Same in 2022 and in 2021","Lower in 2022 than in 2021")))) + 
    geom_col(position="stack")+
    ylab("") + xlab("") +
    # ggtitle("")+
    facet_wrap(~factor(dep_var, levels=c("Room temperature",
                                         "Heated area",
                                         "Frequency of bathing & showering"
    )), ncol = 2)+
    theme(panel.grid.major.x = element_line(colour="darkgrey"), panel.grid.major.y = element_blank(),
          panel.grid.minor = element_line(colour="lightgrey"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          axis.ticks.y = element_blank(),axis.ticks.x = element_blank(),
          strip.background = element_blank())+
    theme(legend.title=element_blank())+
    scale_fill_manual(values=cbPalette, limits = c("Lower in 2022 than in 2021","Same in 2022 and in 2021","Higher in 2022 than in 2021"))+
    #geom_bar(aes(y = (..count..)/sum(..count..))) + 
    theme(text=element_text(size=16))
  #  +geom_text(size = 4, position = position_stack(vjust = 0.5), colour="black", aes(label=paste0(round(perc, digits=0),"%")))
  +scale_y_continuous(labels = function(x) paste0(x, "%"))
  # +geom_text(size = 4, position = position_stack(vjust = 0.5), colour="grey20",aes(label=paste0(round(perc, digits=1),"%")), check_overlap=T)
  +geom_text_repel(position = position_stack(vjust = 0.5),force=0.004,size=5,
                   #aes(label=paste0(n, " (",round(perc, digits=0),"%)")))
                   aes(label=paste0(round(perc, digits=0),"%")))
  +coord_flip()
)

ggsave("images/Figure_1.png",Figure_1, width = 14, height=6 )



### Section 1.1.3: Figure 2 Overview of change in energy deprivation outcome variables between 2021 and 2022 by country. ----


cbPalette <- c(  "#009E73","#E69F00",  "#F0E442" )

(Figure_2<-plot_data1%>%
    subset(!is.na(dep_var))%>%
    mutate(category=case_when(category=="Lower in 2022 than in 2021"~"Not experienced in 2022 but experienced in 2021",
                              category=="Higher in 2022 than in 2021"~"Experienced in 2022 but not in 2021",
                              T~category))%>%
    filter(dep_var%in%c("Worried about energy bill payments - difference","Room temperature perceived as unsafe - difference"))%>%
    ggplot(aes(x = factor(country, levels=c("Latvia","Italy","Germany","France","Denmark", "Pooled sample")), 
               y = perc, 
               fill=factor(category, levels=c( "Experienced in 2022 but not in 2021","Same in 2022 and in 2021","Not experienced in 2022 but experienced in 2021")))) + 
    geom_col(position="stack")+
    ylab("") + xlab("") +
    # ggtitle("")+
    facet_wrap(~factor(dep_var, levels=c(#"Room temperature",
      #"Heated area",
      #"Frequency of bathing & showering",
      "Room temperature perceived as unsafe - difference",
      "Worried about energy bill payments - difference")))+
    theme(panel.grid.major.x = element_line(colour="darkgrey"), panel.grid.major.y = element_blank(),
          panel.grid.minor = element_line(colour="lightgrey"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          axis.ticks.y = element_blank(),axis.ticks.x = element_blank(),
          strip.background = element_blank())+
    theme(legend.title=element_blank())+
    scale_fill_manual(values=cbPalette, limits = c("Not experienced in 2022 but experienced in 2021","Same in 2022 and in 2021","Experienced in 2022 but not in 2021"))+
    #geom_bar(aes(y = (..count..)/sum(..count..))) + 
    theme(text=element_text(size=16))
  #  +geom_text(size = 4, position = position_stack(vjust = 0.5), colour="black", aes(label=paste0(round(perc, digits=0),"%")))
  +scale_y_continuous(labels = function(x) paste0(x, "%"))
  # +geom_text(size = 4, position = position_stack(vjust = 0.5), colour="grey20",aes(label=paste0(round(perc, digits=1),"%")), check_overlap=T)
  +geom_text_repel(position = position_stack(vjust = 0.5),force=0.004,size=5,
                   #aes(label=paste0(n, " (",round(perc, digits=0),"%)")))
                   aes(label=paste0(round(perc, digits=0),"%")))
  +theme(legend.position="bottom",
         plot.margin = margin(1, 10, 1, 0))
  
  +coord_flip()
)


ggsave("images/Figure_2.png",Figure_2, width = 12, height=4 )


### Section 1.1.4: Figure 3 Stated change in per unit heating price between 2021 and 2022 per country.  ----

cbPalette <- c( "#009E73", "#56B4E9",  "#F0E442", "#CC79A7","#E69F00" )


cost_plot_data<-fulfill_data%>%
  mutate(H10 = case_when(
    H10 == "1" ~ "Significantly decreased",
    H10 == "2" ~ "Somewhat decreased",
    H10 == "3" ~ "More or less unchanged",
    H10 == "4" ~ "Somewhat increased",
    H10 == "5" ~ "Significantly increased",
    TRUE ~ NA
  )) %>%
  subset(!is.na(H10))

(Figure_3<-cost_plot_data%>%
    count(H10, country) %>%
    subset(!is.na(H10))%>%
    group_by(country) %>%
    mutate(perc = (n / sum(n)) * 100) %>%  # Calculate percentage for each country
    ungroup() %>%
    bind_rows(  # Add a pooled sample (all countries together)
      cost_plot_data %>%
        count(H10) %>%
        mutate(country = "Pooled sample", perc = (n / sum(n)) * 100))%>%
    ggplot(aes(factor(country, levels=c("Latvia","Italy","Germany","France","Denmark", "Pooled sample")),perc,fill=factor(H10, levels=c("Significantly increased",
                                                                                                                                        "Somewhat increased",
                                                                                                                                        "More or less unchanged",
                                                                                                                                        "Somewhat decreased",
                                                                                                                                        "Significantly decreased")))) + 
    geom_col(position="stack", )+
    ylab("") + xlab("") +
    scale_fill_manual(values=cbPalette, limits = c( "Significantly decreased",
                                                    "Somewhat decreased",
                                                    "More or less unchanged",
                                                    "Somewhat increased",
                                                    "Significantly increased"))+
    #labs(caption=paste0("DK: n=1851, LV: n=1369, IT: n=1901, DE: n=1803, FR: n=1836, and "))+
    # ggtitle("")+
    #facet_wrap(~factor(heat_fuel))+
    theme(panel.grid.major.x = element_line(colour="darkgrey"), 
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_line(colour="lightgrey"),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank())+
    theme(legend.title=element_blank())+
    #geom_bar(aes(y = (..count..)/sum(..count..))) + 
    theme(text=element_text(size=17))
  #  +geom_text(size = 4, position = position_stack(vjust = 0.5), colour="black", aes(label=paste0(round(perc, digits=0),"%")))
  +scale_y_continuous(labels = function(x) paste0(x, "%"))
  # +geom_text(size = 4, position = position_stack(vjust = 0.5), colour="grey20",aes(label=paste0(round(perc, digits=1),"%")), check_overlap=T)
  +geom_text_repel(position = position_stack(vjust = 0.5),force=0.001,size=5,lineheight = 0.7,
                   #aes(label=paste0(n, "\n(",round(perc, digits=0),"%)")))
                   aes(label=paste0(round(perc, digits=0),"%")))
  #aes(label=paste0(n)))
  +coord_flip()
)




ggsave(plot=Figure_3, file="images/Figure_3.jpeg", width = 12, height=4)




# Section 1.2: Appendix A1: Table A1	Descriptive statistics of the variables. ----


fulfill_data_sub <- fulfill_data %>%
  select(diff_room_temp, diff_pp_area, diff_shower_freq, diff_cold_dum,  diff_worry_dum, big_price_incr, female, Age, income_pp_1000, high_educ, sfh, owner, city, country) %>%
  rename(
    "Heated area per person" = diff_pp_area,
    "Room temperature" = diff_room_temp,
    "Frequency of bathing" = diff_shower_freq,
    "Worried about paying energy bill" = diff_worry_dum,
    "Room temperature perceived as unsafe" = diff_cold_dum,
    "Urban (vs. town/rural)"=city,
    "Female (vs. male)"=female,
    "Income (in 1,000€ per person)"=income_pp_1000, 
    "Academic degree (vs. no academic degree)"=high_educ, 
    "Single-family-home (vs. flat)"=sfh, 
    "Homeowner (vs. tenant)"=owner,
    "Significant price increase"=big_price_incr
  )

# List of variables for the descriptive statistics
variables <- names(fulfill_data_sub)[1:13]

# Empty list
results <- list()

# Loop across all variables
for (var in variables) {
  
  # pooled sample
  pooled_stats <- fulfill_data_sub %>%
    summarise(
      N = sum(!is.na(get(var))),
      Mean = mean(get(var), na.rm = TRUE),
      Min = min(get(var), na.rm = TRUE),
      Max = max(get(var), na.rm = TRUE),
      SD = sd(get(var), na.rm = TRUE)
    ) %>%
    mutate(Variable = var)%>%
    mutate(country="Pooled")
  
  # Country samples
  country_stats <- fulfill_data_sub %>%
    group_by(country) %>%
    summarise(
      N = sum(!is.na(get(var))),
      Mean = mean(get(var), na.rm = TRUE),
      Min = min(get(var), na.rm = TRUE),
      Max = max(get(var), na.rm = TRUE),
      SD = sd(get(var), na.rm = TRUE)
    ) %>%
    mutate(Variable = var)
  
  stats<-rbind(pooled_stats, country_stats )
  
  results[[var]] <- stats %>%
      pivot_wider(names_from = country, values_from = c(N, Mean, SD, Min, Max),
                  names_sep = "_") %>%
      arrange(Variable)
  

}

# Combining rows 
Table_A1 <- bind_rows(results, .id = "Variable")


#reordering columns
column_order <- sort(unique(fulfill_data_orig$country))
new_order <- c()
for ( country in c("Pooled",column_order)) {
  new_order <- c(new_order, 
                 paste0( "N_",country), 
                 paste0(  "Mean_",country), 
                 paste0(  "SD_",country), 
                 paste0(  "Min_",country), 
                 paste0(  "Max_",country))
}

#Adding variable heading
new_order <- c("Variable", new_order)

Table_A1 <- Table_A1[new_order]
Table_A1[-1] <- lapply(Table_A1[-1], round, 2)

# Exporting as excel
library(writexl)
write_xlsx(Table_A1, "Results/Table_A1_Descriptive_statistics.xlsx")













