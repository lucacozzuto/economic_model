library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr)
library(plm)
library(jtools)
library(janitor)
library(texreg)
library(fixest)
library(modelsummary)
library(gridExtra)

rm(list = ls())


#################################################################
#BOURGEOISIE WEALTH
bourgeoisie_ratio_files <- c("text/bourgeoisie-ratio-wealth_27.0_cat.txt","text/bourgeoisie-ratio-wealth_30.0_cat.txt",
               "text/bourgeoisie-ratio-wealth_33.0_cat.txt")
               
data_bourgeoisie_ratio <- map_df(bourgeoisie_ratio_files, read_csv, skip = 6,
               show_col_types = FALSE) #imports the list of file names
data_bourgeoisie_ratio <- clean_names(data_bourgeoisie_ratio) # very useful, automatically cleans all names to easy R handling names
data_bourgeoisie_ratio <- data_bourgeoisie_ratio %>% mutate(id = run_number * starting_seed,
                                          bourgeoisie_ratio_factor = as.factor(bourgeoisie_ratio_wealth))

bourgeoisie_ratio_plot <- ggplot(data = data_bourgeoisie_ratio, aes(x = bourgeoisie_ratio_wealth, y = gdp_spending, fill = bourgeoisie_ratio_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(27,30,33)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Ration of Bourgeoisie Wealth', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave("bourgeoisie_ratio")
#################################################################
#DISTRIBUTION


distribution_files <- c("text/distribution_3.6_cat.txt","text/distribution_4.0_cat.txt",
                        "text/distribution_4.4_cat.txt")

data_distribution <- map_df(distribution_files, read_csv, skip = 6,
                                 show_col_types = FALSE) #imports the list of file names
data_distribution <- clean_names(data_distribution) # very useful, automatically cleans all names to easy R handling names
data_distribution <- data_distribution %>% mutate(id = run_number * starting_seed,
                                                       distribution_factor = as.factor(distribution))

data_distribution_plot <- ggplot(data = data_distribution, aes(x = distribution, y = gdp_spending, fill = distribution_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(3.6,4.0,4.4)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Distribution', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################
#FIRM MAX LABOR 

firm_labor_files <- c("text/firm-labor_18.0_cat.txt","text/firm-labor_20.0_cat.txt",
                      "text/firm-labor_22.0_cat.txt")

data_firm_labor <- map_df(firm_labor_files, read_csv, skip = 6,
                            show_col_types = FALSE) #imports the list of file names
data_firm_labor <- clean_names(data_firm_labor) # very useful, automatically cleans all names to easy R handling names
data_firm_labor <- data_firm_labor %>% mutate(id = run_number * starting_seed,
                                                  firm_labor_factor = as.factor(firm_labor))

data_firm_labor_plot <- ggplot(data = data_firm_labor, aes(x = firm_labor, y = gdp_spending, fill = firm_labor_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(3.6,4.0,4.4)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Firms Max Labor', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#################################################################
#FIRM PRODUCTIVITY

firm_prod_files <- c("text/firm-productivity_5.4_cat.txt","text/firm-productivity_6.0_cat.txt",
                     "text/firm-productivity_6.6_cat.txt")


data_firm_prod_files<- map_df(firm_prod_files, read_csv, skip = 6,
                          show_col_types = FALSE) #imports the list of file names
data_firm_prod_files <- clean_names(data_firm_prod_files) # very useful, automatically cleans all names to easy R handling names
data_firm_prod_files <- data_firm_prod_files %>% mutate(id = run_number * starting_seed,
                                                        firm_productivity_factor = as.factor(firm_productivity))

data_firm_prod_files_plot <- 
  ggplot(data = data_firm_prod_files, 
         aes(x = firm_productivity, 
             y = gdp_spending,
             fill = firm_productivity_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(5.5,6.0,6.6)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Firm Productivity', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#################################################################
## GOVERNMENT DEMAND

gov_demand_files <- c("text/government-demand_0.09_cat.txt","text/government-demand_0.10_cat.txt",
            "text/government-demand_0.11_cat.txt")


data_gov_demand_files <- map_df(gov_demand_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_gov_demand_files <- clean_names(data_gov_demand_files) # very useful, automatically cleans all names to easy R handling names
data_gov_demand_files <- data_gov_demand_files %>% 
  mutate(id = run_number * starting_seed,
         government_demand_factor = as.factor(government_demand))

data_gov_demand_files_plot <- 
  ggplot(data = data_gov_demand_files, aes(x = government_demand, y = gdp_spending, fill = government_demand_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(0.09,0.1,0.11)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'GOVERMENT DEMAND', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#################################################################
gov_initial_wealth_files <- c("text/government-initial-wealth_90000.0_cat.txt","text/government-initial-wealth_100000.0_cat.txt",
            "text/government-initial-wealth_110000.0_cat.txt")


data_gov_initial_wealth_files <- map_df(gov_initial_wealth_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_gov_initial_wealth_files <- clean_names(data_gov_initial_wealth_files) # very useful, automatically cleans all names to easy R handling names
data_gov_initial_wealth_files <- data_gov_initial_wealth_files %>%
  mutate(id = run_number * starting_seed,
         government_initial_wealth_factor = as.factor(government_initial_wealth))

data_gov_initial_wealth_files_plot <- ggplot(data = data_gov_initial_wealth_files, 
                               aes(x = government_initial_wealth, y = gdp_spending, 
                                   fill = government_initial_wealth_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(90000,100000,110000)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'GOVERNMENT INITIAL WEALTH', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################

#INDUSTRIAL SWITCH PROBABILITY

industrial_switch_probability_files <- c("text/industrial-switch-probability_0.18_cat.txt","text/industrial-switch-probability_0.20_cat.txt",
            "text/industrial-switch-probability_0.22_cat.txt")


data_industrial_switch_probability_files <- map_df(industrial_switch_probability_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_industrial_switch_probability_files <- clean_names(data_industrial_switch_probability_files) # very useful, automatically cleans all names to easy R handling names
data_industrial_switch_probability_files <- data_industrial_switch_probability_files %>% 
  mutate(id = run_number * starting_seed,
    industrial_switch_probability_factor = as.factor(industrial_switch_probability))

data_industrial_switch_probability_files_plot <- 
  ggplot(data = data_industrial_switch_probability_files,
         aes(x =industrial_switch_probability , y = gdp_spending, 
             fill = industrial_switch_probability_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(0.18,0.2,0.22)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Industrial Switch Probability', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################
initial_capital_files <- c("text/initial-capital_450.0_cat.txt","text/initial-capital_500.0_cat.txt",
            "text/initial-capital_550.0_cat.txt")


data_initial_capital_files <- map_df(initial_capital_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_initial_capital_files <- clean_names(data_initial_capital_files) # very useful, automatically cleans all names to easy R handling names
data_initial_capital_files <- data_initial_capital_files %>% mutate(id = run_number * starting_seed,
                                                                    initial_capital_factor = as.factor(initial_capital))

data_initial_capital_files_plot <- ggplot(data = data_initial_capital_files, 
                               aes(x = initial_capital, y = gdp_spending, fill = initial_capital_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(450,500,550)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'INITIAL FIRM CAPITAL', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################
initial_household_wealth_files <- c("text/initial-household-wealth_27.0_cat.txt","text/initial-household-wealth_30.0_cat.txt",
            "text/initial-household-wealth_33.0_cat.txt")


data_initial_household_wealth_files <- map_df(initial_household_wealth_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_initial_household_wealth_files <- clean_names(data_initial_household_wealth_files) # very useful, automatically cleans all names to easy R handling names
data_initial_household_wealth_files <- data_initial_household_wealth_files %>% 
  mutate(id = run_number * starting_seed,
         initial_household_wealth_factor = as.factor(initial_household_wealth))

data_initial_household_wealth_filesplot <- ggplot(data = data_initial_household_wealth_files, 
                               aes(x = initial_household_wealth, y = gdp_spending, fill = initial_household_wealth_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(27,30,33)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'INITIAL HOUSEHOLD WEALTH', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################
initial_service_price_files <- c("text/initial-service-price_13.5_cat.txt","text/initial-service-price_15.0_cat.txt",
            "text/initial-service-price_16.5_cat.txt")


data_initial_service_price_files <- map_df(initial_service_price_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_initial_service_price_files <- clean_names(data_initial_service_price_files) # very useful, automatically cleans all names to easy R handling names
data_initial_service_price_files <- data_initial_service_price_files %>% 
  mutate(id = run_number * starting_seed,
         initial_service_price_factor = as.factor(initial_service_price))

data_initial_service_price_files_plot <- ggplot(data = data_initial_service_price_files, 
                                                aes(x = initial_service_price, y = gdp_spending, fill = initial_service_price_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(13.5,15,16.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'INITIAL SERVICE PRICE', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################
markup_files <- c("text/markup_1.8_cat.txt","text/markup_2.0_cat.txt",
            "text/markup_2.2_cat.txt")


data_markup_files <- map_df(markup_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_markup_files <- clean_names(data_markup_files) # very useful, automatically cleans all names to easy R handling names
data_markup_files <- data_markup_files %>% mutate(id = run_number * starting_seed,
                          markup_factor = as.factor(markup))

data_markup_files_plot <- ggplot(data = data_markup_files, aes(x = markup, y = gdp_spending, fill = markup_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(1.8,2.0,2.2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'MARKUP', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################
nobles_ratio_wealth_files <- c("text/nobles-ratio-wealth_45.0_cat.txt","text/nobles-ratio-wealth_50.0_cat.txt",
            "text/nobles-ratio-wealth_55.0_cat.txt")


data_nobles_ratio_wealth_files <- map_df(nobles_ratio_wealth_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_nobles_ratio_wealth_files <- clean_names(data_noble_ratio_wealth_files) # very useful, automatically cleans all names to easy R handling names
data_nobles_ratio_wealth_files <- data_nobles_ratio_wealth_files %>% 
  mutate(id = run_number * starting_seed,
         nobles_ratio_wealth_factor = as.factor(nobles_ratio_wealth))

data_nobles_ratio_wealth_files_plot <- ggplot(data = data_nobles_ratio_wealth_files, aes(x = nobles_ratio_wealth, y = gdp_spending, fill = nobles_ratio_wealth_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(45,50,55)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NOBLES RATIO WEALTH', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################


number_of_bourgeoisie_files <- c("text/number-of-bourgeoisie_900.0_cat.txt","text/number-of-bourgeoisie_1000.0_cat.txt",
            "text/number-of-bourgeoisie_1100.0_cat.txt")


data_number_of_bourgeoisie_files <- map_df(number_of_bourgeoisie_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_number_of_bourgeoisie_files <- clean_names(data_number_of_bourgeoisie_files) # very useful, automatically cleans all names to easy R handling names
data_number_of_bourgeoisie_files <- data_number_of_bourgeoisie_files %>% 
  mutate(id = run_number * starting_seed,
         number_of_bourgeoisie_factor = as.factor(number_of_bourgeoisie))

data_number_of_bourgeoisie_files_plot <- ggplot(data = data_number_of_bourgeoisie_files, aes(x = number_of_bourgeoisie, y = gdp_spending, fill = number_of_bourgeoisie_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(900,1000,1100)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NUMBER OF BOURGEOISIE', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#################################################################


number_of_firms_files <- c("text/number-of-firms_450.0_cat.txt","text/number-of-firms_500.0_cat.txt",
                           "text/number-of-firms_550.0_cat.txt")


data_number_of_firms_files <- map_df(number_of_firms_files, read_csv, skip = 6,
                show_col_types = FALSE) #imports the list of file names
data_number_of_firms_files <- clean_names(data_number_of_firms_files) # very useful, automatically cleans all names to easy R handling names
data_number_of_firms_files <- data_number_of_firms_files %>% 
  mutate(id = run_number * starting_seed,
         number_of_firms_files_factor = as.factor(number_of_firms))

data_number_of_firms_files_plot <- ggplot(data = data_number_of_firms_files, 
                                          aes(x = number_of_firms, y = gdp_spending, fill = number_of_firms_files_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(450,500,550)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NUMBER OF FIRMS', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################


number_of_nobles_files <- c("text/number-of-nobles_45.0_cat.txt","text/number-of-nobles_50.0_cat.txt",
                           "text/number-of-nobles_55.0_cat.txt")


data_number_of_nobles_files <- map_df(number_of_nobles_files, read_csv, skip = 6,
                                     show_col_types = FALSE) #imports the list of file names
data_number_of_nobles_files <- clean_names(data_number_of_nobles_files) # very useful, automatically cleans all names to easy R handling names
data_number_of_nobles_files <- data_number_of_nobles_files %>% 
  mutate(id = run_number * starting_seed,
         number_of_nobles_files_factor = as.factor(number_of_nobles))

data_number_of_nobles_files_plot <- ggplot(data = data_number_of_nobles_files, 
                                          aes(x = number_of_nobles, y = gdp_spending, fill = number_of_firms_files_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(45,50,55)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NUMBER OF NOBLES', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################


number_of_workers_files <- c("text/number-of-workers_9000.0_cat.txt","text/number-of-workers_10000.0_cat.txt",
                           "text/number-of-workers_11000.0_cat.txt")


data_number_of_workers_files <- map_df(number_of_workers_files, read_csv, skip = 6,
                                     show_col_types = FALSE) #imports the list of file names
data_number_of_workers_files <- clean_names(data_number_of_workers_files) # very useful, automatically cleans all names to easy R handling names
data_number_of_workers_files <- data_number_of_workers_files %>% 
  mutate(id = run_number * starting_seed,
         number_of_workers_files_factor = as.factor(number_of_workers))

data_number_of_workers_files_plot <- ggplot(data = data_number_of_workers_files, 
                                          aes(x = number_of_workers, y = gdp_spending, fill = number_of_workers_files_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(9000,10000,11000)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NUMBER OF WORKERS', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################


price_delta_files <- c("text/price-delta_0.18_cat.txt","text/price-delta_0.20_cat.txt",
                           "text/price-delta_0.22_cat.txt")


data_price_delta_files <- map_df(price_delta_files, read_csv, skip = 6,
                                     show_col_types = FALSE) #imports the list of file names
data_price_delta_files <- clean_names(data_price_delta_files) # very useful, automatically cleans all names to easy R handling names
data_price_delta_files <- data_price_delta_files %>% 
  mutate(id = run_number * starting_seed,
         price_delta_files_factor = as.factor(price_delta))

data_price_delta_files_plot <- ggplot(data = data_price_delta_files, 
                                          aes(x = price_delta, y = gdp_spending, fill = price_delta_files_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(0.18,0.2,0.22)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NUMBER OF FIRMS', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################


safe_zone_files <- c("text/safe-zone_0.27_cat.txt","text/safe-zone_0.30_cat.txt",
                       "text/safe-zone_0.33_cat.txt")


data_safe_zone_files <- map_df(safe_zone_files, read_csv, skip = 6,
                                 show_col_types = FALSE) #imports the list of file names
data_safe_zone_files <- clean_names(data_safe_zone_files) # very useful, automatically cleans all names to easy R handling names
data_safe_zone_files <- data_safe_zone_files %>% 
  mutate(id = run_number * starting_seed,
         safe_zone_files_factor = as.factor(safe_zone))

data_safe_zone_files_plot <- ggplot(data = data_safe_zone_files, 
                                      aes(x = safe_zone, y = gdp_spending, fill = safe_zone_files_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(0.18,0.2,0.22)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NUMBER OF FIRMS', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################


service_productivity_files <- c("text/service-productivity_9.0_cat.txt","text/service-productivity_10.0_cat.txt",
                       "text/service-productivity_11.0_cat.txt")


data_service_productivity_files <- map_df(service_productivity_files, read_csv, skip = 6,
                                 show_col_types = FALSE) #imports the list of file names
data_service_productivity_files <- clean_names(data_service_productivity_files) # very useful, automatically cleans all names to easy R handling names
data_service_productivity_files <- data_service_productivity_files %>% 
  mutate(id = run_number * starting_seed,
         service_productivity_files_factor = as.factor(service_productivity))

data_service_productivity_files_plot <- ggplot(data = data_service_productivity_files, 
                                      aes(x = service_productivity, y = gdp_spending, fill = service_productivity_files_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(9,10,11)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NUMBER OF FIRMS', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################


tax_rate_files <- c("text/tax-rate_1.0_cat.txt","text/tax-rate_1.01_cat.txt",
                       "text/tax-rate_1.02_cat.txt")


data_tax_rate_files <- map_df(tax_rate_files, read_csv, skip = 6,
                                 show_col_types = FALSE) #imports the list of file names
data_tax_rate_files <- clean_names(data_tax_rate_files) # very useful, automatically cleans all names to easy R handling names
data_tax_rate_files <- data_tax_rate_files %>% 
  mutate(id = run_number * starting_seed,
         tax_rate_files_factor = as.factor(tax_rate))

data_tax_rate_files_plot <- ggplot(data = data_tax_rate_files, 
                                      aes(x = tax_rate, y = gdp_spending, fill = price_delta_files_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(1,1.01,1.02)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'NUMBER OF FIRMS', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

###############################################################################

#GRAPH


all__review_plot(bourgeoisie_ratio_plot
                 + theme(legend.position = "none"),
                 data_distribution_plot
                 + theme(legend.position = "none"),
                 data_firm_labor_plot
                 + theme(legend.position = "none"),
                 data_firm_prod_files_plot
                 + theme(legend.position = "none"),
                 data_gov_demand_files_plot
                 + theme(legend.position = "none"),
                 data_gov_initial_wealth_files_plot
                 + theme(legend.position = "none"),
                 data_industrial_switch_probability_files_plot
                 + theme(legend.position = "none"),
                 data_initial_household_wealth_filesplot
                 + theme(legend.position = "none"),
                 data_initial_service_price_files_plot
                 + theme(legend.position = "none"),
                 data_markup_files_plot
                 + theme(legend.position = "none"),
                 data_nobles_ratio_wealth_files_plot
                 + theme(legend.position = "none"),
                 data_number_of_bourgeoisie_files_plot
                 + theme(legend.position = "none"),
                 data_number_of_firms_files_plot
                 + theme(legend.position = "none"),
                 data_number_of_nobles_files_plot
                 + theme(legend.position = "none"),
                 number_of_workers_files_plot
                 + theme(legend.position = "none"),
                 data_price_delta_files_plot
                 + theme(legend.position = "none"),
                 data_safe_zone_files_plot
                 + theme(legend.position = "none"),
                 data_service_productivity_files_plot
                 + theme(legend.position = "none"),
                 data_tax_rate_files_plot
                 + theme(legend.position = "none"),
                 ncol=2, nrow=3, align = "hv")

all_review_plot


ggsave("review.pdf")
