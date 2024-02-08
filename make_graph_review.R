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

#################################################################
initial_labor_price_files <- c("text/initial-labor-price_5.0_cat.txt","text/initial-labor-price_10.0_cat.txt",
                          "text/initial-labor-price_15.0_cat.txt")


initial_labor_price_files <- map_df(initial_labor_price_files, read_csv, skip = 6,
                                    show_col_types = FALSE) #imports the list of file names
initial_labor_price_files <- clean_names(initial_labor_price_files) # very useful, automatically cleans all names to easy R handling names
initial_labor_price_files <- initial_labor_price_files %>% mutate(id = run_number * starting_seed,
                                                                  initial_labor_price_factor = as.factor(initial_labor_price))

initial_labor_price_files_plot <- ggplot(data = initial_labor_price_files, aes(x = initial_labor_price, y = gdp_spending, fill = initial_labor_price_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(5,10,15)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'INITIAL LABOR PRICE', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#################################################################


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
