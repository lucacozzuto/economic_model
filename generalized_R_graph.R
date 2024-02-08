#!/usr/bin/env Rscript

##Argument parser:
#Create parser object
library("argparse")
library(tidyverse)
library(readr)
library(dplyr)

parser <- ArgumentParser()

#Define desired outputs:
#GLOBAL FEATURES:
parser$add_argument("-input", "--Input_name", type="character", help="Input filenames.")
parser$add_argument("-output", "--Output_prefix", type="character", help="Output prefix for plots")
#parser$add_argument("-fasta", "--Fasta_file", type="character", help="Genome fasta file.")
#parser$add_argument("-ini_pos", "--Initial_position", type="integer", default=50, help="Initial position [default %(default)].")
#parser$add_argument("-fin_pos", "--Final_position", type="integer", help="Final position.")


######### OUTPUT ANALYSIS ##########

#Get command line options, if help option encountered - print help and exit:
args <- parser$parse_args()

input_file <- args$Input_name


output_plot1 <- paste(args$Output_prefix, "_1.pdf", sep="")
output_plot2 <- paste(args$Output_prefix, "_2.pdf", sep="")
output_plot3 <- paste(args$Output_prefix, "_3.pdf", sep="")


#varname for the different variables 

## importing data
#rm(list = ls())

outputs <- read_csv(input_file, 
                    #col_types = cols(X1 = col_number()), 
                    skip = 6,)


#######
#GENERIC GRAPH

# generic variable analysis
variable_1 <- outputs %>%
  group_by(step) %>%
  summarise_at(vars(`name of variable 1`),list(variable_1_mean = mean))

variable_1_shade <- outputs %>%
  group_by(step) %>%
  summarize(high_variable_1 = quantile(`name of variable 1`, probs = 0.975),
            low_variable_1 = quantile(`name of variable 1`, probs = 0.025))
variable_1 <- merge(variable_1,variable_1_shade, by = "step")

### repeat this for n variables

forgraph <- merge(variable_1,variable_2,by="step")
forgraph <- merge(forgraph,variable_3,by="step")
## max 2 at a time, so repeat 

graph <- ggplot(data = forgraph, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=variable_1,color="name of variable 1")) +
  geom_line(aes(y=variable_2, color="name of variable 2")) +
  geom_line(aes(y=variable_3, color="name of variable 3")) +
  geom_ribbon(data = forgraph,aes(x=step,y=variable_1_mean,ymin = low_variable_1, ymax = high_variable_1), alpha=0.1) +
  geom_ribbon(data = forgraph,aes(x=step,y=variable_2_mean,ymin = low_variable_2, ymax = high_variable_2), alpha=0.1) +
  geom_ribbon(data = forgraph,aes(x=step,y=variable_3_mean,ymin = low_variable_3, ymax = high_variable_3), alpha=0.1) +
  labs(x = 'Time', y = 'Value' ) + #changes the plot to a line
  ggtitle('Pick a name') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



