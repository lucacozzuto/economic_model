#!/usr/bin/env Rscript

##Argument parser:
#Create parser object
library("argparse")
library("tidyverse")
library("readr")
library("dplyr")
library("stringr")
library("reshape2")

parser <- ArgumentParser()

#Define desired outputs:
#GLOBAL FEATURES:
parser$add_argument("-input", "--Input_name", type="character", help="Input filenames.")
parser$add_argument("-output", "--Output_prefix", type="character", help="Output prefix for plots")
parser$add_argument("-params", "--Params_list", type="character", help="Comma separated param list")
parser$add_argument("-title", "--Output_title", type="character", help="Plot tile")



######### OUTPUT ANALYSIS ##########

#Get command line options, if help option encountered - print help and exit:
args <- parser$parse_args()

input_file <- args$Input_name
param_list<-args$Params_list
params <- str_split(param_list, ",", simplify = TRUE)
my_title <- args$Output_title
my_title_j<-gsub(" ", "_", my_title)
params_j<-gsub(" ", "_", params)

output_plot1 <- paste(args$Output_prefix, "_", my_title, ".pdf", sep="")



#input_file <- "initial-capital_500.0_cat.txt"
#param_list<-"mean price of farms,mean price of firms,mean price of bourgeoisie,mean salary of workers"
#params <- str_split(param_list,  ",", simplify = TRUE)
#my_title <- "Price averages"
#my_title_j<-gsub(" ", "_", my_title)
#params_j<-gsub(" ", "_", params)
#output_plot1 <- paste("initial-capital_500.0", "_", my_title_j, ".pdf", sep="")


outputs <- read_csv(input_file, skip = 6, show_col_types = FALSE)

#######
#GENERIC GRAPH

# generic variable analysis
calcStats <- function(varval, outputs) {
    varname<-sym(varval)

	variable_1 <- outputs %>%
 	 group_by(step) %>%
 	 summarise_at(vars(!!varname),list(variable_1 = mean))

	variable_1_shade <- outputs %>%
 	group_by(step) %>%
 	summarize(high_variable_1 = quantile(!!varname, probs = 0.975),
    low_variable_1 = quantile(!!varname, probs = 0.025))
    variable_1 <- merge(variable_1,variable_1_shade, by = "step")
	clean_name<-gsub(" ", "_", varval)
	names(variable_1)<-c("step", clean_name, paste0("high_", clean_name), paste0("low_", clean_name))

	return(variable_1)
}

### repeat this for n variables

mylist <- lapply(params, function(x) {
  df <- calcStats(x, outputs)
})

forgraph <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "step", all.x = TRUE),
        mylist)
        

forgraph2<-forgraph[, params_j]
row.names(forgraph2)<-forgraph[, c("step")]
## max 2 at a time, so repeat 
forgraph3<-melt(t(forgraph2))



graph <- ggplot(forgraph3, aes(x=Var2, y=value, group=Var1, color=Var1)) +
    geom_line() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) + theme_bw() +
    labs(x = 'Time', y = 'Value' ) + #changes the plot to a line
	ggtitle(my_title) + theme(plot.title = element_text(hjust = 0.5)) +
	theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

  
datalist = list()

for (i in params_j) {
	my_data <- forgraph[, c("step", i, paste0("high_", i), paste0("low_", i))]
	my_data$Var1 <- i
	names(my_data)<-c("Var2", "meanVal", "highVal", "lowVal", "Var1") 
	datalist[[i]] <- my_data
  }

big_data = do.call(rbind, datalist)
  
graph <- graph + geom_ribbon(data = big_data, aes(x=Var2, y=meanVal,ymin = lowVal, ymax = highVal), fill = "grey70", alpha=0.3, size = 0.1) 


pdf(output_plot1)
print(graph)
dev.off()
  
  
