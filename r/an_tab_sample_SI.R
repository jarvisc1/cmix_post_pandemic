

## Packages
library(data.table)
library(lubridate)
library(flextable)
library(gtsummary)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(wpp2022)
library(dplyr)
if(require(rstudioapi) && isAvailable()){
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
}

source('../r/00_setup_filepaths.R')

# Load data ---------------------------------------------------------------
dt <- qs::qread('../data/wrapup_part_cnts.qs')

dt <- dt[survey_round == 1000]

# Filter to needed vars ---------------------------------------------------
dt <- dt[, .(
  part_uid,
  ## Characteristics
 part_age_group, 
  sample_type,
  part_gender, 
  country,
  #area_2_name,
  # part_social_group,                                    
  # part_social_group_be,
  # part_social_group_score_be,                           
  # part_social_group1, 
  ## Extra
  part_employed
  )]


# Get percentages ---------------------------------------------------------
## Will remove unknown and not include in perc calculation
## Using the get function turns the varibale name to get
get_perc <- function(dt_, group_var_, adult = FALSE){
  top <- dt_[, .(num = .N), by = .(country, get(group_var_))]
  bottom <- dt_[!is.na(get(group_var_)) & !(get(group_var_) %in% c("Unknown", "Other", "remove")),
                .(denom = .N), by = country]
  x1 <- merge(top, bottom)
  x1[!(get %in% c("Unknown", "Other", "remove")), 
     text:= paste0(formatC(num, big.mark = ","), 
                   " (", 
                   formatC(num/denom*100, digits = 1, format = "f"),
                   "%)")]
  x1[get %in% c("Unknown", "Other"), 
     text:= formatC(num, big.mark = ",")]
  x1 <- x1[get != "remove"]
  x1
}



per_adult <- get_perc(dt, "sample_type")
per_age_child   <- get_perc(dt[sample_type == "child"], "part_age_group")
per_age_adult   <- get_perc(dt[sample_type == "adult"], "part_age_group")
per_age_pop<- get_perc(dt, "part_age_group")
# Download population data for 2023 from the wpp package for UK
# Load the data
data("popAge1dt")

name_countries<-c("United Kingdom","Belgium","Netherlands","Switzerland")
name_countries_in_survey<-c("UK","BE","NL","CH")
# Function to calculate sum for each combination of age_min and age_max
calculate_sum <- function(datatable,age_min, age_max) {
  total_sum<-datatable[age > age_min & age <= age_max, sum(pop)]
  return(total_sum)
}

plot_list <- list()
counter_plot<-1
## full population loop
for(name_country in name_countries){
  #name_country<-name_countries[1]
  name_country_in_survey<-name_countries_in_survey[which(name_countries==name_country)]
  this_perc_pop<-per_age_pop %>%filter(get!="Unknown") %>%filter(country==name_country_in_survey)
  
  # Subset the data for the country
  this_country_data <- subset(popAge1dt, name == name_country)
  this_country_data <- subset(this_country_data, year == "2021")
  
  age_min_values<-c(0,5,12,18,30,40,50,60,70)
  age_max_values<-c(4,11,17,29,39,49,59,69,100)
  
  # Apply the function to all combinations of age_min and age_max for each data frame
  total_sums <- mapply(calculate_sum, 
                       MoreArgs = list(data = this_country_data),
                       age_min = age_min_values, 
                       age_max = age_max_values)
  results_df <- data.frame(age_min = age_min_values,
                           age_max = age_max_values,
                           total_sums = total_sums)
  
  results_df_pop<-results_df
  results_df_pop$perc<-results_df_pop$total_sums/sum(results_df_pop$total_sums)*100
  
  # generate a new column that is the combination of age_min and age_max
  results_df_pop$age_group<-paste0(results_df_pop$age_min,"-",results_df_pop$age_max)
  # change age_group 70-100 into 70+
  results_df_pop$age_group[results_df_pop$age_group=="70-100"]<-"70+"
  # attach columns age_group and perc of results_df_adult to this_per_age_adult by matching column age_group with column get
  this_perc_pop<-merge(this_perc_pop,results_df_pop[,c("age_group","perc")],by.x="get",by.y="age_group",all.x=TRUE)
  #extract from column text the percentage between brackets
  this_perc_pop$perc_sample<-as.numeric(gsub(".*\\((.*)%\\).*","\\1",this_perc_pop$text))
  #generate a plot of comparison of perc and perc_sample, with two bars on top of each other with a different fill add a legend for the fill, with blue corresponding to the population and red to the sample
  this_plot<-ggplot(this_perc_pop, aes(x = get)) +
    geom_bar(aes(y = perc_sample, fill = "Sample"), stat = "identity", position = "dodge", width = 0.5, alpha = 0.8) +
    geom_bar(aes(y = perc, fill = "Population"), stat = "identity", position = "dodge", width = 0.5, alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Age group", y = "Percentage",title=paste0(name_country)) +
    scale_fill_manual(values = c("Sample" = "blue", "Population" = "red"), labels = c("Sample", "Population")) +
    guides(fill = guide_legend(title = "Source"))
    #
  
  # Add ggplot object to the list
  plot_list[[counter_plot]] <- this_plot
  counter_plot<-counter_plot+1
}

final_plot<-grid.arrange(grobs = plot_list, ncol = 2)
ggsave(final_plot,filename=paste0("../output/sample_vs_population.jpeg"),width=10,height=5)


