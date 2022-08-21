#Clearing enviroments
rm(list=ls())

#Loading packages
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(gridExtra)


######Read in raw data
data = read.csv("/data/mackanholt_lab/yp/local_chemical_acute_screen/arsenic/17data_R.csv")

#Format data: convert "Line" from numeric to factor for graphs 
data$line = as.factor(data$line)



######Functions for graphs

###Create line graph for across all exposures and lines for each sex and concentration

#Filter data and calculate mean survival and std mean for plotting line graph
df_line_graph = data %>% 
  group_by(line, concentration, sex, exposure) %>% 
  summarise(mean_survival = mean(survival_proportion), sd= sd(survival_proportion), std_mean= sd/sqrt(length(rep)))

#Create line graph
line_graph = function(data, sex, conc) {
  
  #Create graph title
  graph_title_conc = sub(" ","",paste(toString(conc), 'mM'))
  graph_title = paste(toString(sex), graph_title_conc)

  
  #Get the first letter of sex. Then filter data based on sex and concentration
  sex1 = substr(sex, 1, 1)
  sex_conc1 = data %>% 
    filter(sex == sex1, concentration == conc)
  
  #Making graph
  sex_conc1_plot = ggplot(data = sex_conc1,aes(x=exposure, y=mean_survival, color=line)) +
    geom_line() + 
    geom_point() +
    geom_errorbar(aes(ymin=mean_survival-std_mean, ymax=mean_survival+std_mean),
                  position=position_dodge(.9)) +
    scale_x_continuous("Exposure (hrs)", breaks = c(0,24,48)) + 
    ylab("Mean Survival") + 
    labs(colour = "Fly Line") +
    ggtitle(graph_title) + 
    theme_classic() + theme(
      axis.text = element_text(size = 20, color = "black"),
      axis.title = element_text(size=20),
      plot.title = element_text(hjust = 0.5, size=25),
      legend.title = element_text(size=15),
      legend.text = element_text(size=12))
  
  return(sex_conc1_plot)
}


###Create box plot with dots to visulize mean survival across all the lines 
###at one concentration, exposure, and sex at a time

#Create box plot functions
box_plot = function(data, sex, exp, conc) {
  
  #Create Graph Title
  graph_title_conc = sub(" ","",paste(toString(conc), 'mM'))
  graph_title_exp = sub(" ","",paste(toString(exp), 'hr'))
  graph_title = paste(toString(sex), graph_title_conc, graph_title_exp)
  
  
  #Filtering based on data
  df_boxplot = data %>% 
    filter(exposure == exp, sex == sex, concentration == conc)
  
  boxplot = ggplot(data = df_boxplot, aes(x=line, y=survival_proportion, color=line)) +
    geom_boxplot() +
    geom_point() + 
    ylab("Mean Survival") + 
    labs(colour = "Fly Line") +
    ggtitle(graph_title) + 
    theme_classic() + theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size=15),
      plot.title = element_text(hjust = 0.5, size=25),
      legend.title = element_text(size=15),
      legend.text = element_text(size=12))
  
  return(boxplot)
}


######Applying functions

###Update variables:
conc1 = 0.8
conc2 = 1.5
female = "Female"
male = "Male"

#Apply line graph. Input: df_line_graph, sex, concentrations
a = line_graph(df_line_graph, female, conc1)
b = line_graph(df_line_graph, female, conc2)
c = line_graph(df_line_graph, male, conc1)
d = line_graph(df_line_graph, male, conc2)

#Combine all 4 line plots all with each plot having its own legend
combined_plot = plot_grid(a, b, c, d, nrow=2)

#Apply Box plot. Input: data, sex, exposure and concentration with maximum variation
e = box_plot(data, female, 48, conc1)
f = box_plot(data, male, 24, conc1)
