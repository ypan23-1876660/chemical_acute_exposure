#Clearing enviroments
rm(list=ls())

#Loading packages
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(gridExtra)

###Read in raw data
data = read.csv("/data/mackanholt_lab/yp/chemical_acute_screen/arsenic/17data_R.csv")

#Get variables
concentrations = unique(data[c("concentration")])

#change variables
conc1 = 0.8
conc2 = 1.5


#Filter data and calculate mean survival and std mean for plotting
df = data %>% 
  group_by(line, concentration, sex, exposure) %>% 
  summarise(mean_survival = mean(survival_proportion), sd= sd(survival_proportion), std_mean= sd/sqrt(length(rep)))

#Convert "Line" from numeric to factor for graphs 
df$line = as.factor(df$line)


#Run function
line_graph = function(data, sex, conc1) {

  
  #Filter data and calculate mean survival and std mean for plotting
  df = data %>% 
    group_by(line, concentration, sex, exposure) %>% 
    summarise(mean_survival = mean(survival_proportion), sd= sd(survival_proportion), std_mean= sd/sqrt(length(rep)))
  
  #Convert "Line" from numeric to factor for graphs 
  df$line = as.factor(df$line)
  
  #create graph title
  graph_title_conc = sub(" ","",paste(toString(conc1), 'mM'))
  graph_title = paste(toString(sex), graph_title_conc)

  
  #Get the first letter of sex. Then filter data based on sex and concentration
  sex1 = substr(sex, 1, 1)
  sex_conc1 = df %>% 
    filter(sex == sex1, concentration == conc1)
  
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
}

#Plug in variables and make graphs 
a = line_graph(data, "Female", conc1)
b = line_graph(data, "Female", conc2)
c = line_graph(data, "Male", conc1)
d = line_graph(data, "Male", conc2)

###combine all 4 plots all with its own legend
combined_plot = plot_grid(a, b, c, d, nrow=2)

max_exposure = 24
exp_24 = df %>% 
  filter(exposure == 24, sex == "F", concentration == conc1)

ggplot(data = exp_24, aes(x=line, y=mean_survival, color=line)) + 
  geom_boxplot()


#Create box plot with dots 
exp_24_survival_proportion = data %>% 
  filter(exposure == 24, sex == "F", concentration == conc1)
exp_24_survival_proportion$line = as.factor(exp_24_survival_proportion$line)

boxplot = ggplot(data = exp_24_survival_proportion, aes(x=line, y=survival_proportion, color=line)) +
  geom_boxplot() +
  geom_point() + 
  ylab("Mean Survival") + 
  labs(colour = "Fly Line") +
  ggtitle("Female, 24hr, 0.8mM") + 
  theme_classic() + theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size=15),
    plot.title = element_text(hjust = 0.5, size=25),
    legend.title = element_text(size=15),
    legend.text = element_text(size=12))

  