setwd("C:/Users/HP/OneDrive/Documents/R/trial")
library(readxl)
library(dplyr)

x = read_excel("cafpH.xlsx")
head(x) 

# Dropping the Staley soil series
x1 = subset(x, x$`Soil Series`!= "Staley")

#converting to factor variable
x2 <-x1 %>%
  mutate(Depth=as.factor(Depth),
         `Soil Series`= as.factor(`Soil Series`),
         Year = as.factor(Year),
         CropRotation = as.factor(CropRotation))
str(x2)

plot(x2)


library(nlme)
library(lme4)
library(lsmeans)
library(emmeans)
library(multcomp)
library(multcompView)
library(ggpubr)

model1 <- lmer(pH ~ Depth*Year*`Soil Series`*CropRotation + (1|`Soil Series`), data = x2)
anova(model1)
model2 <- lm(pH ~ Depth*Year*`Soil Series`*CropRotation, data = x2)
anova(model2)

anova(model1, model2) # model comparison




#calculate mean, sd, se and IC for Depth x Soil series interaction
my_sum<- x2 %>%
  group_by(Depth,`Soil Series`) %>%
  summarise(
    n =n(),
    mean = mean(pH),
    sd = sd(pH)
  ) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ic = se* qt ((1-0.05)/2 +.5, n-1))
my_sum


#calculate mean, sd, se and IC for Crop x Soil series interaction
my_sum1<- x2 %>%
  group_by(CropRotation,`Soil Series`) %>%
  summarise(
    n =n(),
    mean = mean(pH),
    sd = sd(pH)
  ) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ic = se* qt ((1-0.05)/2 +.5, n-1))
my_sum1

#calculate mean, sd, se and IC for Year x Depth interaction
my_sum2<- x2 %>%
  group_by(Year,Depth) %>%
  summarise(
    n =n(),
    mean = mean(pH),
    sd = sd(pH)
  ) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ic = se* qt ((1-0.05)/2 +.5, n-1))
my_sum2

#ggplot with standard error
#crop x soil series

c <- ggplot(my_sum1, aes(x= `Soil Series`, y =mean, fill= CropRotation))+
  geom_bar(stat="identity", color = "black", position = position_dodge()) +
  coord_cartesian(ylim=c(5.4,6.2))+ theme_bw()+
  geom_errorbar( aes(ymin=mean-se, ymax=mean+se), width=0.4, position = position_dodge(.9))

c1 <- c+ xlab("") + ylab ("Soil pH")
c2 <- c1 + theme(
  axis.text.x = element_text( color = "black", size = 12, face = "bold"),
  axis.text.y = element_text( color = "black", size = 12, face = "bold"),
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face ="bold"),
  panel.grid = element_blank(), #to remove the background grid
  panel.border = element_rect(colour = "black", fill=NA, size=1)) #to add frame to the graph



c3 <- c2 + theme(legend.position = c(0.73, 0.915),
                 legend.direction = "horizontal",
                 legend.title=element_blank(), 
                 legend.text=element_text(size=8, face = "bold"),
                 legend.key.size = unit(0.3, 'cm')) #change the size of the legend symbol

#Year x Depth
d <- ggplot(my_sum2, aes(x= Depth, y =mean, fill= Year))+
  geom_bar(stat="identity", color = "black", position = position_dodge()) +
  coord_cartesian(ylim=c(4.6,7.2))+ theme_bw()+
  geom_errorbar( aes(ymin=mean-se, ymax=mean+se), width=0.4, position = position_dodge(.9))

d1 <- d+ xlab(" Soil depth (cm)") + ylab ("Soil pH")
d2 <- d1 + theme(
  axis.text.x = element_text( color = "black", size = 12, face = "bold"),
  axis.text.y = element_text( color = "black", size = 12, face = "bold"),
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face ="bold"),
  panel.grid = element_blank(), #to remove the background grid
  panel.border = element_rect(colour = "black", fill=NA, size=1)) #to add frame to the graph



d3 <- d2 + theme(legend.position = c(0.29, 0.92), legend.direction = "horizontal",
                 legend.title = element_blank(),
                 legend.text=element_text(size=8,  face = "bold"),
                 legend.key.size = unit(0.3, 'cm')) #change the size of the legend symbol

#soil sereis x Depth

s <- ggplot(my_sum, aes(x= Depth, y =mean, fill= `Soil Series`))+
  geom_bar(stat="identity", color = "black", position = position_dodge()) +
 coord_cartesian(ylim=c(4.8,6.8))+ theme_bw()+
  geom_errorbar( aes(ymin=mean-se, ymax=mean+se), width=0.4, position = position_dodge(.9))

s1 <- s+ xlab(" Soil depth (cm)") + ylab ("Soil pH")
s2 <- s1 + theme(
  axis.text.x = element_text( color = "black", size = 12, face = "bold"),
  axis.text.y = element_text( color = "black", size = 12, face = "bold"),
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face ="bold"),
  panel.grid = element_blank(), #to remove the background grid
  panel.border = element_rect(colour = "black", fill=NA, size=1)) #to add frame to the graph
  
 

s3 <- s2 + theme(legend.position = c(0.36, 0.93), legend.direction = "horizontal",
                 legend.title=element_blank(), 
                 legend.text=element_text(size=8, face = "bold"),
                 legend.key.size = unit(0.3, 'cm')) #change the size of the legend symbol




soil_pH<- grid.arrange(c3, d3, s3, nrow = 2, ncol = 2)

#using cowplot creating grid
library(gridExtra)
require(cowplot)
grd <- plot_grid(c3, d3, s3, 
          labels = c('A', 'B', 'C'),
          ncol = 2, nrow = 2)

#saving the plot
ggsave("pH_interactions.tiff",soil_depth, units="in", width=7.3, height=6, dpi = 800)
ggsave("pH_interactions_labels.tiff",grd, units="in", width=7.3, height=6, dpi = 800)

