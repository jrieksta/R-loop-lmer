##########12022021#########
## Loop for extracting lmer results ##
#########Jolanta Rieksta###########

#############Load libraries################
.libPaths()
.libPaths("C:/R Packages")
Sys.setenv(LANGUAGE='en')
suppressPackageStartupMessages({
  library(dplyr)
  library(HistData)
  library(vegan)
  library(devtools)
  library(tidyverse)
  library(scales)
  library(ggplot2)
  library(LabApplStat)
  library(emmeans)
  library(multcompView)
  library(Rmisc)
  library(sciplot)
  library(ggpubr)
  library(lme4)
  library(nlme)
  library(wesanderson)
  library(multcomp)
  library(readxl)
  library(plyr)
  library(RColorBrewer)
  library(dplyr)
  library(car)
  library(e1071)
  library(lme4)
  library(bbmle)
  library(generics)
  library(caret)
  library(leaps)
  library(emmeans)
  library(devtools)
  library(LabApplStat)
  library(sjmisc)
  library(sjPlot)
  library(ggpubr)
  library(viridisLite)
  library(viridis)
  library(treemap)
  library(shiny)
  library(esquisse)
  library(remotes)
  library(ggpattern)
  library(readxl)
  
})
#################################################

#1
names(d1)
voc_group <-as.data.frame(d1[,c(42:50)]) #select columns of VOCs / voc groups interested
dim(voc_group)
for (i in 1:length(voc_group)) { 
  variable <- voc_group[,i]
  lme_cer <- lmer(variable ~ treatment*herbivory*Date+(1|Block/Plot),data=d1)
  print(summary(lme_cer)$coef)
}

#2
Response <-as.data.frame(d1[,c(42:50)]) #select columns of VOCs / voc groups interested
results <- vector("list", length(Response))
model<- formula(paste(Response[i],"~ treatment  * herbivory * Date + (1|Block/Plot)"))
lmer <- lmer(model)

Save_the_results{
  lmer <- lmer(model)
  results[[i]] <- summary(model)
}
{
  print(results[[i]])
  
  
  #END#
  