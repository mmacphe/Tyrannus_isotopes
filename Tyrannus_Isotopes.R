rm(list=ls())
require(tidyverse)

### Set source directory to the folder this file came from within RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Bring in the .csv data file
AOV_isotopes<-read.csv('./Tyrannus_savana_isotope_data.csv')
View(AOV_isotopes)

### Subset the data for just the T. s. savana subspecies
savana<-subset(AOV_isotopes, subspecies=="savana")

### Check if models that include sex explain more of the variation in isotope results
lm1sex<-lm(Nsig~Feather+IndividualID, data=savana)
lm2sex<-lm(Nsig~Feather*Sex+IndividualID, data=savana)

AICsex_output<-AIC(lm1sex,lm2sex)
AICsex_output<-AICsex_output %>% rename(dfsex = df, AICsex = AIC) #Rename AIC column
View(AICsex_output)

### Check if models that include age explain more of the variation in isotope results
lm1age<-lm(Nsig~Feather+IndividualID, data=savana)
lm2age<-lm(Nsig~Feather*Age+IndividualID, data=savana)

AICage_output<-AIC(lm1age,lm2age)
AICage_output<-AICage_output %>% rename(dfage = df, AICage = AIC) #Rename AIC column
View(AICage_output)

### Check if models that include year explain more of the variation in isotope results
lm1year<-lm(Nsig~Feather+IndividualID, data=savana)
lm2year<-lm(Nsig~Feather*Year+IndividualID, data=savana)

AICyear_output<-AIC(lm1year,lm2year)
AICyear_output<-AICyear_output %>% rename(dfyear = df, AICyear = AIC) #Rename AIC column
View(AICage_output)

### Write out output ###
write.csv(c(AICsex_output,AICage_output,AICyear_output), file='./Output Files/AIC_output.csv')

### Perform ANOVA on T. s. savana subspecies
fit_aov<-aov(Nsig~Feather+IndividualID, data=savana)

### Write out output ###
sink('./Output Files/ANOVA_summary.txt')
summary(fit_aov)
sink()

### Do Tukey's post hoc test to assess where differences were found ###
post_hoc<-TukeyHSD(fit_aov)

### Write out output ###
sink('./Output Files/TukeyHSD_summary.txt')
post_hoc
sink()

### Calculate mean square error and q-value ###
test2<-HSD.test(fit_aov,trt='Feather') #returns the MSerror and Df
test2

N<-length(savana$Nsig) #total sample size
k<-length(unique(savana$Feather)) #number of treatments
n<-length(savana$Nsig)/k #number of samples per group

q.value<-qtukey(p=0.95, nmeans=k, df=N-k)
q.value

### Compute Cohen's d - effect sizes ###
require(effsize)
P1savA<-subset(savana, Feather=="P1")
P1savB<-P1savA[-c(13),]

S6sav<-subset(savana, Feather=="S6") 
S6sav<-na.omit(S6sav$Nsig)

R3sav<-subset(savana, Feather=="R3")

P1S6d<-cohen.d(P1savB$Nsig,S6sav, paired=TRUE)
P1R3d<-cohen.d(P1savA$Nsig,R3sav$Nsig, paired=TRUE)

sink('./Output Files/CohensD_EffSize.txt')
Effsize<-c(P1S6d,P1R3d)
Effsize
sink()

### Make the boxplot
require(ggplot2)
require(plyr)
require(cowplot)

png(file='T_savana_isotopes.png', height=7, width=7, units="in", res=500)
jpeg(file="T_savana_isotopes.jpg", height=7, width=7, units="in", res=500)

neworder<-c("savana", "monachus")
neworderF<-c("P1", "S6", "R3")

AOV_isotopes2<-arrange(transform(AOV_isotopes, subspecies=factor(subspecies,levels=neworder)),subspecies)
AOV_isotopes2<-arrange(transform(AOV_isotopes2, Feather=factor(Feather,levels=neworderF)),Feather)
responsevariable.labs<-c("T. s. savana (n=21)", "T. s. monachus (n=6)")
names(responsevariable.labs)<- c("savana", "monachus")

col<-col<-c("black","black","black","gray40","gray40","gray40")

p<-ggplot(data=AOV_isotopes2, aes(x=Feather, y=Nsig)) + geom_boxplot(color=col) +
  geom_point(aes(fill=Feather, group=IndividualID), position=position_dodge(0.2), show.legend = FALSE) +
  geom_line(aes(group=IndividualID), position = position_dodge(0.2), color = "gray70", show.legend = FALSE) +
  geom_boxplot(alpha=0.5, lwd=0.8, color=col) + #alpha makes the boxplots transparent
  facet_wrap(~subspecies,labeller=labeller(subspecies= responsevariable.labs)) +
  panel_border() +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  labs(y= "dN‰")

draw_plot(p, x = 0, y = 0, width = 1, height = 1)
ggdraw() +  
  draw_plot(p,0,0,1,1) +
  draw_plot_label(c("A", "B"), c(0.063,0.53), c(0.99,0.99), size=15) +
  draw_plot_label(c("A", "B", "B"), c(0.14, 0.285, 0.43), c(0.95, 0.95, 0.95), size=9)

dev.off()
