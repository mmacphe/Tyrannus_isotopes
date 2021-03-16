#Analyzing Stable Nitrogen Isotope Data for isotope paper
#Last updated November 22nd, 2020
rm(list=ls())

#bring in data file
library(readxl)
Tyrannus_isotope_data_for_aov <- read_excel("~/Downloads/Tyrannus_isotope_data_for_aov (1).xlsx")
View(Tyrannus_isotope_data_for_aov)

#Rename data
AOV_isotopes <-Tyrannus_isotope_data_for_aov

#Check if models that include sex explain more of the variation in isotope results (repeat for each feather: P1, S6, R3)
lm1<-lm(N_P1~ subspecies, data=AOV_isotopes) 
lm2<-lm(N_P1~ subspecies*Sex, data=AOV_isotopes)

AIC(lm1, lm2)
#    df       AIC
#lm1  
#lm2  

#Check if models that include age explain more of the variation in isotope results for each feather
lm1<-lm(N_R3~ subspecies, data=AOV_isotopes)
lm2<-lm(N_R3~ subspecies*Age, data=AOV_isotopes)

AIC(lm1, lm2)
#    df       AIC
#lm1  
#lm2  

#results can be found here: https://docs.google.com/spreadsheets/d/1DxWMgDnavC8xhzhhQeeENGoEN0-wmcODNAs6j_ZNpHQ/edit#gid=1987429752

##perform ANOVA:
fit_aov<-aov(delta_N~Feather*Subspecies+Individual, data=AOV_isotopes)
summary(fit_aov)

#Results returned below:
#                                             Df Sum Sq Mean Sq F value  Pr(>F)   
#AOV_isotopes$Subspecies                       1   3.56   3.563   5.033 0.02972 * 
#AOV_isotopes$Feather                          2   9.38   4.688   6.621 0.00297 **
#AOV_isotopes$Individual                      25  44.48   1.779   2.513 0.00332 **
#AOV_isotopes$Subspecies:AOV_isotopes$Feather  2   8.09   4.044   5.712 0.00609 **
#Residuals                                    46  32.57   0.708                   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#4 observations deleted due to missingness

##Do Tukey's post hoc test to assess where differences were found.
TukeyHSD(fit_aov)
#  Tukey multiple comparisons of means
#95% family-wise confidence level

#$`Feather:Subspecies`
#                               diff         lwr        upr     p adj
#R3:monachus-P1:monachus  0.47551002 -1.10663447  2.0576545 0.9461376
#S6:monachus-P1:monachus  0.74536734 -0.83677715  2.3275118 0.7263764
#P1:savana-P1:monachus    1.64137796  0.39655333  2.8862026 0.0037439
#R3:savana-P1:monachus    0.72816128 -0.51666334  1.9729859 0.5139431
#S6:savana-P1:monachus    0.45735706 -0.79343798  1.7081521 0.8839991
#S6:monachus-R3:monachus  0.26985732 -1.31228717  1.8520018 0.9956522
#P1:savana-R3:monachus    1.16586794 -0.07895669  2.4106926 0.0782025
#R3:savana-R3:monachus    0.25265127 -0.99217336  1.4974759 0.9902682
#S6:savana-R3:monachus   -0.01815296 -1.26894800  1.2326421 1.0000000
#P1:savana-S6:monachus    0.89601062 -0.34881401  2.1408352 0.2855974
#R3:savana-S6:monachus   -0.01720605 -1.26203068  1.2276186 1.0000000
#S6:savana-S6:monachus   -0.28801027 -1.53880532  0.9627848 0.9828031
#R3:savana-P1:savana     -0.91321667 -1.68522413 -0.1412092 0.0120024
#S6:savana-P1:savana     -1.18402089 -1.96561888 -0.4024229 0.0006161
#S6:savana-R3:savana     -0.27080422 -1.05240220  0.5107938 0.9053234

### to make figure describe the range in delta N for each feather
monachus<-subset(AOV_isotopes, Subspecies=="monachus")
View(monachus)

monachusP1<-subset(monachus, Feather=="P1")
monachusP1<-na.omit(monachusP1$delta_N)
View(monachusP1)
mean(monachusP1)
sd(monachusP1)

monachusS6<-subset(monachus, Feather=="S6")
monachusS6<-na.omit(monachusS6$delta_N)
mean(monachusS6)
sd(monachusS6)

monachusR3<-subset(monachus, Feather="R3")
monachusR3<-na.omit(monachusR3$delta_N)
mean(monachusR3)
sd(monachusR3)

#now describe the feather isotope data for each feather for savana subsp.
savana<-subset(AOV_isotopes, Subspecies=="savana")
savanaP1<-subset(savana, Feather=="P1")
savanaP1<-na.omit(savanaP1$delta_N)
mean(savanaP1)
sd(savanaP1)

savanaS6<-subset(savana, Feather=="S6")
savanaS6<-na.omit(savanaS6$delta_N)
mean(savanaS6)
sd(savanaS6)

savanaR3<-subset(savana, Feather=="R3")
savanaR3<-na.omit(savanaR3$delta_N)
mean(savanaR3)
sd(savanaR3)

#check whether each feather differed significantly between subspecies
boxplot(savanaP1, monachusP1)
P1t<-t.test(savanaP1, monachusP1)
P1t

S6t<-t.test(savanaS6, monachusS6)
S6t
boxplot(savanaS6, monachusS6)

R3t<-t.test(savanaR3, monachusR3)
R3t
boxplot(savanaR3, monachusR3)

#show the trend using boxplots in one figure for both subspecies
library(ggplot2)
monthsdata<-AOV_isotopes
png(file="~/Downloads/isotopes.png", height=7, width=7, units="in", res=500)

p <- ggplot(monthsdata, aes(x=monthsdata$`Month Number`, y=monthsdata$delta_N, color=monthsdata$Subspecies)) + 
  geom_point(aes(fill=monthsdata$`Month Number`, group=monthsdata$Individual), position=position_dodge(0.2), show.legend = FALSE) +
  #geom_jitter(aes(fill=monthsdata$`Month Number`, group = monthsdata$Individual)) +
  geom_line(aes(group=monthsdata$Individual), position = position_dodge(0.2), color = "grey", show.legend = FALSE) +
  geom_boxplot(alpha=0.5, lwd=1) + #alpha makes the boxplots transparent
  theme(legend.position = c(0.8,0.8))

p + xlab ("Feather Type") + ylab ("dN%%") + 
  scale_x_discrete(labels=c("month3" = "P1", "month4" = " ", "month5" = "S6", "month6" = " ", "month7.1" = "R3", "month7.2" = "P1", "month8" = "S6", "month9" = "R3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #theme(legend.position = c(0.9, 0.9)) +
  labs(colour="") +
  theme(text = element_text(size=20)) +
  #annotate("text", x=1, y=10, label = "P1") +
  annotate("text", x=3, y=11.5, label = "__", size = 8) +
  annotate("text", x=5, y=12, label = "__", size = 8) +
  annotate("text", x=6, y=9.5, label = "__", size = 8) +
  #annotate("text", x=7, y=9.3, label = "S6") +
  #annotate("text", x=8, y=8.8, label = "R3") +
  annotate("text", x=1, y=13, label = "*", size = 10)
dev.off()

