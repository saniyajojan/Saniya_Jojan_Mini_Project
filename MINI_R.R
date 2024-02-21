#Firstly clear your workspace and the r environment
rm(list = ls())

#Load package readx1 and Import the dataset into RStudio : The dataset has 294 obs initially
library(readxl)

#Here you need to speify where exactly ypu have saved the Raw dataset. 
MHC_DATA <- read_excel("C:/Users/saniy/Desktop/IMPERIAL MINI PROJECT/Playing around with data/DATA.xls")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#DATA cleaning
#The MHC similar or dissimilarity between male and female are determined by the MHC DR antigens so all females or males with MHC Dr unknown is removed from the dataset.
MHC <- subset(MHC_DATA, !MHC_DATA$HLA_DR_Rater...7 == "NA")
# all men whose identity was unknown is also removed, no missing identities for women
MHC <- subset(MHC, !MHC$DonorID == "NA")
# All men whose ages were unknown were removed from the dataset, all women's ages were known
MHC <- subset(MHC, !MHC$DonorID == "3" & !MHC$DonorID == "5" & !MHC$DonorID == "6" & !MHC$DonorID == "17" & !MHC$DonorID == "49")

#change the sexiness variable from a string to a numeric factor
MHC$Sexiness<-as.numeric(MHC$Sexiness)
MHC$Pill_User<- as.factor(MHC$Pill_User)
MHC$DonorID<- as.factor(MHC$MHC_Sharing)


#---------------------------------------------------------------------------------------------------------------------------------------------
#Basic exploration of data -> get a feeel for how the data is structured what you can do with it.

#This gives you a summary of the whole dataset
summary(MHC)

#Gives you a number of rows in the dataset
nrow(MHC) 

#Gives you all the uniques values in the specified column 
unique(MHC$Sexiness)
unique(MHC$MHC_Sharing)

#List the structure of the dataset
str(MHC)

# This returns the number of NA values in each variable of a dataset. 
colSums(is.na(MHC))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#MODEL BUILDING 
# Building linear regression models looking at the two categorical variables independantly.
# linear model with mhc sharing against sexiness
MHC_MOD1 <- lm(Sexiness~MHC_Sharing, data = MHC)

#Calls the summary output for the model
summary(MHC_MOD1)

#Gives the diagnostic plots for the model
par(mfrow=c(2,2)) 
plot(MHC_MOD1)
par(mfrow=c(1,1))

#linear model with pill user against sexiness
MHC_MOD2 <- lm(Sexiness~Pill_User, data = MHC)

summary(MHC_MOD2)

par(mfrow=c(2,2)) 
plot(MHC_MOD2)
par(mfrow=c(1,1))


#Linear model with two categorical variables and an interaction term
MHC_MOD3 <- lm(Sexiness~Pill_User + MHC_Sharing + Pill_User*MHC_Sharing, data = MHC)

summary(MHC_MOD3)

par(mfrow=c(2,2)) 
plot(MHC_MOD3)
par(mfrow=c(1,1))
# Another way to check the model assumptions. Dharma also carries tests to determine that the normality and homogeneity of variances are met.
hist(MHC$Sexiness)
library(DHARMa)
plot(simulateResiduals(MHC_MOD3))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#Visualising the data load the packages ggplot2, cowplot and dplyr.
library(ggplot2)
library(cowplot)
library(dplyr)

#Plotting raw data - boxplots
ggplot(MHC, aes(x=Pill_User, y=Sexiness, fill = MHC_Sharing))+
  stat_boxplot(geom='errorbar', linetype=1)+
  geom_boxplot() +
  scale_fill_manual(values = c("violetred1","grey"))

#Plotting the interaction effects
interaction.plot(x.factor = MHC$Pill_User, trace.factor = MHC$MHC_Sharing, 
                 response = MHC$Sexiness, fun = mean, 
                 type = "b", legend = FALSE, 
                 xlab = "MHC SHARING", ylab="SEXINESS",
                 pch=c(1,20),col = c("violetred1", "grey"),
                 lwd = 2,)
legend("right", c("disimilar","similar"), bty="n",lty=c(2,1),lwd=2,pch=c(1,20),
       col=c("violetred1", "grey"), title="MHC sharing", inset = .02, xpd = TRUE)



# making a figure on the same panel to include in the report
p1<- ggplot(MHC, aes(x=Pill_User, y=Sexiness, fill = MHC_Sharing))+
  stat_boxplot(geom='errorbar', linetype=1)+
  geom_boxplot() +
  ylab("Sexiness score")+
  xlab("Use of oral contraception: yes/no")+
  scale_fill_manual(values = c("violetred1","grey")) 
p1<- p1 + labs(fill = "MHC classification")

#Manually create the interaction plot for ggplot
p2 <-MHC %>% 
  ggplot() +
  aes(x = Pill_User, color = MHC_Sharing, group = MHC_Sharing, y = Sexiness) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  ylab("Sexiness score")+
  xlab("Use of oral contraception: yes/no")+
  scale_color_manual(values=c("violetred1", "grey"))
p2<-p2 + labs(color = "MHC classification")

#Using cowplot to place both plots side by side and give it labels.
plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)



