#Firstly clear the workspace and r environment
rm(list = ls())

#Load package readx1 and Import the dataset : 294 obs intially
library(readxl)

MHC_DATA <- read_excel("C:/Users/saniy/Desktop/IMPERIAL MINI PROJECT/Playing around with data/DATA.xls")

#---------------------------------------------------------------------------------------------------------------------------------------------
#DATA cleaning, the MHC similar or disimlarity between male and female are determined by the MHC DR antigens so all females or males with MHC Dr unknown is removed from the dataset.
MHC <- subset(MHC_DATA, !MHC_DATA$HLA_DR_Rater...7 == "NA")
# all men whose identity was unknown is also removed, no missing identities for women
MHC <- subset(MHC, !MHC$DonorID == "NA")
# All men whose ages were unknow were removed from the dataset, all women's ages were known
unique(MHC$Sexiness)
MHC <- subset(MHC, !MHC$DonorID == "3" & !MHC$DonorID == "5" & !MHC$DonorID == "6" & !MHC$DonorID == "17" & !MHC$DonorID == "49")

#change the sexiness variable from a string to a numeric factor
MHC$Sexiness<-as.numeric(MHC$Sexiness)
MHC$Pill_User<- as.factor(MHC$Pill_User)
MHC$DonorID<- as.factor(MHC$MHC_Sharing)
#linear model with mhc sharing against sexiness
MHC_MOD1 <- lm(Sexiness~MHC_Sharing, data = MHC)

summary(MHC_MOD1)

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


hist(MHC$Sexiness)
library(DHARMa)
plot(simulateResiduals(MHC_MOD3))

#Visualising the data
library(ggplot2)
library(cowplot)

#Plotting raw data - boxplots
ggplot(MHC, aes(x=Pill_User, y=Sexiness, fill = MHC_Sharing))+
  stat_boxplot(geom='errorbar', linetype=1)+
  geom_boxplot() +
  scale_fill_manual(values = c("violetred1","grey"))

# Plotting interaction effects
interaction.plot(x.factor = MHC$Pill_User, trace.factor = MHC$MHC_Sharing, 
                 response = MHC$Sexiness, fun = mean, 
                 type = "b", legend = FALSE, 
                 xlab = "MHC SHARING", ylab="SEXINESS",
                 pch=c(1,20),col = c("violetred1", "grey"),
                 lwd = 2,)
legend("right", c("disimilar","similar"), bty="n",lty=c(2,1),lwd=2,pch=c(1,20),
       col=c("violetred1", "grey"), title="MHC sharing", inset = .02, xpd = TRUE)



# making a figure on the same panel
p1<- ggplot(MHC, aes(x=Pill_User, y=Sexiness, fill = MHC_Sharing))+
  stat_boxplot(geom='errorbar', linetype=1)+
  geom_boxplot() +
  ylab("Sexiness score")+
  xlab("Use of oral contraception: yes/no")+
  scale_fill_manual(values = c("violetred1","grey")) 
p1<- p1 + labs(fill = "MHC classification")

library(dplyr)

p2 <-MHC %>% 
  ggplot() +
  aes(x = Pill_User, color = MHC_Sharing, group = MHC_Sharing, y = Sexiness) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  ylab("Sexiness score")+
  xlab("Use of oral contraception: yes/no")+
  scale_color_manual(values=c("violetred1", "grey"))
p2<p2 + labs(color = "MHC classification")

#Making a panel
library(cowplot)
plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)




summary_result <- summary(MHC_MOD3)

# Extract coefficients table
coefficients_table <- summary_result$coefficients



require(broom) # for tidy()
require(knitr) # for kable()

out <- tidy(MHC_MOD3)
out

out<-kable(out)
out
