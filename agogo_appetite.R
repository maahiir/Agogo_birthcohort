library(mosaicCore)
library(lattice)
library(ggplot2)
library(factoextra)
library(car)
library(tab)
library(sjPlot)
library(texreg)
library(ggpubr)
library(terra)
library(data.table)
library(gapminder)
library(tidyquant)
library(plotly)
library(ARPALData)
library(shinyjs)
library(shiny)
library(tmaptools)
library(plotly)
library(incidence)
library(xts)
library(bigmemory)
library(plyr)
library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(dygraphs)
library(zoo)
library(heatmaply)
library(ggcorrplot)
library(openair)
library(gtsummary)
library(jtools)
library(huxtable)
library(flextable)
library(broom)
library(officer)
Sys.setenv("LANGUAGE"= "EN")

#load the appetite dataset

Agogo<- read.csv(file="Agogo.csv",header=TRUE,sep = ";",na.strings=c("NA","NaN", " "), dec = ",")
Agogo<-Agogo %>%mutate(pcr_fal_mo=na_if(pcr_fal_mo,"#NULL!")) #replace values with #NULL to NA
Agogo$pcr_fal_mo<-as.factor(Agogo$pcr_fal_mo) #convert the pcr_fal_mo variable to factor variable
Agogo<-subset(Agogo,is.na(pcr_fal_mo)==F) #remove observations with NA

Agogo$id<-as.character(Agogo$id)

mo_ed<-regression1[,c(1,41,43)] #get the mothers education variable from regression file for dietary pattern analysis
Agogo<-left_join(Agogo,mo_ed,by = c("id"="id")) #add the mothers education varialbe to appetite file
Agogo<-Agogo[,c(-19,-20)]
Agogo<-Agogo%>%mutate(bmi=bmi/100) #convert BMI to kg/m2

#Summarise the appetite score for boys and girls
Agogo%>%filter(Agogo$sex_2015==1)%>%summarise(quantile(Agogo$SumScoreAppetite,0.25))
Agogo%>%filter(Agogo$sex_2015==1)%>%summarise(quantile(Agogo$SumScoreAppetite,0.75))


Agogo%>%filter(Agogo$sex_2015==2)%>%summarise(quantile(Agogo$SumScoreAppetite,0.25))
Agogo%>%filter(Agogo$sex_2015==2)%>%summarise(quantile(Agogo$SumScoreAppetite,0.75))

IQR(Agogo$SumScoreAppetite)

median(Agogo$SumScoreAppetite)

#Create a binary variable for appetite score >18 (high appetite) <18 (low appetite)
Agogo$sumScore<-ifelse(Agogo$SumScoreAppetite>18,1,0)



#Crude association between in utero exposure to malaria and appetite score

cr1<-glm(sumScore~pcr_fal_mo, family = "binomial", data = Agogo)
print(summ(cr1, confint = T, ci.width = 0.95))
export_summs(adj,cr1,error_format = "95% CI [{conf.low}, {conf.high}]",to.file = "docx", file.name = "hux1.docx")
tbl_regression(cr1,exponentiate = T)

#Adjusted association between in utero exposure to malaria and appetite score

adj<-glm(sumScore~pcr_fal_mo + edu_m + occu_m  + mother_age + parity + hh_size, family = "binomial", data = Agogo)
print(summ(cr1, confint = T, ci.width = 0.95))
tbl_regression(adj,exponentiate = T)
table(Agogo$sumScore,useNA = "always")


#Crude and adjusted association between appetite scorea and cardiometabollic risk factors

#BMI
bmic<-lm(bmi~SumScoreAppetite, data = Agogo)
summary(bmic)
tbl_regression(bmic)
bmia<-lm(bmi~SumScoreAppetite + edu_m + occu_m  + mother_age + parity + hh_size, data = Agogo)
summary(bmia)
tbl_regression(bmia)

#FPG
fpgc<-lm(fpg~SumScoreAppetite, data = Agogo)
summary(fpgc)
tbl_regression(fpgc)
fpga<-lm(fpg~SumScoreAppetite + edu_m + occu_m  + mother_age + parity + hh_size, data = Agogo)
summary(fpga)
tbl_regression(fpga)

#Systolic blood pressure
bpc<-lm(systolic_bp~SumScoreAppetite, data = Agogo )
summary(bpc)
tbl_regression(bpc)
bpa<-lm(systolic_bp~SumScoreAppetite + edu_m + occu_m  + mother_age + parity + hh_size, data = Agogo)
summary(bpa)
tbl_regression(bpa)

#Diastolic blood pressure
dbpc<-lm(diastolic_bp~SumScoreAppetite, data = Agogo)
summary(dbpc)
tbl_regression(dbpc)
dbpa<-lm(diastolic_bp~SumScoreAppetite + edu_m + occu_m  + mother_age + parity + hh_size, data = Agogo)
summary(dbpa)
tbl_regression(dbpa)



