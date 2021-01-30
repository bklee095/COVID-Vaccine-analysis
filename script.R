rm(list=ls()) #removing preexisting variables
library(Hmisc) #import packages
library(magrittr)
library(ggplot2)
library(dplyr)
library(Rmisc)

VaccineData <- read.csv("~/COVID vaccine/country_vaccinations.csv") #import data
 View(VaccineData)

describe(VaccineData) #general overview
head(VaccineData)



argentina <- data.frame(VaccineData %>%
  filter(country == "Argentina"))

plot1 <- argentina %>%
  ggplot(aes(x = date, y = total_vaccinations)) +
  geom_point(color = "skyblue", size = 3) +
  xlab("Argentina") + 
  ggtitle("Total immunizations in Argentina") + 
  theme(axis.text.x = element_text(angle = 90))

plot2 <- argentina %>%
  ggplot(aes(x = date, y = daily_vaccinations)) +
  geom_point(color = "skyblue", size = 3) +
  xlab("Argentina") + 
  ggtitle("Daily vaccination count in Argentina") + 
  theme(axis.text.x = element_text(angle = 90))



USA <- data.frame(VaccineData %>%
        filter(country == "United States"))

plot3 <- USA %>%
  ggplot(aes(x = date, y = total_vaccinations)) +
  geom_point(color = "red", size = 3) +
  xlab("USA") + 
  ggtitle("Total immunizations in the USA") + 
  theme(axis.text.x = element_text(angle = 90))

plot4 <- USA %>%
  ggplot(aes(x = date, y = daily_vaccinations)) +
  geom_point(color = "red", size = 3) +
  xlab("Argentina") + 
  ggtitle("Daily vaccination count in the USA") + 
  theme(axis.text.x = element_text(angle = 90))

multiplot(plot1, plot3)
multiplot(plot2, plot4)


unique(VaccineData$vaccines) #checking types of vaccines

VaccineTypes <- 
  data.frame(count(VaccineData$vaccines))
head(VaccineTypes)

pie <-
  ggplot(VaccineTypes, aes(x = "", y = freq, fill = x)) + 
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0)

library(waffle)

VacType <- c("CNBG, Sinovac" = 44, "Covaxin, Covishield" = 13, "Covishield" = 2,
             "Covishield, Sinopharm" = 19, "Moderna, Pfizer/BioNTech" = 366, 
             "Oxford/AstraZeneca, Pfizer/BioNTech" = 215,
             "Pfizer/BioNTech" = 828, "Pfizer/BioNTech, Sinopharm" = 53, 
             "Pfizer/BioNTech, Sinopharm, Sputnik V" = 20,
             "Pfizer/BioNTech, Sinovac" = 9, "Sinovac" = 46, "Sputnik V" = 61)
VacType <- sort(VacType, decreasing = TRUE)

waffle <-
  waffle(parts = VacType/2, rows = 20, size = 1, title = "Proportion of COVID Vaccines",
  colors = c("#44D2AC", "#E48B8B", "#B67093", "#3A9ABD", "#CFE252", "#94022a",
             "#230aa5", "#6e166e", "#411351", "#1b9645", "#4d6509", "#ed94eb"))

