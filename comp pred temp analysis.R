---
#'title: "comp pred temp analysis"
#'author: "Zoey Neale"
#'date: "2022-11-04"
#'output:
#'  html_document:
#'  keep_md: yes
---

#+{r setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)


#+{r
library(tidyverse)


df <- read.csv("Data/comp_data.csv", header = T)%>%
  mutate_at(vars(Trial, Bath, Location, Species, Pred), as.factor)


#I'll analyze the simo and pulex data separately
#Pulex

Pulexdf <- df%>%
  filter(Species %in% c("Pulex", "Comp"))%>%
  drop_na(Pulex) #Adding this because at the moment all the samples haven't been processed. I need to check any NA's after all data has be input

##Exploratory analysis

###Plot
Pulexdf%>%
  ggplot(aes(x = TempC, y = Pulex, color = Pred)) +
  geom_point() +
  facet_wrap(~Species) +
  theme_classic()


##Check out the distribution
library(fitdistrplus)

descdist(Pulexdf$Pulex)


#Closest to Beta or gamma distributions. I'll try a glm with gamma

#First a fixed-effects model


Pulexglm <- Pulexdf%>%
  glm((Pulex + 0.0001) ~ TempC * Species * Pred, family = "Gamma", data = .)

plot(Pulexglm)

library(lme4)
Pulexdf%>%
  glmer((Pulex + 0.001) ~ TempC * Species * Pred + (1|Trial) + (1|Bath), family = "Gamma", data = .)


#Simo
Simodf <- df%>%
  filter(Species %in% c("Simo", "Comp"))%>%
  drop_na(Simo) #Adding this because at the moment all the samples haven't been processed. I need to check any NA's after all data has be input

##Exploratory analysis
Simodf%>%
  ggplot(aes(x = TempC, y = Simo, color = Pred)) +
  geom_point() +
  facet_wrap(~Species) +
  theme_classic()





