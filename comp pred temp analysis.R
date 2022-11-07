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
#Pulex first

Pulexdf <- df%>%
  filter(Species %in% c("Pulex", "Comp"))%>%
  drop_na(Pulex) #Adding this because at the moment all the samples haven't been processed. I need to check any NA's after all data has be input

#Exploratory analysis
