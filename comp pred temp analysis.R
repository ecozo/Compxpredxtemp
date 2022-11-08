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
  glm(Pulex ~ poly(TempC, 2)*Pred*Species, family = "poisson", data = .)

plot(Pulexglm)

library(car)

Anova(Pulexglm, type = 3)









library(lme4)
Pulexglmer <- Pulexdf%>%
  glmer(Pulex ~ poly(TempC, 2) * Species * Pred + (1|Trial) + (1|Bath),
        family = "poisson", data = .)

plot(Pulexglmer)



Anova(Pulexglmer, type = 3)


#Generate predicted values and CI

##Trying bootpredictlme4 package

library(bootpredictlme4)

###Calculate the predicted values and CI by bootstrapping. Going with 10 simulations for now, but I'll need to increase that.

Pulexpredict <- predict(Pulexglmer, newdata = Pulexdf, re.form = NA, se.fit = T, nsim = 10, type = "response")

###Create a data frame out of the fit and CI lists. They'll need to be transposed to bind to Pulexdf

Pulexdf%>%
  mutate(fit = data.frame(Pulexpredict$fit))

Pulexpredict$ci.fit%>%
  data.frame()%>%
  pivot_longer(X1:X90)%>%
  view()

##Trying bootMer function


b <- bootMer(Pulexglmer, FUN = function(x)predict(x, newdata = Pulexdf, re.form = NA), nsim = 10)

library(merTools)

predictInterval(Pulexglmer, newdata = Pulexdf, which = "fixed", include.resid.var = F)

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





