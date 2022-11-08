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
  drop_na(Pulex.surv) #Adding this because at the moment all the samples haven't been processed. I need to check any NA's after all data has be input

##Exploratory analysis

###Plot
Pulexdf%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Pred)) +
  geom_point() +
  facet_wrap(~Species) +
  theme_classic()


##Check out the distribution
library(fitdistrplus)

descdist(Pulexdf$Pulex.surv)


#Closest to Beta or gamma distributions. I'll try a glm with gamma

#First a fixed-effects model


Pulexglm <- Pulexdf%>%
  glm(Pulex.surv ~ poly(TempC, 2)*Pred*Species, family = "poisson", data = .)

plot(Pulexglm)

library(car)

Anova(Pulexglm, type = 3)

#Mixed effects

library(lme4)
Pulexglmer <- Pulexdf%>%
  glmer(Pulex.surv ~ poly(TempC, 2) * Species * Pred + (1|Trial) + (1|Bath),
        family = "poisson", data = .)

plot(Pulexglmer)



Anova(Pulexglmer, type = 3)


#Generate predicted values and CI

##Trying bootpredictlme4 package

library(bootpredictlme4)

###Calculate the predicted values and CI by bootstrapping. Going with 10 simulations for now, but I'll need to increase that.

Pulexpredict <- predict(Pulexglmer, newdata = Pulexdf, re.form = NA, se.fit = T, nsim = 10, type = "response")

###Create a data frame out of the fit and CI lists. They'll need to be transposed to bind to Pulexdf


Pulexdf <- Pulexdf%>%
  mutate(fit = unname(data.frame(Pulexpredict$fit)$Pulexpredict.fit),
         lwr.CI = unname(t(data.frame(Pulexpredict$ci.fit))[,1]),
         upr.CI = unname(t(data.frame(Pulexpredict$ci.fit))[,2]))


#Plot

##Facet by comp

Pulexdf%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Pred), alpha = 0.5, color = NA) +
  facet_wrap(vars(Species)) +
  theme_classic()

##Facet by pred

Pulexdf%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Species)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Species), alpha = 0.5, color = NA) +
  facet_wrap(vars(Pred)) +
  theme_classic()

##Trying bootMer function


b <- bootMer(Pulexglmer, FUN = function(x)predict(x, newdata = Pulexdf, re.form = NA), nsim = 10)

library(merTools)

predictInterval(Pulexglmer, newdata = Pulexdf, which = "fixed", include.resid.var = F)


########################################

#Simo
Simodf <- df%>%
  filter(Species %in% c("Simo", "Comp"))%>%
  drop_na(Simo.surv) #Adding this because at the moment all the samples haven't been processed. I need to check any NA's after all data has be input

##Exploratory analysis
Simodf%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Pred)) +
  geom_point() +
  facet_wrap(~Species) +
  theme_classic()

Simodf%>%
  ggplot(aes(x = Simo.surv)) +
  geom_histogram()

##Check out the distribution

descdist(Simodf$Simo.surv)

#Mixed effects

library(lme4)
Simoglmer <- Simodf%>%
  glmer(Simo.surv ~ poly(TempC, 2) * Species * Pred + (1|Trial) + (1|Bath),
        family = "poisson", data = .)

plot(Simoglmer)

Anova(Simoglmer, type = 3)

#Generate predicted values and CI

##Trying bootpredictlme4 package


###Calculate the predicted values and CI by bootstrapping. Going with 10 simulations for now, but I'll need to increase that.

Simopredict <- predict(Simoglmer, newdata = Simodf, re.form = NA, se.fit = T, nsim = 10, type = "response")

###Create a data frame out of the fit and CI lists. They'll need to be transposed to bind to Pulexdf



Simodf <- Simodf%>%
  mutate(fit = unname(data.frame(Simopredict$fit)$Simopredict.fit),
         lwr.CI = unname(t(data.frame(Simopredict$ci.fit))[,1]),
         upr.CI = unname(t(data.frame(Simopredict$ci.fit))[,2]))



#Plot

##Facet by comp

Simodf%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Pred), alpha = 0.5, color = NA) +
  facet_wrap(vars(Species)) +
  theme_classic()

##Facet by pred

Simodf%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Species)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Species), alpha = 0.5, color = NA) +
  facet_wrap(vars(Pred)) +
  theme_classic()




