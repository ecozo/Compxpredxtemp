---
#'title: "comp pred temp analysis"
#'author: "Zoey Neale"
#'date: "2022-11-04"
#'output:
#'  html_document:
#'  keep_md: yes
---

#+{r
  library(tidyverse)

#+{r setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)




df <- read.csv("Data/comp_data.csv", header = T)%>%
  mutate_at(vars(Trial, Bath, Location, Species, Pred), as.factor) %>%
  mutate(Species = factor(Species, c("Pulex", "Simo", "Comp")))


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

###Histogram
Pulexdf%>%
  ggplot(aes(x = Pulex.surv)) +
  geom_histogram() +
  theme_classic() +
  labs(title = "Pulex data distribution") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

##Check out the distribution
library(fitdistrplus)

descdist(Pulexdf$Pulex.surv)


#Closest to Beta or gamma distributions. I'll try a glm with gamma

#First a fixed-effects model


Pulexglm <- Pulexdf%>%
  glm(Pulex.surv ~ poly(TempC, 2)*Pred*Species, family = "poisson", data = .)

plot(Pulexglm)

Anova(Pulexglm, type = 3)

#Checking for overdispersion

library(AER)
dispersiontest(Pulexglm)

#Yes it is overdispersed.

#For making plots I'm going to create standardized text sizes
Textsize <- theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  strip.text = element_text(size = 12))
#Plot facet by comp

Pulexdf%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = predict(Pulexglm, type = "response"))) +
  geom_ribbon(aes(ymax = (predict(Pulexglm, type = "response") + predict(Pulexglm, type = "response", se.fit = T)$se.fit),
                  ymin = (predict(Pulexglm, type = "response") - predict(Pulexglm, type = "response", se.fit = T)$se.fit),
                  fill = Pred,linetype = NA),  alpha = 0.5) +
  facet_wrap(vars(Species), labeller = labeller(Species = c("Pulex" = "No competitor", "Comp" = "With competitor"))) +
  theme_classic() +
  scale_fill_discrete(name = "Predator", labels = c( "No", "Yes"))+
  scale_color_discrete(name = "Predator", labels = c( "No", "Yes"))+
  labs(x = "Temperature (C)", y = "Number of survivors +/- SE",
       title = "Pulex poisson glm") +
  Textsize

#Plot facet by pred
Pulexdf%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Species)) +
  geom_point() +
  geom_line(aes(y = predict(Pulexglm, type = "response"))) +
  geom_ribbon(aes(ymax = (predict(Pulexglm, type = "response") + predict(Pulexglm, type = "response", se.fit = T)$se.fit),
                  ymin = (predict(Pulexglm, type = "response") - predict(Pulexglm, type = "response", se.fit = T)$se.fit),
                  fill = Species,linetype = NA),  alpha = 0.5, ) +
  facet_wrap(vars(Pred), labeller = labeller(Pred = c("N" = "No Predator", "Y" = "With Predator"))) +
  theme_classic() +
  scale_fill_discrete(name = "Competitor", labels = c( "No", "Yes"))+
  scale_color_discrete(name = "Competitor", labels = c( "No", "Yes"))+
  labs(x = "Temperature (C)", y = "Number of survivors +/- SE",
       title = "Pulex poisson glm") +
  Textsize

#GLM on log-transformed data

Pulexglm.log <- Pulexdf%>%
  lm(log(Pulex.surv + 0.0001) ~ poly(TempC, 2)*Pred*Species, data = .)

plot(Pulexglm.log)

#Residual plot is no good


#Trying negative binomial



Pulexglm.nb <- Pulexdf%>%
  glm.nb(Pulex.surv ~ poly(TempC, 2)*Pred*Species, data = .)


plot(Pulexglm.nb)


#For making plots I'm going to create standardized text sizes
Textsize <- theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  strip.text = element_text(size = 12))

#Plot facet by comp

Pulexdf%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = predict(Pulexglm.nb, type = "response"))) +
  geom_ribbon(aes(ymax = (predict(Pulexglm.nb, type = "response") + predict(Pulexglm.nb, type = "response", se.fit = T)$se.fit),
                     ymin = (predict(Pulexglm.nb, type = "response") - predict(Pulexglm.nb, type = "response", se.fit = T)$se.fit),
                  fill = Pred,linetype = NA),  alpha = 0.5) +
  facet_wrap(vars(Species), labeller = labeller(Species = c("Pulex" = "No competitor", "Comp" = "With competitor"))) +
  theme_classic() +
  scale_fill_discrete(name = "Predator", labels = c( "No", "Yes"))+
  scale_color_discrete(name = "Predator", labels = c( "No", "Yes"))+
  labs(x = "Temperature (C)", y = "Number of survivors +/- SE",
       title = "Pulex negative binomial glm") +
  Textsize

#Plot facet by pred
Pulexdf%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Species)) +
  geom_point() +
  geom_line(aes(y = predict(Pulexglm.nb, type = "response"))) +
  geom_ribbon(aes(ymax = (predict(Pulexglm.nb, type = "response") + predict(Pulexglm.nb, type = "response", se.fit = T)$se.fit),
                  ymin = (predict(Pulexglm.nb, type = "response") - predict(Pulexglm.nb, type = "response", se.fit = T)$se.fit),
                  fill = Species,linetype = NA),  alpha = 0.5, ) +
  facet_wrap(vars(Pred), labeller = labeller(Pred = c("N" = "No Predator", "Y" = "With Predator"))) +
  theme_classic() +
  scale_fill_discrete(name = "Competitor", labels = c( "No", "Yes"))+
  scale_color_discrete(name = "Competitor", labels = c( "No", "Yes"))+
  labs(x = "Temperature (C)", y = "Number of survivors +/- SE",
       title = "Pulex negative binomial glm") +
  Textsize

#######Mixed effects

library(lme4)

Pulexglmer <- Pulexdf%>%
  glmer(Pulex.surv ~ poly(TempC, 2) * Species * Pred + (1|Trial) + (1|Bath),
        family = "poisson", data = .)

plot(Pulexglmer)

#Checking for overdispersion

library(blmeco)

dispersion_glmer(Pulexglmer)

#Value is 8.9, which does indicate overdispersion. Trying another function for testing overdispersion following suggestions at https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(Pulexglmer)



#Trying a general model on log transformed data

Pulexlmerlog <- Pulexdf%>%
  lmer(log(Pulex.surv + 0.00001) ~ poly(TempC, 2) * Species * Pred + (1|Trial), data = .)

plot(Pulexlmerlog)

#Generate predicted values and CI

##Trying bootpredictlme4 package

library(bootpredictlme4)

###Calculate the predicted values and CI by bootstrapping. Going with 10 simulations for now, but I'll need to increase that.

Pulexpredict <- predict(Pulexglmer, newdata = Pulexdf, re.form = NA, se.fit = T, nsim = 10, type = "response")


###Create a data frame out of the fit and CI lists. They'll need to be transposed to bind to Pulexdf


Pulexdfglmer <- Pulexdf%>%
  mutate(fit = unname(data.frame(Pulexpredict$fit)$Pulexpredict.fit),
         lwr.CI = unname(t(data.frame(Pulexpredict$ci.fit))[,1]),
         upr.CI = unname(t(data.frame(Pulexpredict$ci.fit))[,2]))

write.csv(Pulexdf, "Data/Pulexdf.csv", row.names = F)


#Plot

##Facet by comp

Pulexdfglmer%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Pred), alpha = 0.5, color = NA) +
  facet_wrap(vars(Species)) +
  labs(title = "Poisson glmer") +
  theme_classic()

##Facet by pred

Pulexdfglmer%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Species)) +
  geom_point() +
  #stat_smooth(se=F, method="lm", formula=y~poly(x,2))+
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Species), alpha = 0.5, color = NA) +
  facet_wrap(vars(Pred)) +
  labs(title = "Poisson glmer") +
  theme_classic()

###Trying negative binomial glmer


Pulexglmer.nb <- Pulexdf%>%
  glmer.nb(Pulex.surv ~ poly(TempC, 2) * Species * Pred  + (1|Trial) + (1|Bath), data = .)

#Singular fit

plot(Pulexglmer.nb)

###Calculate the predicted values and CI by bootstrapping. Going with 10 simulations for now, but I'll need to increase that.

Pulexpredict.nb <- predict(Pulexglmer.nb, newdata = Pulexdf, re.form = NA, se.fit = T, nsim = 10, type = "response")


###Create a data frame out of the fit and CI lists. They'll need to be transposed to bind to Pulexdf


Pulexdfglmer.nb <- Pulexdf%>%
  mutate(fit = unname(data.frame(Pulexpredict.nb$fit)$Pulexpredict.nb.fit),
         lwr.CI = unname(t(data.frame(Pulexpredict.nb$ci.fit))[,1]),
         upr.CI = unname(t(data.frame(Pulexpredict.nb$ci.fit))[,2]))


#Plot

##Facet by comp

Pulexdfglmer.nb%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Pred), alpha = 0.5, color = NA) +
  facet_wrap(vars(Species)) +
  labs(title = "Pulex Negative binomial glmer") +
  theme_classic()

##Facet by pred

Pulexdfglmer.nb%>%
  ggplot(aes(x = TempC, y = Pulex.surv, color = Species)) +
  geom_point() +
  #stat_smooth(se=F, method="lm", formula=y~poly(x,2))+
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Species), alpha = 0.5, color = NA) +
  facet_wrap(vars(Pred)) +
  labs(title = "Pulex Negative binomial glmer") +
  theme_classic()









###############################

#Simo


Simodf <- df%>%
  filter(Species %in% c("Simo", "Comp"))%>%
  drop_na(Simo.surv) #Adding this because at the moment all the samples haven't been processed. I need to check any NA's after all data has be input

##Exploratory analysis

###Plot
Simodf%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Pred)) +
  geom_point() +
  facet_wrap(~Species) +
  theme_classic()

###Histogram
Simodf%>%
  ggplot(aes(x = Simo.surv)) +
  geom_histogram() +
  theme_classic() +
  labs(title = "Simo data distribution") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

##Check out the distribution
library(fitdistrplus)

descdist(Pulexdf$Pulex.surv)


#Closest to Beta or gamma distributions. I'll try a glm with gamma

#First a fixed-effects model


Simoglm <- Simodf%>%
  glm(Simo.surv ~ poly(TempC, 2)*Pred*Species, family = "poisson", data = .)

plot(Simoglm)

Anova(Simoglm, type = 3)

#Checking for overdispersion

library(AER)
dispersiontest(Simoglm)

#Yes it is overdispersed.

#For making plots I'm going to create standardized text sizes
Textsize <- theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  strip.text = element_text(size = 12))
#Plot facet by comp

Simodf%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = predict(Simoglm, type = "response"))) +
  geom_ribbon(aes(ymax = (predict(Simoglm, type = "response") + predict(Simoglm, type = "response", se.fit = T)$se.fit),
                  ymin = (predict(Simoglm, type = "response") - predict(Simoglm, type = "response", se.fit = T)$se.fit),
                  fill = Pred,linetype = NA),  alpha = 0.5) +
  facet_wrap(vars(Species), labeller = labeller(Species = c("Pulex" = "No competitor", "Comp" = "With competitor"))) +
  theme_classic() +
  scale_fill_discrete(name = "Predator", labels = c( "No", "Yes"))+
  scale_color_discrete(name = "Predator", labels = c( "No", "Yes"))+
  labs(x = "Temperature (C)", y = "Number of survivors +/- SE",
       title = "Simo poisson glm") +
  Textsize

#Plot facet by pred
Simodf%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Species)) +
  geom_point() +
  geom_line(aes(y = predict(Simoglm, type = "response"))) +
  geom_ribbon(aes(ymax = (predict(Simoglm, type = "response") + predict(Simoglm, type = "response", se.fit = T)$se.fit),
                  ymin = (predict(Simoglm, type = "response") - predict(Simoglm, type = "response", se.fit = T)$se.fit),
                  fill = Species,linetype = NA),  alpha = 0.5, ) +
  facet_wrap(vars(Pred), labeller = labeller(Pred = c("N" = "No Predator", "Y" = "With Predator"))) +
  theme_classic() +
  scale_fill_discrete(name = "Competitor", labels = c( "No", "Yes"))+
  scale_color_discrete(name = "Competitor", labels = c( "No", "Yes"))+
  labs(x = "Temperature (C)", y = "Number of survivors +/- SE",
       title = "Simo poisson glm") +
  Textsize

#GLM on log-transformed data

Simoglm.log <- Simodf%>%
  lm(log(Simo.surv + 0.0001) ~ poly(TempC, 2)*Pred*Species, data = .)

plot(Pulexglm.log)

#Residual plot is no good


#Trying negative binomial



Simoglm.nb <- Simodf%>%
  glm.nb(Simo.surv ~ poly(TempC, 2)*Pred*Species, data = .)


plot(Simoglm.nb)


#For making plots I'm going to create standardized text sizes
Textsize <- theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  strip.text = element_text(size = 12))

#Plot facet by comp

Simodf%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = predict(Simoglm.nb, type = "response"))) +
  geom_ribbon(aes(ymax = (predict(Simoglm.nb, type = "response") + predict(Simoglm.nb, type = "response", se.fit = T)$se.fit),
                  ymin = (predict(Simoglm.nb, type = "response") - predict(Simoglm.nb, type = "response", se.fit = T)$se.fit),
                  fill = Pred,linetype = NA),  alpha = 0.5) +
  facet_wrap(vars(Species), labeller = labeller(Species = c("Simo" = "No competitor", "Comp" = "With competitor"))) +
  theme_classic() +
  scale_fill_discrete(name = "Predator", labels = c( "No", "Yes"))+
  scale_color_discrete(name = "Predator", labels = c( "No", "Yes"))+
  labs(x = "Temperature (C)", y = "Number of survivors +/- SE",
       title = "Simo negative binomial glm") +
  Textsize

#Plot facet by pred
Simodf%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Species)) +
  geom_point() +
  geom_line(aes(y = predict(Simoglm.nb, type = "response"))) +
  geom_ribbon(aes(ymax = (predict(Simoglm.nb, type = "response") + predict(Simoglm.nb, type = "response", se.fit = T)$se.fit),
                  ymin = (predict(Simoglm.nb, type = "response") - predict(Simoglm.nb, type = "response", se.fit = T)$se.fit),
                  fill = Species,linetype = NA),  alpha = 0.5, ) +
  facet_wrap(vars(Pred), labeller = labeller(Pred = c("N" = "No Predator", "Y" = "With Predator"))) +
  theme_classic() +
  scale_fill_discrete(name = "Competitor", labels = c( "No", "Yes"))+
  scale_color_discrete(name = "Competitor", labels = c( "No", "Yes"))+
  labs(x = "Temperature (C)", y = "Number of survivors +/- SE",
       title = "Simo negative binomial glm") +
  Textsize

#######Mixed effects

library(lme4)

Simoglmer <- Simodf%>%
  glmer(Simo.surv ~ poly(TempC, 2) * Species * Pred + (1|Trial) + (1|Bath),
        family = "poisson", data = .)

plot(Simoglmer)


#Generate predicted values and CI

##Trying bootpredictlme4 package

library(bootpredictlme4)

###Calculate the predicted values and CI by bootstrapping. Going with 10 simulations for now, but I'll need to increase that.

Simopredict <- predict(Simoglmer, newdata = Simodf, re.form = NA, se.fit = T, nsim = 10, type = "response")


###Create a data frame out of the fit and CI lists. They'll need to be transposed to bind to Pulexdf


Simodfglmer <- Simodf%>%
  mutate(fit = unname(data.frame(Simopredict$fit)$Simopredict.fit),
         lwr.CI = unname(t(data.frame(Simopredict$ci.fit))[,1]),
         upr.CI = unname(t(data.frame(Simopredict$ci.fit))[,2]))

write.csv(Simodf, "Data/Simodf.csv", row.names = F)


#Plot

##Facet by comp

Simodfglmer%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Pred), alpha = 0.5, color = NA) +
  facet_wrap(vars(Species)) +
  labs(title = "Simo Poisson glmer") +
  theme_classic()

##Facet by pred

Simodfglmer%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Species)) +
  geom_point() +
  #stat_smooth(se=F, method="lm", formula=y~poly(x,2))+
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Species), alpha = 0.5, color = NA) +
  facet_wrap(vars(Pred)) +
  labs(title = "Simo Poisson glmer") +
  theme_classic()

#Trying a general model on log transformed data

Simolmerlog <- Simodf%>%
  lmer(log(Simo.surv + 0.00001) ~ poly(TempC, 2) * Species * Pred + (1|Trial), data = .)

plot(Simolmerlog)

###Trying negative binomial glmer


Simoglmer.nb <- Simodf%>%
  glmer.nb(Simo.surv ~ poly(TempC, 2) * Species * Pred  + (1|Trial) + (1|Bath), data = .)

#Singular fit

plot(Simoglmer.nb)

###Calculate the predicted values and CI by bootstrapping. Going with 10 simulations for now, but I'll need to increase that.

Simopredict.nb <- predict(Simoglmer.nb, newdata = Simodf, re.form = NA, se.fit = T, nsim = 10, type = "response")


###Create a data frame out of the fit and CI lists. They'll need to be transposed to bind to Pulexdf


Simodfglmer.nb <- Simodf%>%
  mutate(fit = unname(data.frame(Simopredict.nb$fit)$Simopredict.nb.fit),
         lwr.CI = unname(t(data.frame(Simopredict.nb$ci.fit))[,1]),
         upr.CI = unname(t(data.frame(Simopredict.nb$ci.fit))[,2]))


#Plot

##Facet by comp

Simodfglmer.nb%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Pred)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Pred), alpha = 0.5, color = NA) +
  facet_wrap(vars(Species)) +
  labs(title = "Simo Negative binomial glmer") +
  theme_classic()

##Facet by pred

Simodfglmer.nb%>%
  ggplot(aes(x = TempC, y = Simo.surv, color = Species)) +
  geom_point() +
  #stat_smooth(se=F, method="lm", formula=y~poly(x,2))+
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr.CI, ymax = upr.CI, fill= Species), alpha = 0.5, color = NA) +
  facet_wrap(vars(Pred)) +
  labs(title = "Simo Negative binomial glmer") +
  theme_classic()






