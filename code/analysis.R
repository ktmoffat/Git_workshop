#analysis of canopy data
#install.packages("ggplot2")
library(ggplot2)
library(nlme)

source("code/source_functions.R")

# Canopy cover and soil humidity analysis ####
  #import data 
    canopy <- read.csv("data/canopy_humidity.csv")
    head(canopy)
    
  #convert units
    canopy$canopy_perc_fixed <- correct_canopy(canopy$canopy_perc)
    canopy$soil_hum_prop <- convert_humidity(canopy$soil_hum_perc)
  
    head(canopy)
  
  #visualize data
    by_forest <- ggplot(aes(x=soil_hum_prop, y=canopy_perc_fixed), data=canopy)+
      geom_point(aes(colour=as.factor(site_id)))+
      facet_wrap(~forest_type)
    by_forest
  
  #basic analysis
    lm_simple <- lm(canopy_perc_fixed~soil_hum_prop, data=canopy)
    lm_simple
    
    lm_hum_forest <- lm(canopy_perc_fixed~soil_hum_prop+forest_type, data=canopy)
    lm_hum_forest
    anova(lm_hum_forest)

# Ant species richness and soil humidity analysis ####
    ant_richness <- read.csv("data/ant_richness.csv")
    ant_richness$soil_hum_prop <- convert_humidity(ant_richness$soil_hum_perc)
    
# Linear mixed effect model
    # first model to explain ant richness by humidity of the soil with a random effect of site
    simple_mod <- nlme::lme(ant_richness ~ soil_hum_prop,
                            random = ~1|site_id, data = ant_richness)
    summary(simple_mod)
    # trying to explain it by forest type instead
    simple_mod_for <- nlme::lme(ant_richness ~ as.factor(forest_type),
                            random = ~1|site_id, data = ant_richness)
    summary(simple_mod_for)
    
    # complete model now
    nlme_model <- nlme::lme(ant_richness ~ soil_hum_prop + as.factor(forest_type),
                           random = ~1|site_id, data = ant_richness)
  summary(nlme_model)

# Adding a simple plot
  ggplot(aes(x=ant_richness, y=soil_hum_prop), data = ant_richness) +
    geom_point(aes(colour=as.factor(site_id)))

ggplot(aes(x=ant_richness, y=soil_hum_prop), data = ant_richness) +
    geom_point(aes(colour=as.factor(site_id)))+
    geom_line(aes(group=site_id))
  
  
  ggplot(aes(y=ant_richness, x=site_id), data = ant_richness) +
    geom_point(aes(colour=forest_type))
  

# Adding another simple plot (says nothing)
  ggplot(aes(x=ant_richness, y=forest_type), data = ant_richness) +
    geom_point(aes(colour=as.factor(site_id)))

  