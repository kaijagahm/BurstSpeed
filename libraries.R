# Libraries to load
library(matrixStats)
library(tidyverse) # because of course
library(bootstrap)
library(pracma)
library(stringr) # for tadpole names
library(trajr) # to make trajectories from coordinates
library(data.table)
library(lubridate) 
library(smooth)
library(zoo)
library(magrittr) # for pipes
library(DAAG)
library(viridis) # for pretty plot colors
library(geomorph) # for geometric morphometrics
library(abind) # to bind together arrays of landmarks
source("https://raw.githubusercontent.com/kaijagahm/general/master/locate.nas.txt") # to find NA's in data
library(lme4) # for lmer models
library(GGally) # for ggpairs
library(lmerTest) # to get quasi-p-values on lmer models
library(MuMIn) # for dredge
library(car) # for the vif function
library(sjPlot) # to visualize effect sizes.
library(mgcv)
