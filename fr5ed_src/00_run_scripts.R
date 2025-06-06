
source("01_data_prepare.R")
rm(list = ls())

source("02_analysis_country_contrast.R")
rm(list = ls())
source("02_analysis_single_estimate.R")
rm(list = ls())
source("02_analysis_two_countries.R")
rm(list = ls())

source("03_posterior_visualization_country_contrast.R")
rm(list = ls())
source("03_posterior_visualization_single_estimate.R")
rm(list = ls())
source("03_posterior_visualization_two_countries.R")
rm(list = ls())


#Load model with the contrast between countries for each estimate and without this contrast
load("posterior_samples_contrasts.RData")
load("posterior_samples_single.RData")

#Compare the two models
compare(m1,mC)

options(max.print=10000000)
precis(mC,depth=2)


tapply(dat$D,d$Country,mean)
tapply(d$donation,d$Country,mean)
tapply(d$donation,d$Country,sd)




