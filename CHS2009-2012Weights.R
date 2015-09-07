if (require(survey) == FALSE) 
{
  install.packages("survey")
}
if(require(sas7bdat) == FALSE)
{
  install.packages("sas7bdat")
}
library(survey)
library(sas7bdat)

# import 4-year weights SAS data set
chsWeights <- read.sas7bdat("/users/audreyrenson/google drive/NYC CHS Healtcare Access Project/Survey Weights 2009-2012/chs2009_2012combinedweights.sas7bdat")
attach(chsWeights)

setwd("/Users/audreyrenson/CHS-4-year-weighted-data/data-files/")
chs2009 <- read.sas7bdat("chs2009_public.sas7bdat")
chs2010 <- read.sas7bdat("chs2010_public.sas7bdat")
chs2011 <- read.sas7bdat("chs2011_public.sas7bdat")
chs2012 <- read.sas7bdat("chs2012_public.sas7bdat")



