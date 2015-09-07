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

