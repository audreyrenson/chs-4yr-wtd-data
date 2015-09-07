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

# import SAS data set
if(is.data.frame(chs2011) == FALSE)
{
  chs2011 <- read.sas7bdat("http://www.nyc.gov/html/doh/downloads/sas/episrv/chs2011_public.sas7bdat")
}
attach(chs2011)

# code lgb as binary variable
lgb <- integer(length(sexualid))
lgb[sexualid == 2] <- 1
lgb[sexualid == 3] <- 1
lgb[msm == 1] <- 1
lgb[wsw == 1] <- 1
lgb[sexualid == 1] <- 0
# R doesn't read '.d' & '.r' values from SAS data. Encode them as 'NA'.
lgb[sexualid == NaN] <- NA

#recode didntgetcare as binary
nocare <- integer(length(didntgetcare11))
nocare[didntgetcare11 == 2] <- 1
nocare[didntgetcare11 == 1] <- 0
nocare[didntgetcare11 == NaN] <- NA

#recode povgroup3
povrec <- povgroup3
povrec[povgroup3 == 4] <- NA
#recode generalhealth
healthrec <- generalhealth
healthrec[generalhealth < 4] <- 0
healthrec[generalhealth >= 4] <- 1

# MENTAL HEALTH VARIABLES
# recode kessler variables
kesslerbin = integer(length(k6_12m))
kesslerbin[k6_12m >= 13] <- 0
kesslerbin[k6_12m < 13] <- 1
#create binary variable for received MH treatment past yr
mhtbin <- integer(length(pastyrmht))
mhtbin[pastyrmht == 1] <- 0
mhtbin[pastyrmht <= 2] <- 1
#create composite variable for kesslerbin >= 13 but no MH trtmt
nohm <- integer(length(k6_12m))
nohm[kesslerbin == 0 & mhtbin == 0] <- 1

# add newly created variables to chs2011 dataframe
chs2011 <- data.frame(chs2011,lgb,nocare,povrec,healthrec,
                      kesslerbin,mhtbin,nohm)
# reorder factor variables to specify reference values for regression



# encode survey design factors
chsdesign <- svydesign(id=~1,strata=strata,weights=
                         wt12_dual,variables=chs2011,data=chs2011)
# crude regressions for physical health
nocare_lgb <- svyglm(nocare~lgb,design=chsdesign,family=quasibinomial)
nocare_insured <- svyglm(nocare~insured,design=chsdesign,family=quasibinomial)
nocare_povrec <- svyglm(nocare~povrec,design=chsdesign,family=quasibinomial)
nocare_newrace <- svyglm(nocare~newrace,design=chsdesign,family=quasibinomial)
nocare_sex <- svyglm(nocare~sex,design=chsdesign,family=quasibinomial)

  ## regression on healthrec with 1 and 0 as reference variable
  chs2011$healthrec <- factor(healthrec, levels=c("0","1"))
  nocare_healthrec_ref0 <- svyglm(nocare~healthrec,design=chsdesign,family=quasibinomial)
  chs2011$healthrec <- factor(healthrec, levels=c("1","0"))
  nocare_healthrec1_ref1 <- svyglm(nocare~healthrec,design=chsdesign,family=quasibinomial)

  ## regression on agegroup with 1 and 4 as reference variables
  chs2011$agegroup <- factor(agegroup, levels=c("1","2","3","4"))
  nocare_agegroup_ref1 <- svyglm(nocare~agegroup,design=chsdesign,family=quasibinomial)
  chs2011$agegroup <- factor(agegroup, levels=c("4","3","2","1"))
  nocare_agegroup_ref4 <- svyglm(nocare~agegroup,design=chsdesign,family=quasibinomial)
  
  ## regression on borough with "Manhattan" as reference variable (I think 3 is Manhattan)
  chs2011$borough <- factor(borough, levels=c("3","1","2","4","5"))
  nocare_borough <- svyglm(nocare~borough,design=chsdesign,family=quasibinomial)


detach(chs2011)