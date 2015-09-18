if (require(survey) == FALSE) 
  {
    install.packages("survey")
    library(survey)
  }
if(require(sas7bdat) == FALSE)
{
  install.packages("sas7bdat")
  library(sas7bdat)
}

if (!file.exists("chs2011.RData")) {
  # import SAS data set (not necessary if R object is already saved)
  chs2011 <- read.sas7bdat("chs2011_public.sas7bdat")
  # Save as R data object (not necessary if R object is already saved)
  save(chs2011,file="chs2011.RData")
}

# Load R object 
load("chs2011.RData")
#change NaN's to NA
chs2011[is.na(chs2011)] <- NA


# This section has issues -- I have to manually run some of the lines 2-3 times in order
# for it to encode properly. I changed the order, first coding the zeros and NA's,
# then coding 1's on top of them for sexualid, msm, and wsw.
# For some reason, if I don't include !is.na in the criteria for these,
# it ends up completely confused.

chs2011$sexualid[is.na(chs2011$sexualid)] <- NA
chs2011$lgb[chs2011$sexualid == 1 & !is.na(chs2011$sexualid)] <- 0
chs2011$lgb[chs2011$sexualid == 2 & !is.na(chs2011$sexualid)] <- 1
chs2011$lgb[chs2011$sexualid == 3 & !is.na(chs2011$sexualid)] <- 1
chs2011$lgb[chs2011$msm == 1 & !is.na(chs2011$msm)] <- 1
chs2011$lgb[chs2011$wsw == 1 & !is.na(chs2011$wsw)] <- 1

chs2011$lgb <- factor(chs2011$lgb)

#recode didntgetcare 0,1,NA
chs2011$nocare <- factor(chs2011$didntgetcare11-1)

#recode povgroup3 1=0,2=1,3=2,4=NA
chs2011$povrec <- factor(chs2011$povgroup3-1)
chs2011$povrec[chs2011$povrec==3] <- NA

#recode generalhealth
chs2011$healthrec[chs2011$generalhealth < 4 & !is.na(chs2011$generalhealth)] <- 0
chs2011$healthrec[chs2011$generalhealth >= 4 & !is.na(chs2011$generalhealth)] <- 1
chs2011$healthrec <- factor(chs2011$healthrec)

# MENTAL HEALTH VARIABLES
# recode kessler variables
chs2011$kesslerbin[chs2011$k6_12m >= 13] <- 0
chs2011$kesslerbin[chs2011$k6_12m < 13] <- 1
chs2011$kesslerbin <- factor(chs2011$kesslerbin)

#create binary variable for received MH treatment past yr
chs2011$mhtbin[chs2011$pastyrmht == 1 & !is.na(chs2011$pastyrmht)] <- 0
chs2011$mhtbin[chs2011$pastyrmht >= 2 & !is.na(chs2011$pastyrmht)] <- 1
chs2011$mhtbin <- factor(chs2011$mhtbin)

#create composite variable for kesslerbin >= 13 but no MH trtmt
chs2011$nohm[is.na(chs2011$mhtbin)] <- 0
chs2011$nohm[chs2011$kesslerbin == 0 & chs2011$mhtbin == 0 & !is.na(chs2011$mhtbin)] <- 1
chs2011$nohm <- factor(chs2011$nohm)
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