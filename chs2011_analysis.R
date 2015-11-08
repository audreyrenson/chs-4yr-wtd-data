if (require(survey) == FALSE) 
  {
    install.packages("survey")
    library(survey)
  }
'if(require(sas7bdat) == FALSE)
{
  install.packages("sas7bdat")
}
library(survey)
library(sas7bdat)'

# 11/7/2015 checking for NA behavior
# SAS US born proportions = 54.38 / 45.62
# R proportions = 54.24 / 45.75
df_USborn_naomit <- na.omit(
			data.frame(
				cid = chs2011$cid,
				wt12_dual = chs2011$wt12_dual,
				strata = chs2011$strata,
				usborn = chs2011$usborn
				)
			)
dsn_USborn <- svydesign(
			ids = ~1,
			strata = ~strata,
			weights = ~wt12_dual,
			data = df_USborn_naomit
			)
svytable(
	~usborn,
	design = dsn_USborn,
	Ntotal = 1
	)

df_USborn_nainclude <- data.frame(
				cid = chs2011$cid,
				wt12_dual = chs2011$wt12_dual,
				strata = chs2011$strata,
				usborn = as.numeric(chs2011$usborn)
				)
df_USborn_nainclude[is.na(df_USborn_nainclude)] <- 1 # assign US born to NA's

dsn_nainclude <- svydesign(
			ids = ~1,
			strata = ~strata,
			weights = ~wt12_dual,
			data = df_USborn_nainclude
			)
svytable(
	~usborn,
	design = dsn_nainclude,
	Ntotal = 1
	)

# import SAS data set

load("chs2011.RData")
list# R doesn't read '.d' & '.r' values from SAS data. Encode them as 'NA'.
chs2011[is.na(chs2011)] <- NA
# When SAS data imported directly, required 2 times going through the 
# following code to recode new variables correctly -- some values that were  
# supposed to have been coded as 1 came up as NA, vice versa, 0 as NA, vice versa.
# After saving as an R object and then reloaded in a new session, this problem
# disappeared.

# code lgb as binary factor variable
# 1 = lgb / msm / wsw
# 0 = not lgb

chs2011$lgb[is.na(chs2011$sexualid)] <- NA
chs2011$lgb[chs2011$sexualid == 2 & !is.na(chs2011$sexualid)] <- 1
chs2011$lgb[chs2011$sexualid == 3 & !is.na(chs2011$sexualid)] <- 1
chs2011$lgb[chs2011$sexualid == 1 & !is.na(chs2011$sexualid)] <- 0
chs2011$lgb[chs2011$msm == 1 & !is.na(chs2011$msm)] <- 1
chs2011$lgb[chs2011$wsw == 1 & !is.na(chs2011$wsw)] <- 1
chs2011$lgb <- factor(chs2011$lgb, levels = c(0,1), labels = c("Non-LGB","LGB"))


# recode insured
# 0 = Yes
# 1 = No
chs2011$insuredbin <- chs2011$insured - 1
chs2011$insuredbin <- factor(chs2011$insuredbin, 
		levels = c(0,1), labels = c("Yes","No"))

# recode nocare = didntgetcare11
# Was there a time in the past 12 months that you needed medical care but did NOT get it?
# 1 = Yes
# 0 = No
chs2011$nocare <- chs2011$didntgetcare11
chs2011$nocare[chs2011$didntgetcare11==2] <- 0		# Recode no (2 -> 0)
chs2011$nocare <- factor(chs2011$nocare, 
		levels=c(0,1), labels=c("No","Yes"))

# recode povrec = povgroup3
# 1 = <200% FPL
# 2 = 200 - <400% FPL
# 3 = >400% FPL
chs2011$povrec <- chs2011$povgroup3
chs2011$povrec[chs2011$povgroup3 == 4] <- NA
chs2011$povrec <- factor(chs2011$povrec, levels=3:1, 
		labels=c(">400% FPL","200 - <400% FPL","<200% FPL"))

# recode healthrec = generalhealth
# 0 = Good, Very good, or Excellent
# 1 = Fair or Poor
chs2011$healthrec[chs2011$generalhealth < 4] <- 0
chs2011$healthrec[chs2011$generalhealth >= 4] <- 1
chs2011$healthrec <- factor(chs2011$healthrec,
		levels = 0:1, labels=c("Good, very good, or excellent","Fair or poor"))

# MENTAL HEALTH VARIABLES
# recode kessler variables (any psychological distress)
# 0 = None - minor
# 1 = Mild - severe
chs2011$kesslerbin <- 0
chs2011$kesslerbin[chs2011$k6_12m >= 13] <- 1
chs2011$kesslerbin[chs2011$k6_12m < 13] <- 0
chs2011$kesslerbin <- factor(chs2011$kesslerbin,
		levels=1:0, labels=c("Mild-severe","None-minor"))
summary(chs2011$kesslerbin)

# create binary variable for received MH treatment past yr
# 0 = No
# 1 = Yes
chs2011$mhtbin <- 0
chs2011$mhtbin[chs2011$pastyrmht == 1 & !is.na(chs2011$pastyrmht)] <- 0
chs2011$mhtbin[chs2011$pastyrmht >= 2 & !is.na(chs2011$pastyrmht)] <- 1
chs2011$mhtbin <- factor(chs2011$mhtbin, levels=0:1, labels=c("No","Yes"))
summary(chs2011$mhtbin)

#create composite variable for kesslerbin >= 13 but no MH trtmt
# 1 = Yes (psychological distress but no MH treatment)
# 0 = No (otherwise)
chs2011$nohm <- 0
chs2011$nohm <- as.numeric(chs2011$kesslerbin)
chs2011$nohm[chs2011$nohm==2] <- 0
chs2011$nohm[chs2011$mhtbin == 1] <- 0
chs2011$nohm <- factor(chs2011$nohm, levels=0:1, 
		labels=c("Any needed care received","Care needed, not received"))
summary(chs2011$nohm)

# sex
# 0 = Male
# 1 = Female
chs2011$sex <- chs2011$sex - 1
chs2011$sex <- factor(chs2011$sex, levels = 0:1, labels=c("Male","Female"))

# country of birth
# 1 = USA
# 2 = Foreign born
chs2011$usborn <- factor(chs2011$usborn, levels=1:2, labels=c("US born","Foreign born"))

# insurance status
# 1 = Private insurance
# 2 = Medicare
# 3 = Medicaid
# 4 = Other
# 5 = Uninsured
chs2011$insure5 <- factor(
			chs2011$insure5,
			levels = 1:5,
			labels =  c(
				"Private",
				"Medicare",
				"Medicaid",
				"Other",
				"Uninsured"
					)
				)

# age group
# 1 = 18-24
# 2 = 25-44
# 3 = 45-64
# 4 = 65+
chs2011$agegroup <- factor(
				chs2011$agegroup,
				levels = 1:4,
				labels = c(
					"18-24 yrs",
					"25-44 yrs",
					"45-64 yrs",
					"65+ yrs"
						)
					)
# newrace
# 1 = White
# 2 = Black
# 3 = Hispanic
# 4 = Asian / PI
# 5 = Other

chs2011$newrace <- factor(chs2011$newrace,
				levels = 1:5,
				labels = c("White non-Hispanic",
					"Black non-Hispanic",
					"Hispanic",
					"Asian PI",
					"Other"
					)
				)
# reorder factor variables to specify reference values for regression

# encode survey design object
chsdesign <- svydesign(id=~0,strata=chs2011$strata,weights=
                         chs2011$wt12_dual,data=chs2011)

# Table 1: Demographics
tbl_lgb <- svytable(~lgb,chsdesign, Ntotal = 1,exclude=NULL)
tbl_newrace <- svytable(~newrace,chsdesign, Ntotal=1,exclude=NULL)
tbl_sex <- svytable(~sex, chsdesign,Ntotal=1,exclude=NULL)
tbl_povrec <- svytable(~povrec,chsdesign,Ntotal=1,exclude=NULL)
tbl_usborn <- svytable(~usborn,chsdesign,Ntotal=1,exclude=NULL)
tbl_healthrec <- svytable(~healthrec,chsdesign,Ntotal=1,exclude=NULL)
tbl_insure5 <- svytable(~insure5,chsdesign,Ntotal=1,exclude=NULL)
tbl_agegroup <- svytable(~agegroup,chsdesign,Ntotal=1,exclude=NULL)
tbl_insured <- svytable(~insuredbin,chsdesign,Ntotal=1,exclude=NULL)

# create seed data frame 'svy' with strata & weight variables
	svy <- data.frame(cid = chs2011$cid, 
				strata = chs2011$strata,
				wt12_dual = chs2011$wt12_dual)


# crude regressions for physical health
  #nocare ~ lgb
   

	glm_lgb <- svyglm(nocare ~ lgb,
		design = chsdesign,
		family=quasibinomial
		)
	OR_lgb <- exp(
			coef(
				glm_lgb
				)
			)

  #nocare ~ insured

	glm_insured <- svyglm(
            nocare ~ insured,
            design = chsdesign,
		family=quasibinomial
		)
	OR_insured <- exp(
				coef(
					glm_insured
					)
				)
    
  #nocare ~ povrec

    	glm_nocare_povrec <- svyglm(
            nocare ~ povrec,
            design = chsdesign,
		family=quasibinomial
		)
	OR_povrec <- exp(coef(glm_nocare_povrec))

### BOOKMARK 11/7/2015 --- EVERYTHING WORKS UP TO THIS POINT ####

# nocare ~ newrace
 
    	df_newrace <- na.omit( 
		data.frame(svy,
            nocare = chs2011$nocare,
            newrace = chs2011$newrace) 
		)

	dsn_newrace <- svydesign(
		ids=~1,
		strata = ~strata,
		weights = ~wt12_dual,
		data = df_newrace
		)
    
    	glm_newrace <- svyglm(
            nocare ~ newrace,
            design = chsdesign,
		family=quasibinomial
		)
  #nocare ~ sex
    	df_sex <- na.omit( 
		data.frame(svy,
            nocare = chs2011$nocare,
            sex = chs2011$sex)
		)

	dsn_sex <- svydesign(
		ids=~1,
		strata = ~strata,
		weights = ~wt12_dual,
		data = df_sex
		)

    glm_nocare_sex <- svyglm(
            nocare ~ sex,
            design = dsn_sex,
		family=quasibinomial
		)

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