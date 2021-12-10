# Appendix 2. Data generation
# EQ-5D-3L Baseline Profile Generator for R
# Version 2.00 March 2019

# 1. load libraries
library(readr)
# 2. Set up a working diretory
setwd("<<directory path>>")

# 3. Load the function profGen.f to generate baseline profiles for the 3L version
profGen.f <<-function(cases){
  dim.name <- c("Mobility","Self Care","Usual Act","Pain & Dis","Anx & Dep","Profile")
  baseProf.m <<- matrix(0,nrow=cases, ncol=6)
  colnames(baseProf.m) <<- dim.name
  for (v1 in 1:cases) {
    prof=0
    for (v2 in 1:5) {
      P=runif(1)
      if(P <= prob.m[v2,1]) {
        baseProf.m[v1,v2] <<- 1
      }else{
        if(P <=(prob.m[v2,1]+prob.m[v2,2])) {
          baseProf.m[v1,v2] <<- 2
        }else { baseProf.m[v1,v2] <<- 3 }
      }
      prof=prof+(baseProf.m[v1,v2]*(10^(5-v2)))
    }
    baseProf.m[v1,6] <<- prof
  }
  cat("\n","First 10 profiles","\n")
  print(baseProf.m[1:10,1:6])
}
#*******************************************************************************
## DISEASE (d) [i.e., DEPRESSION, LOW BACK PAIN, OSTEOARTHRITIS, CANCER]

## BASELINE PROFILE – MILD
## Import the disease baseline probability matrix control group
prob.m <- read.csv("d_bpm_ml_t0.csv", row.names=1)
profGen.f(150) # generate baseline profiles
write.csv(baseProf.m, "d_bp_ml_t0.csv") # save baseline profiles
## Import the disease baseline probability matrix treatment group
prob.m <- read.csv("d_bpm_ml_t1.csv", row.names=1)
profGen.f(150)
write.csv(baseProf.m, "d_bp_ml_t1.csv") 

## BASELINE PROFILE – MODERATE 
## Import the disease baseline probability matrix control group
prob.m <- read.csv("d_bpm_mo_t0.csv", row.names=1)
profGen.f(150)
write.csv(baseProf.m, "d_bp_mo_t0.csv")
## Import the disease baseline probability matrix treatment group 
prob.m <- read.csv("d_bpm_mo_t1.csv", row.names=1)
profGen.f(150)
write.csv(baseProf.m, "d_bp_mo_t1.csv") 

## BASELINE PROFILE – SEVERE 
## Import the disease baseline probability matrix control group
prob.m <- read.csv("d_bpm_se_t0.csv", row.names=1)
profGen.f(150)
write.csv(baseProf.m, "d_bp_se_t0.csv") 
## Import the disease baseline probability matrix treatment group 
prob.m <- read.csv("d_bpm_se_t1.csv", row.names=1)
profGen.f(150)
write.csv(baseProf.m, "d_bp_se_t1.csv")

################################################################################

# Package: EQSimLab3L
# Title: EQ-5D-3L Simulation Laboratory
# Version: 0.0.0.9000

# 1. Load libraries
library(readr)
library(eq5d)
library(foreign)

# 2. Set up a working diretory
setwd("<<directory path>>")

# 3. generate follow-up profiles by treatment group
## TREATMENT GOUP
eq_make_profile_change()
# Do you want to import probabilities from a data file? (y = yes) > y
# File Name? > 0-tp-mld3l-t1.txt
# # Base profile data file name ? > d_bp_mlt1.txt
# Matched outcome profile data file name ? > d_fp_mlt1.txt

## CONTROL GROUP - the same transition probabilities as mild depression small effect size
eq_make_profile_change()
# Do you want to import probabilities from a data file? (y = yes) > y
# File Name? > 0-tp-mld3l-t0.txt
# Base profile data file name ? > d_bp_mlt0.txt
# Matched outcome profile data file name ? > d_fp_mlt0.txt

# 4. Calculate baseline and follow-up utilities
# EQ-5D-5L country-specific value sets must be one of: Canada, China, Denmark, Egypt, England, 
# Ethiopia, France, Germany, HongKong, Hungary, Indonesia, Ireland, Japan, Malaysia, Netherlands, 
# Peru_cTTO, Peru_DCE, Poland, Portugal, SouthKorea, Spain, Sweden, Taiwan, Thailand, Uruguay, 
# USA, Vietnam

# 4.1 baseline utilities control group
`baseProf.m.t0` <- read.csv("~/2020-EuroQol-call/EQ5D_laboratory/probabilities/d_bp_mlt0.txt", sep="")
names(baseProf.m.t0)[1] <- "id"
names(baseProf.m.t0)[2] <- "MO"
names(baseProf.m.t0)[3] <- "SC"
names(baseProf.m.t0)[4] <- "UA"
names(baseProf.m.t0)[5] <- "PD"
names(baseProf.m.t0)[6] <- "AD"
names(baseProf.m.t0)[7] <- "Profile.b"
baseProf.m.t0$trt <- 0

baseProf.m.t0$NL.utility.b <- eq5d(baseProf.m.t0, type="TTO", version="3L", country = "Netherlands", ignore.incomplete = TRUE)
baseProf.m.t0$US.utility.b <- eq5d(baseProf.m.t0, type="TTO", version="3L", country = "USA", ignore.incomplete = TRUE)
baseProf.m.t0$JP.utility.b <- eq5d(baseProf.m.t0, type="TTO", version="3L", country = "Japan", ignore.incomplete = TRUE)

# 4.2 follow-up utilities control group
`outProf.m.t0` <- read.csv("~/2020-EuroQol-call/EQ5D_laboratory/probabilities/d_fp_mlt0.txt", sep="")
names(outProf.m.t0)[1] <- "id"
names(outProf.m.t0)[2] <- "MO"
names(outProf.m.t0)[3] <- "SC"
names(outProf.m.t0)[4] <- "UA"
names(outProf.m.t0)[5] <- "PD"
names(outProf.m.t0)[6] <- "AD"
names(outProf.m.t0)[7] <- "Profile.f"
outProf.m.t0$trt <- 0
outProf.m.t0 <- data.frame(lapply(outProf.m.t0, function(x) as.numeric(as.character(x))))

outProf.m.t0$NL.utility.f <- eq5d(outProf.m.t0, type="TTO", version="3L", country = "Netherlands", ignore.incomplete = TRUE)
outProf.m.t0$US.utility.f <- eq5d(outProf.m.t0, type="TTO", version="3L", country = "USA", ignore.incomplete = TRUE)
outProf.m.t0$JP.utility.f <- eq5d(outProf.m.t0, type="TTO", version="3L", country = "Japan", ignore.incomplete = TRUE)

# 4.3. Merge baseline and follow-up data control group
control <- merge(baseProf.m.t0, outProf.m.t0, by = "id")

# 4.4 baseline utilities treatment group
`baseProf.m.t1` <- read.csv("~/2020-EuroQol-call/EQ5D_laboratory/probabilities/dep_bp_mlt1.txt", sep="")
names(baseProf.m.t1)[1] <- "id"
names(baseProf.m.t1)[2] <- "MO"
names(baseProf.m.t1)[3] <- "SC"
names(baseProf.m.t1)[4] <- "UA"
names(baseProf.m.t1)[5] <- "PD"
names(baseProf.m.t1)[6] <- "AD"
names(baseProf.m.t1)[7] <- "Profile.b"
baseProf.m.t1$trt <- 1

baseProf.m.t1$NL.utility.b <- eq5d(baseProf.m.t1, type="TTO", version="3L", country = "Netherlands", ignore.incomplete = TRUE)
baseProf.m.t1$US.utility.b <- eq5d(baseProf.m.t1, type="TTO", version="3L", country = "USA", ignore.incomplete = TRUE)
baseProf.m.t1$JP.utility.b <- eq5d(baseProf.m.t1, type="TTO", version="3L", country = "Japan", ignore.incomplete = TRUE)

# 4.5 follow-up utilities treatment group
`outProf.m.t1` <- read.csv("~/2020-EuroQol-call/EQ5D_laboratory/probabilities/dep_fp_mlt1.txt", sep="")
names(outProf.m.t1)[1] <- "id"
names(outProf.m.t1)[2] <- "MO"
names(outProf.m.t1)[3] <- "SC"
names(outProf.m.t1)[4] <- "UA"
names(outProf.m.t1)[5] <- "PD"
names(outProf.m.t1)[6] <- "AD"
names(outProf.m.t1)[7] <- "Profile.f"
outProf.m.t1$trt <- 0
outProf.m.t1 <- data.frame(lapply(outProf.m.t1, function(x) as.numeric(as.character(x))))

outProf.m.t1$NL.utility.f <- eq5d(outProf.m.t1, type="TTO", version="3L", country = "Netherlands", ignore.incomplete = TRUE)
outProf.m.t1$US.utility.f <- eq5d(outProf.m.t1, type="TTO", version="3L", country = "USA", ignore.incomplete = TRUE)
outProf.m.t1$JP.utility.f <- eq5d(outProf.m.t1, type="TTO", version="3L", country = "Japan", ignore.incomplete = TRUE)

# 4.6. Merge baseline and follow-up data treatment group
treatment <- merge(baseProf.m.t1, outProf.m.t1, by = "id")

# 5. Merge treatment and control
small.eff.size <- rbind(treatment, control)

# 6. Calculate cohen's d = mean difference between groups/ sd

small.eff.size$NL.QALY <- (0.5*(small.eff.size$NL.utility.b + small.eff.size$NL.utility.f))
small.eff.size$US.QALY <- (0.5*(small.eff.size$US.utility.b + small.eff.size$US.utility.f))
small.eff.size$JP.QALY <- (0.5*(small.eff.size$US.utility.b + small.eff.size$JP.utility.f))

# 7. Prepare data to save in .dta
colnames(small.eff.size)[colnames(small.eff.size)=="trt.x"] <- "trt"
colnames(small.eff.size)[colnames(small.eff.size)=="MO.x"] <- "MO_BASELINE"
colnames(small.eff.size)[colnames(small.eff.size)=="SC.x"] <- "SC_BASELINE"
colnames(small.eff.size)[colnames(small.eff.size)=="UA.x"] <- "UA_BASELINE"
colnames(small.eff.size)[colnames(small.eff.size)=="PD.x"] <- "PD_BASELINE"
colnames(small.eff.size)[colnames(small.eff.size)=="AD.x"] <- "AD_BASELINE"

colnames(small.eff.size)[colnames(small.eff.size)=="MO.y"] <- "MO_T1"
colnames(small.eff.size)[colnames(small.eff.size)=="SC.y"] <- "SC_T1"
colnames(small.eff.size)[colnames(small.eff.size)=="UA.y"] <- "UA_T1"
colnames(small.eff.size)[colnames(small.eff.size)=="PD.y"] <- "PD_T1"
colnames(small.eff.size)[colnames(small.eff.size)=="AD.y"] <- "AD_T1"
small.eff.size$trt.y <- NULL

# 8. Check effect size NL, US, JP
NL.lm <-lm(NL.QALY ~ trt, data = small.eff.size)
summary(NL.lm)
sd.NL <- sd(small.eff.size$NL.QALY)
cohen.d.NL <- NL.lm[["coefficients"]][["trt"]]/sd.NL
cohen.d.NL

US.lm <-lm(US.QALY ~ trt, data = small.eff.size)
summary(US.lm)
sd.US <- sd(small.eff.size$US.QALY)
cohen.d.US <-US.lm[["coefficients"]][["trt"]]/sd.US
cohen.d.US

JP.lm <-lm(JP.QALY ~ trt, data = small.eff.size)
summary(JP.lm)
sd.JP <- sd(small.eff.size$JP.QALY)
cohen.d.JP <- JP.lm[["coefficients"]][["trt"]]/sd.JP
cohen.d.JP

small.eff.size <<- cbind(Case =c(1:nrow(small.eff.size)),small.eff.size)
small.eff.size$id <- NULL
colnames(small.eff.size)[colnames(small.eff.size)=="Case"] <- "id"

write.dta(small.eff.size, file = "<<directory path>>/ml-small-effsize.dta")

################################################################################

# Generate age, gender, and costs
# 1. Load libraries
library(haven)
library(simstudy)
library(foreign)

# 2. Import EQ-5D dataset and prepare to merge with simulated dataset
setwd("<<directory path>>")
dataset <- read_dta("ml-small-effsize.dta") # replace the name of EQ-5D dataset here

# 3. Generate baseline characteristics
def <- defData(varname = "age", dist="uniformInt", formula="25;75", id="id")
def <- defData(def, varname = "gender", formula = 0.19, dist = "binary", id="id")
simulatie <- genData(300, def)

# 4. Merge baseline characteristics and EQ-5D dataset
simulatie <- merge(simulatie,dataset, by ="id")

# 5. Generate correlated costs and QALYs
def1 <- defDataAdd(varname = "costs", formula = "2000 + 250*trt", variance = 1, dist = "gamma")
simulatie <- addColumns(def1, simulatie)
simulatie <- addCorFlex(simulatie, def1, rho = 0.75, corstr = "cs")

# 6. Check correlation
correlation <- simulatie[,cor(NL_QALY, i.costs)]
correlation

# 7. Save data in .dta
write.dta(simulatie, file = "<<directory path>>/ml-small-effsize.dta")

################################################################################

# Package: EQSimLab5L
# Title: EQ-5D-5L Simulation Laboratory
# Version: 0.0.0.9000

# 1. Load libraries
library(EQSimLab5L)
library(readr)
library(dplyr)
library(eq5d)
library(foreign)

# 2. Set up a working diretory
setwd("<<directory path>>")

# 3. Generate baseline profiles per treatment group
## TREATMENT GOUP
eq_make_profile_data(150)
#Do you want to import probabilities? (y = yes) > n
# These are the probabilities that you have specified: 
#                         Level 1 Level 2 Level 3 Level 4 Level 5
# Mobility                0.00    0.25    0.50    0.25       0
# Self Care               0.50    0.25    0.25    0.00       0
# Usual Activities        0.00    0.25    0.25    0.50       0
# Pain & Discomfort       0.00    0.00    0.50    0.50       0
# Anxiety & Depression    0.25    0.50    0.00    0.25       0
# Save probability matrix? (y = yes) > y
# Save File Name? > d_bpm_ml5l_t1.txt
# Do you want to randomise by Profile or Dimension? (Choose p for Profile) > p
# Save File Name? > d_bp_ml5l_t1.txt

## CONTROL GROUP
eq_make_profile_data(150)
#Do you want to import probabilities? (y = yes) > n
# These are the probabilities that you have specified: 
#                         Level 1 Level 2 Level 3 Level 4 Level 5
# Mobility                0.00    0.00    0.75    0.25       0
# Self Care               0.25    0.50    0.25    0.00       0
# Usual Activities        0.25    0.00    0.25    0.50       0
# Pain & Discomfort       0.00    0.00    0.75    0.25       0
# Anxiety & Depression    0.25    0.25    0.50    0.00       0
# Save probability matrix? (y = yes) > y
# Save File Name? > d_bpm_ml5l_t0.txt
# Do you want to randomise by Profile or Dimension? (Choose p for Profile) > p
# Save File Name? > d_bp_ml5l_t0.txt

# 4. Generate follow-up profiles per treatment group
## TREATMENT GOUP
eq_make_profile_change()
# Do you want to import probabilities from a data file? (y = yes) > y
# File Name? > d_tp_ml5l_t1.txt
# Base profile data file name ? > d_bp_ml5l_t1.txt
# Matched outcome profile data file name ? > d_fp_ml5l_t1.txt

## CONTROL GROUP
eq_make_profile_change()
# Do you want to import probabilities from a data file? (y = yes) > y
# File Name? > d_tp_ml5l_t0.txt
# File name ? > d_bp_ml5l_t0.txt
# Matched outcome profile data file name ? > d_fp_ml5l_t0.txt

# 5. Calculate baseline and follow-up utilities
# 5.1 baseline utilities control group
`baseProf.m.t0` <- read.csv("<<directory path>>/d_fp_ml5l_t0.txt", sep="")
names(baseProf.m.t0)[1] <- "id"
names(baseProf.m.t0)[2] <- "MO"
names(baseProf.m.t0)[3] <- "SC"
names(baseProf.m.t0)[4] <- "UA"
names(baseProf.m.t0)[5] <- "PD"
names(baseProf.m.t0)[6] <- "AD"
names(baseProf.m.t0)[7] <- "Profile.b"
baseProf.m.t0$trt <- 0

baseProf.m.t0$NL.utility.b <- eq5d(baseProf.m.t0, type="VT", version="5L", country = "Netherlands", ignore.incomplete = TRUE)
baseProf.m.t0$US.utility.b <- eq5d(baseProf.m.t0, type="VT", version="5L", country = "USA", ignore.incomplete = TRUE)
baseProf.m.t0$JP.utility.b <- eq5d(baseProf.m.t0, type="VT", version="5L", country = "Japan", ignore.incomplete = TRUE)

# 5.2 follow-up utilities control group
`outProf.m.t0` <- read.csv("<<directory path>>/d_fp_ml5l_t0.txt", sep="")
names(outProf.m.t0)[1] <- "id"
names(outProf.m.t0)[2] <- "MO"
names(outProf.m.t0)[3] <- "SC"
names(outProf.m.t0)[4] <- "UA"
names(outProf.m.t0)[5] <- "PD"
names(outProf.m.t0)[6] <- "AD"
names(outProf.m.t0)[7] <- "Profile.f"
outProf.m.t0$trt <- 0
outProf.m.t0 <- data.frame(lapply(outProf.m.t0, function(x) as.numeric(as.character(x))))

outProf.m.t0$NL.utility.f <- eq5d(outProf.m.t0, type="VT", version="5L", country = "Netherlands", ignore.incomplete = TRUE)
outProf.m.t0$US.utility.f <- eq5d(outProf.m.t0, type="VT", version="5L", country = "USA", ignore.incomplete = TRUE)
outProf.m.t0$JP.utility.f <- eq5d(outProf.m.t0, type="VT", version="5L", country = "Japan", ignore.incomplete = TRUE)

# 5.3. Merge baseline and follow-up data control group
control <- merge(baseProf.m.t0, outProf.m.t0, by = "id")

# 5.4 baseline utilities treatment group
`baseProf.m.t1` <- read.csv("<<directory path>>/d_fp_ml5l_t1.txt", sep="")
names(baseProf.m.t1)[1] <- "id"
names(baseProf.m.t1)[2] <- "MO"
names(baseProf.m.t1)[3] <- "SC"
names(baseProf.m.t1)[4] <- "UA"
names(baseProf.m.t1)[5] <- "PD"
names(baseProf.m.t1)[6] <- "AD"
names(baseProf.m.t1)[7] <- "Profile.b"
baseProf.m.t1$trt <- 1

baseProf.m.t1$NL.utility.b <- eq5d(baseProf.m.t1, type="VT", version="5L", country = "Netherlands", ignore.incomplete = TRUE)
baseProf.m.t1$US.utility.b <- eq5d(baseProf.m.t1, type="VT", version="5L", country = "USA", ignore.incomplete = TRUE)
baseProf.m.t1$JP.utility.b <- eq5d(baseProf.m.t1, type="VT", version="5L", country = "Japan", ignore.incomplete = TRUE)

# 5.5 follow-up utilities treatment group
`outProf.m.t1` <- read.csv("<<directory path>>/d_fp_ml5l_t1.txt", sep="")
names(outProf.m.t1)[1] <- "id"
names(outProf.m.t1)[2] <- "MO"
names(outProf.m.t1)[3] <- "SC"
names(outProf.m.t1)[4] <- "UA"
names(outProf.m.t1)[5] <- "PD"
names(outProf.m.t1)[6] <- "AD"
names(outProf.m.t1)[7] <- "Profile.f"
outProf.m.t1$trt <- 0
outProf.m.t1 <- data.frame(lapply(outProf.m.t1, function(x) as.numeric(as.character(x))))

outProf.m.t1$NL.utility.f <- eq5d(outProf.m.t1, type="VT", version="5L", country = "Netherlands", ignore.incomplete = TRUE)
outProf.m.t1$US.utility.f <- eq5d(outProf.m.t1, type="VT", version="5L", country = "USA", ignore.incomplete = TRUE)
outProf.m.t1$JP.utility.f <- eq5d(outProf.m.t1, type="VT", version="5L", country = "Japan", ignore.incomplete = TRUE)

# 5.6. Merge baseline and follow-up data treatment group
treatment <- merge(baseProf.m.t1, outProf.m.t1, by = "id")

# 6. Merge treatment and control
small.eff.size <- rbind(treatment, control)

# 7. Calculate QALY
small.eff.size$NL.QALY <- (0.5*(small.eff.size$NL.utility.b + small.eff.size$NL.utility.f))
small.eff.size$JP.QALY <- (0.5*(small.eff.size$US.utility.b + small.eff.size$JP.utility.f))
small.eff.size$US.QALY <- (0.5*(small.eff.size$US.utility.b + small.eff.size$US.utility.f))

# 8. Check effect size (cohen's d = mean difference bewteen groups/ sd)
colnames(small.eff.size)[colnames(small.eff.size)=="trt.x"] <- "trt"

NL.lm <-lm(NL.QALY ~ trt, data = small.eff.size)
summary(NL.lm)
sd.NL <- sd(small.eff.size$NL.QALY)
cohen.d.NL <- NL.lm[["coefficients"]][["trt"]]/sd.NL
cohen.d.NL

US.lm <-lm(US.QALY ~ trt, data = small.eff.size)
summary(US.lm)
sd.US <- sd(small.eff.size$US.QALY)
cohen.d.US <-US.lm[["coefficients"]][["trt"]]/sd.US
cohen.d.US

JP.lm <-lm(JP.QALY ~ trt, data = small.eff.size)
summary(JP.lm)
sd.JP <- sd(small.eff.size$JP.QALY)
cohen.d.JP <- JP.lm[["coefficients"]][["trt"]]/sd.JP
cohen.d.JP

# 9. Prepare data to save in .dta
colnames(small.eff.size)[colnames(small.eff.size)=="MO.x"] <- "MO_BASELINE"
colnames(small.eff.size)[colnames(small.eff.size)=="SC.x"] <- "SC_BASELINE"
colnames(small.eff.size)[colnames(small.eff.size)=="UA.x"] <- "UA_BASELINE"
colnames(small.eff.size)[colnames(small.eff.size)=="PD.x"] <- "PD_BASELINE"
colnames(small.eff.size)[colnames(small.eff.size)=="AD.x"] <- "AD_BASELINE"

colnames(small.eff.size)[colnames(small.eff.size)=="MO.y"] <- "MO_T1"
colnames(small.eff.size)[colnames(small.eff.size)=="SC.y"] <- "SC_T1"
colnames(small.eff.size)[colnames(small.eff.size)=="UA.y"] <- "UA_T1"
colnames(small.eff.size)[colnames(small.eff.size)=="PD.y"] <- "PD_T1"
colnames(small.eff.size)[colnames(small.eff.size)=="AD.y"] <- "AD_T1"
small.eff.size$trt.y <- NULL

write.dta(small.eff.size, file = "<<directory path>>/ml-small-effsize.dta")

################################################################################

# Generate age, gender, and costs
# 1. Load libraries
library(haven)
library(simstudy)
library(foreign)

# 2. Import EQ-5D dataset and prepare to merge with simulated dataset
dataset <- read_dta("<<directory path>>/ml-small-effsize.dta") # replace the name of EQ-5D dataset here
dataset <- as.data.frame(dataset)
dataset <- remove_label(dataset)

# 3. Generate baseline characteristics 
def <- defData(varname = "age", dist="uniformInt", formula="25;75", id="id")
def <- defData(def, varname = "gender", formula = 0.19, dist = "binary", id="id")
simulatie <- genData(300, def)

# 4. Merge baseline characteristics and EQ-5D dataset
simulatie <- cbind(simulatie, dataset)
names(simulatie)[4] <- "id.x"
simulatie$id.x <- NULL

# 5. Generate correlated costs and QALYs
def1 <- defDataAdd(varname = "costs", formula = "2000 + 250*trt", variance = 1, dist = "gamma")
simulatie <- addColumns(def1, simulatie)

# 6. Check correlation
correlation <- simulatie[,cor(NL_QALY, costs)]
correlation

# 7. Save data in .dta
write.dta(simulatie, file = "<<directory path>>/ml-small-effsize.dta")
