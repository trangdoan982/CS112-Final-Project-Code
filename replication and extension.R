###############################################################################################################################################################
#This is the full replication file for Charnysh and Finkel. "The Death Camp Eldorado: Political and Economic Effects of Mass Violence." APSR (forthcoming). ###                
###############################################################################################################################################################

#This file replicates the main empirical analysis in the article
#as well as the additional analysis in the online appendix. 

################################################
#Loading libraries and mail replication files ##
################################################

rm(list=ls())

#install required packages if they are missing:
list.of.packages <- c("stargazer","visreg","spdep","maptools","rgdal","maptools","sandwich","lmtest","RCurl", "SnowballC","wordcloud","RColorBrewer","tm","foreign")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)
rm(list.of.packages, new.packages)

### change path to user's local directory
load("MainDataset.RData")

#Two main datasets are loaded, crd for communities before 1999 and pol for communities after 1999

crd$distRail45KM<-crd$distRail45/1000 #Meters to km 

summary(crd$distTreb[crd$distTreb <=50 & crd$miasto==0])
sd(crd$distTreb[crd$distTreb <=50 & crd$miasto==0])

summary(pol$CampDistKM[pol$CampDistKM <=50 & pol$type!="urban"])
sd(pol$CampDistKM[pol$CampDistKM <=50 & pol$type!="urban"]) 

summary(crd$distRail45KM[crd$distTreb <=50 & crd$miasto==0])
sd(crd$distRail45KM[crd$distTreb <=50 & crd$miasto==0]) 

###############################################################################
############# TABLE 4: Logit Regression, Parliamentary Election 2001 ##########
###############################################################################

# 50km with distance from camp only
pol$NonLPR2001<-pol$Valid2001-pol$LPR2001
LPR2001_50Alog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50Alog) 

# 50km with distance from camp and fixed electoral effect
LPR2001_50Blog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50Blog)  

# 50km with distance from camp, distance from railroad, distance from city, and fixed electoral effect (all confounders)
LPR2001_50Clog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50Clog)  

# 60km with all confounders
LPR2001_60GGlog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=60 & pol$type!="urban" & pol$GG==1,], quasibinomial)
summary(LPR2001_60GGlog)  

# 70km with all confounders
LPR2001_70GGlog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=70 & pol$type!="urban" & pol$GG==1,], quasibinomial)
summary(LPR2001_70GGlog)  

stargazer(LPR2001_50Alog, LPR2001_50Blog, LPR2001_50Clog, LPR2001_60GGlog, LPR2001_70GGlog)

#######################################################################################
############# FIGURE 4: Support for the LPR and Distance to Treblinka    ##############
#######################################################################################

visreg(LPR2001_50Alog, "CampDistKM", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="LPR vote choice (2001)", fill.par=list(density = 15, angle = 90, col="blue"), line.par=list(col="black"))

###############################################################################
############# TABLE 5: Logit Regression, Parliamentary Election 2001 ##########
###############################################################################

# votes in 2001 for PiS party
PiS2001<-glm(cbind(PiS2001, Valid2001-PiS2001)~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(PiS2001)    

# votes in 2001 for AWSP party
AWSP2001<-glm(cbind(AWSP2001, Valid2001-AWSP2001)~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(AWSP2001)  

# votes in 2001 for PO party
PO2001<-glm(cbind(PO2001, Valid2001-PO2001)~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(PO2001)  

#PiS, AWSP & PO
all2001<-glm(cbind(PiS2001 + AWSP2001+ PO2001, Valid2001-(PiS2001 + AWSP2001+ PO2001))~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(all2001)  

# relationship between election turnout and distance
pol$NonTurn2001<-pol$Elig2001-pol$Turn2001
turnout2001<-glm(cbind(Turn2001, NonTurn2001) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(turnout2001)  

stargazer(PiS2001, AWSP2001, PO2001, all2001, turnout2001)

################################################################################################
############# TABLE A11: Logit Regression, Vote for Endecja and BNM in 1928 ####################
################################################################################################
vote1928<-read.csv("election1928.csv") 
head(vote1928)

vote1928$shareBM<-vote1928$BlockMniejsz/vote1928$Valid1928
summary(vote1928$shareBM[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w"])
sd(vote1928$shareBM[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w"])


vote1928$shareEndecja<-vote1928$Endecja/vote1928$Valid1928
summary(vote1928$shareEndecja[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w"])
sd(vote1928$shareEndecja[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w"])

vote1928$nonBM<-vote1928$Valid1928-vote1928$BlockMniejsz
vote1928$nonEndecja<-vote1928$Valid1928-vote1928$Endecja

# votes in 1928 for Endecja party, 50km
reg50ND<-glm(cbind(Endecja, nonEndecja) ~log(Distance.to.Treblinka), data=vote1928[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w",], quasibinomial)
summary(reg50ND) 

# votes in 1928 for Endecja party, 60km
reg60ND<-glm(cbind(Endecja, nonEndecja) ~log(Distance.to.Treblinka), data=vote1928[vote1928$Distance.to.Treblinka<=60 & vote1928$Type=="w",], quasibinomial)
summary(reg60ND) 

# votes in 1928 for BM party, 50 km
reg50BM<-glm(cbind(BlockMniejsz, nonBM) ~log(Distance.to.Treblinka), data=vote1928[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w",], quasibinomial)
summary(reg50BM) 

# votes in 1928 for BM party, 60km
reg60BM<-glm(cbind(BlockMniejsz, nonBM) ~log(Distance.to.Treblinka), data=vote1928[vote1928$Distance.to.Treblinka<=60 & vote1928$Type=="w",], quasibinomial)
summary(reg60BM)

stargazer(reg50ND, reg60ND, reg50BM, reg60BM)


########################################################################################################
############# TABLE A12: Logit Regression, Support for Right-Wing Parties in 1997, 2005, 2015 ##########
########################################################################################################

# votes for AWS party in 1997
crd$nonAWS1997<-crd$Valid97-crd$AWS1997

regAWS1997b<-glm(cbind(AWS1997, nonAWS1997) ~log(distTreb)+Okreg1997, data=crd[crd$distTreb <=50 &crd$miasto==0,], quasibinomial)
summary(regAWS1997b) 

# votes for LPR party in 2005
pol$NonLPR2005<-pol$Valid2005-pol$LPR2005

LPR2005b<-glm(cbind(LPR2005, NonLPR2005) ~log(CampDistKM)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2005b)

# votes for PiS party in 2005
pol$NonPiS2005<-pol$Valid2005-pol$PiS2005

PiS2005b<-glm(cbind(PiS2005, NonPiS2005) ~log(CampDistKM)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(PiS2005b) 

# votes for PiS party in 2015
pol$NonPiS2015<-pol$Valid2015-pol$PiS2015
PiS2015a<-glm(cbind(PiS2015, NonPiS2015)~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(PiS2015a)  

PiS2015b<-glm(cbind(PiS2015, NonPiS2015)~log(CampDistKM)+ ElDist15, data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(PiS2015b)   

stargazer(regAWS1997b, LPR2005b, PiS2005b, PiS2015a, PiS2015b)


                                    ####################################################
                                    ########### EXTENSION WORK - MATCHING ##############
                                    ####################################################
# CLEAN THE DATASET FOR MATCHING
data.matching <- data.frame(pol, stringsAsFactors = TRUE)
removes <- c()
data.matching$IncTax95 <- as.numeric(data.matching$IncTax95)
data.matching$GG <- as.factor(data.matching$GG)

# Define treated and control unit, remove units that do not fall in the range
data.matching$treat <- rep(NA, nrow(data.matching))

for (i in c(1:nrow(data.matching))) {
  if (data.matching$CampDistKM[i] < 25) {
    data.matching$treat[i] <- 1
  }
  else if (data.matching$CampDistKM[i] > 40 & data.matching$CampDistKM[i] <= 60) {
    data.matching$treat[i] <- 0
  } 
  else{
    removes <- append(removes, i)
  }
}
data.matching <- data.matching[-removes, ]

# Remove urban communities for the same reason as the source paper
data.matching <- data.matching[-which(data.matching$type == "urban"), ]

                                    #########################################################################
                                    ########### Matching with Different Covariate Combinations ##############
                                    #########################################################################                       


library(Matching)
library(rbounds)

# FIRST COMBINATION: only the confounders from the source paper
X <- cbind(data.matching$distRail45, data.matching$CityDistKm, data.matching$EllDist01)
Tr <- data.matching$treat
genout <- GenMatch(Tr=Tr, X=X, pop.size = 1000, nboots = 500)
Y <- data.matching$PctLPR2001
mout <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix=genout)
mb  <- MatchBalance(treat ~ distRail45 + CityDistKm + EllDist01, data=data.matching, match.out = mout, nboots = 500)
summary(mout)

# sensitivity analysis
sensitivity <- psens(mout, Gamma = 2.5, GammaInc = 0.1) #gamma = 1.8
sensitivity

# SECOND COMBINATION: with most variables
X2 <- cbind(data.matching$distRail45, data.matching$CityDistKm, data.matching$EllDist01, data.matching$Pop95, 
            data.matching$IncTax95, data.matching$Elig2001, data.matching$TotEnt95)
genout2 <- GenMatch(Tr=Tr, X=X2, pop.size = 1000, nboots = 500)
mout2 <- Match(Y=Y, Tr=Tr,X=X2, Weight.matrix = genout2)
mb2 <- MatchBalance(treat ~ distRail45 + CityDistKm + EllDist01 + Pop95 + IncTax95 + Elig2001 + TotEnt95, data = data.matching,
                    match.out = mout2, nboots = 500)

summary(mout2)

# sensitivity analysis
sensitivity2 <- psens(mout1, Gamma = 2.5, GammaInc = 0.1) #gamma =
sensitivity2



                                      ####################################################
                                      ####### COMPARE RESULTS BETWEEN TWO METHODS ########
                                      ####################################################
# ORIGINAL DATA SET (pol)

########################################################################################################
############### FIGURE 1: Replication of Regression Results from Original Paper ########################
########################################################################################################


pol$NonLPR2001<-pol$Valid2001-pol$LPR2001
LPR2001_50Alog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50Alog)

original <- visreg(LPR2001_50Alog, "CampDistKM", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="LPR vote choice (2001) - Original Dataset", fill.par=list(density = 15, angle = 90, col="blue"), line.par=list(col="black"))
original

########################################################################################################
#### FIGURE 2: Regression results with matched dataset using the original confounders in the paper #####
########################################################################################################
matched.index <- c(mout$index.control, mout$index.treated)
matched_data <- data.matching[matched.index, ]

matched_data$NonLPR2001<-matched_data$Valid2001-matched_data$LPR2001
a.LPR2001_50Alog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM), data=matched_data[matched_data$CampDistKM <=50 & matched_data$type!="urban",],quasibinomial)
summary(a.LPR2001_50Alog) 


new <- visreg(a.LPR2001_50Alog, "CampDistKM", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="LPR vote choice (2001) - Matched Dataset with Original Confounders ", fill.par=list(density = 15, angle = 90, col="red"), line.par=list(col="black"))
new

########################################################################################################
############## FIGURE 3: Regression results with matched dataset using the extra confounders ###########
########################################################################################################
matched.index2 <- c(mout2$index.control, mout2$index.treated)
matched_data2 <- data.matching[matched.index2, ]

matched_data2$NonLPR2001<-matched_data2$Valid2001-matched_data2$LPR2001
aLPR2001_50Alog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM), data=matched_data2[matched_data2$CampDistKM <=50 & matched_data2$type!="urban",],quasibinomial)
summary(LPR2001_50Alog) 

new2 <- visreg(aLPR2001_50Alog, "CampDistKM", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="LPR vote choice (2001) - Matched Dataset with Additional Confounders", fill.par=list(density = 15, angle = 90, col="red"), line.par=list(col="black"))
new2
