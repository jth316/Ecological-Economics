#################################################################
####################### CHOICE EXPERIMENT #######################
#################################################################

# Activate packages 

packageload <- c("lattice", "Icens", "DCchoice", "MASS", "AER", "pscl", 
                 "VGAM", "ggplot2", "foreign", "Matrix", "mgcv", "boot", 
                 "car", "support.CEs", "countreg")

lapply(packageload, library, character.only=TRUE, logical.return=TRUE)


park.data <- read.table(file="park data.txt",header=TRUE,sep=",",dec=".",na.strings="Y")

# *** attach the dataset
attach(park.data)

#beginning analysis

V1 <- CHOICE ~ ASC + DIV_L + DIV_S + FAC_TL + FAC_T + LIT_BR + LIT_B + COST + strata(STR)

cl_1 <- clogit(V1, data=park.data)

cl_1
#looking for z value at ASC to be absolute larger than 1.96 
#negative ASC implies people want to pick alternatives but its not significant so cant condlude
#all attribute levels had a significant difference from status quo
#one pound spent is a utility loss of -0.04
#log likelihood ratio shows that it does differ significantly from null

gofm(cl_1)

#moving into WTP

names(cl_1)

b1 <- cl_1$coefficients
b1

WTP_DIV_L <- - b1[2]/b1[8]

WTP_cl_1 <- - b1[2:7]/b1[8]

WTP_S1 <- WTP_cl_1[1] + WTP_cl_1[4] + WTP_cl_1[6]

WTP_S1 <- WTP_cl_1[1] *1 + WTP_cl_1[2] * 0 + WTP_cl_1[3] * 0 + WTP_cl_1[4] + WTP_cl_1[5] * 0 + WTP_cl_1[6]

#splitting samples

#split sample approach
#looking at q13
table(Q13_EDIN,exclude=NULL)
#54 rows dk/refused therefore 2 people (27 rows per person)
3186/(3186+1296+810+756) #percentage of people living in edinburgh in catgeory 1

oneY <- ifelse(Q13_EDIN<2,1,0)
mean(oneY)
mean(oneY,na.rm=TRUE)

oneYDK <- ifelse(is.na(Q13_EDIN)==TRUE,1,0)
mean(oneYDK)

park.data <- cbind(park.data,oneY, oneYDK)
dim(park.data)

#now doing the subsetting

park.dataY <- subset(park.data, oneY == 1 & oneYDK ==0)


cl_Y <- clogit(V1, data=park.dataY)
cl_Y

park.dataNY <- subset(park.data, oneY == 0 & oneYDK ==0)


cl_NY <- clogit(V1, data=park.dataNY)
cl_NY

bY <- cl_Y$coefficients
WTP_cl_Y <- - bY[2:7]/bY[8]

bNY <- cl_NY$coefficients
WTP_cl_NY <- - bNY[2:7]/bNY[8]

WTP_cl_Y #lit_B for this should be zero as coefficient is not significant
WTP_cl_NY

interaction approach

park.dataDKY <- subset(park.data, oneYDK==0)

V2 <- CHOICE ~ ASC + DIV_L + DIV_S + FAC_TL + FAC_T + LIT_BR + LIT_BR:oneY + LIT_B + LIT_B:oneY + COST + strata(STR)

cl_IA <- clogit(V2, data=park.dataDKY)
cl_IA 

LIT_BR_IA <- LIT_BR * oneY #can do it this way then put it in; either or

bIA <- cl_IA$coefficients

print(WTP_LIT_BR_oneY <- - (bIA[6] + bIA[7] * 1)/bIA[10])
print(WTP_LIT_BR_moreY <- - (bIA[6] + bIA[7] * 0)/bIA[10])

print(WTP_LIT_B_oneY <- - (bIA[8] + bIA[9] * 1)/bIA[10])
print(WTP_LIT_B_moreY <- - (bIA[8] + bIA[9] * 0)/bIA[10])
#notice the location of subset categories affects results

#split sample for report

genM <- ifelse(Q7_GEN<1,1,0)
genF <- ifelse(Q7_GEN==1,1,0)
genO <- ifelse(Q7_GEN>1,1,0)
genNA <- ifelse(is.na(Q7_GEN)==TRUE,1,0)



park.data <- cbind(park.data,genM, genF, genO, genNA)

park.dataM <- subset(park.data, genM==1 & genNA ==0)
park.dataF <- subset(park.data, genF==1 & genNA ==0)

cl_M <- clogit(V1, data=park.dataM)
cl_F <- clogit(V1, data=park.dataF)

bM <- cl_M$coefficients
bF <- cl_F$coefficients

WTP_cl_M <- - bM[2:7]/bM[8]
WTP_cl_F <- - bF[2:7]/bF[8]

ngoY <- ifelse(Q14_MEM<2,1,0)
ngoNA <- ifelse(is.na(Q14_MEM)==TRUE,1,0)

park.data <- cbind(park.data,ngoY, ngoNA)

park.dataNGO <- subset(park.data, ngoY==1 & ngoNA ==0)
park.dataNGNO <- subset(park.data, ngoY==0 & ngoNA ==0)

cl_ngoY <- clogit(V1, data=park.dataNGO)
cl_ngoN <- clogit(V1, data=park.dataNGNO)

bNGO <- cl_ngoY$coefficients
bNGON <- cl_ngoN$coefficients

WTP_cl_ngoY <- - bNGO[2:7]/bNGO[8]
WTP_cl_ngoN <- - bNGON[2:7]/bNGON[8]


