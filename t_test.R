#PART1
#read in the table
JHS <- read.csv("~/Desktop/730_project01/jhst_proj1(1).csv")
#SET NA FOR BLAMK VALUES
JHS[is.na(JHS)==TRUE]<-NA
#SET NA FOR UNKNOWN VALUES
JHS[JHS=='Unknown'] <- NA
#CREATE NEW VARIABLES
JHS$IdealHealthBMIV1=ifelse(JHS$BMIV1>=25,0,1)

JHS$AGEgrp[JHS$AGEV1>20 & JHS$AGEV1<=40]=1
JHS$AGEgrp[JHS$AGEV1>40 & JHS$AGEV1<=50]=2
JHS$AGEgrp[JHS$AGEV1>50 & JHS$AGEV1<=60]=3
JHS$AGEgrp[JHS$AGEV1>60 & JHS$AGEV1<=70]=4
JHS$AGEgrp[JHS$AGEV1>70]=5

JHS$IdealHealthBPV1[JHS$SBPV1<120 & JHS$DBPV1<80]=1
JHS$IdealHealthBPV1[JHS$SBPV1>=120 | JHS$DBPV1>=80]=0

JHS$IdealHealthCholV1[JHS$TOTCHOLV1<240]=1
JHS$IdealHealthCholV1[JHS$TOTCHOLV1>=240]=0

JHS$IdealHealthSMKV1[JHS$SMK3CATV1=='Ideal Health']=1
JHS$IdealHealthSMKV1[JHS$SMK3CATV1=='Poor Health' | JHS$SMK3CATV1=='Intermediate Health']=0

JHS$IdealHealthPAV1[JHS$PA3CATV1=='Ideal Health']=1
JHS$IdealHealthPAV1[JHS$PA3CATV1=='Poor Health' | JHS$PA3CATV1=='Intermediate Health']=0

JHS$IdealHealthNutritionV1[JHS$NUTRITION3CATV1=='Ideal Health']=1
JHS$IdealHealthNutritionV1[JHS$NUTRITION3CATV1=='Poor Health' | JHS$NUTRITION3CATV1=='Intermediate Health']=0

for (i in 1:length(JHS$AGEV1)){
  JHS$Simple6[i]=sum(JHS$IdealHealthBMIV1[i],JHS$IdealHealthBPV1[i],JHS$IdealHealthCholV1[i],JHS$IdealHealthSMKV1[i],JHS$IdealHealthPAV1[i],JHS$IdealHealthNutritionV1[i])
  
}

JHS=JHS[is.na(JHS$DIABETESV1)==FALSE & is.na(JHS$IdealHealthBMIV1)==FALSE & is.na(JHS$IdealHealthBPV1)==FALSE & is.na(JHS$IdealHealthCholV1)==FALSE & is.na(JHS$IdealHealthSMKV1)==FALSE & is.na(JHS$IdealHealthPAV1)==FALSE & is.na(JHS$IdealHealthNutritionV1)==FALSE,]


#PART2
#remove all null in diabetesv123
result=c('Yes','No')
JHS=JHS[JHS$DIABETESV1 %in% result & JHS$DIABETESV2 %in% result & JHS$DIABETESV3 %in% result,]
dim(JHS)
#1790   42

nodia=JHS[JHS$DIABETESV1=='No',]
dim(nodia)
#1433 42

#age category
length(JHS$AGEV1[JHS$AGEV1>20 & JHS$AGEV1<=40])
length(JHS$AGEV1[JHS$AGEV1>40 & JHS$AGEV1<=50])
length(JHS$AGEV1[JHS$AGEV1>50 & JHS$AGEV1<=60])
length(JHS$AGEV1[JHS$AGEV1>60 & JHS$AGEV1<=70])
length(JHS$AGEV1[JHS$AGEV1>70])


JHS$A_D[JHS$AGEV1>20 & JHS$AGEV1<=40]=40
JHS$A_D[JHS$AGEV1>40 & JHS$AGEV1<=50]=50
JHS$A_D[JHS$AGEV1>50 & JHS$AGEV1<=60]=60
JHS$A_D[JHS$AGEV1>60 & JHS$AGEV1<=70]=70
JHS$A_D[JHS$AGEV1>70]=71
table(JHS$A_D,JHS$DIABETESV1)
chisq.test(JHS$A_D,JHS$DIABETESV1)
#table(JHS$AGEV1,JHS$DIABETESV1)
#sex

table(JHS$SexV1,JHS$DIABETESV1)
chisq.test(JHS$SexV1,JHS$DIABETESV1)


#BMI
length(JHS$BMIV1[JHS$DIABETESV1=='Yes'])
length(JHS$BMIV1[JHS$DIABETESV1=='No'])
t.test(JHS$BMIV1[JHS$DIABETESV1=='Yes'],JHS$BMIV1[JHS$DIABETESV1=='No'])

#idealhealthbmi
table(JHS$IdealHealthBMIV1,JHS$DIABETESV1)
chisq.test(JHS$IdealHealthBMIV1,JHS$DIABETESV1)

#SBP
NA %in% JHS$SBPV1 #find no na in SBPV1
t.test(JHS$SBPV1[JHS$DIABETESV1=='Yes'],JHS$SBPV1[JHS$DIABETESV1=='No'])

#DBP
NA %in% JHS$DBPV1 #find no na in DBPV1
t.test(JHS$DBPV1[JHS$DIABETESV1=='Yes'],JHS$DBPV1[JHS$DIABETESV1=='No'])

#ideal health BP
table(JHS$DIABETESV1,JHS$IdealHealthBPV1)
chisq.test(JHS$IdealHealthBPV1,JHS$DIABETESV1)

#TOTAL CHOLESTEROL
NA %in% JHS$TOTCHOLV1 #find no na in TOTCHOLV1
t.test(JHS$TOTCHOLV1[JHS$DIABETESV1=='Yes'],JHS$TOTCHOLV1[JHS$DIABETESV1=='No'])

#ideal health total cholesterol
table(JHS$DIABETESV1,JHS$IdealHealthCholV1)
chisq.test(JHS$IdealHealthCholV1,JHS$DIABETESV1)

#ideal health nutrition
table(JHS$DIABETESV1,JHS$IdealHealthNutritionV1)
chisq.test(JHS$IdealHealthNutritionV1,JHS$DIABETESV1)

#ideal health smoke
table(JHS$DIABETESV1,JHS$IdealHealthSMKV1)
chisq.test(JHS$IdealHealthSMKV1,JHS$DIABETESV1)

#ideal health physical activity
table(JHS$DIABETESV1,JHS$IdealHealthPAV1)
chisq.test(JHS$IdealHealthPAV1,JHS$DIABETESV1)

#simple 6 total score
t.test(JHS$Simple6[JHS$DIABETESV1=='Yes'],JHS$Simple6[JHS$DIABETESV1=='No'])

#income
NA %in% JHS$INCOMEV1 #find no na in TOTCHOLV1
table(JHS$INCOMEV1,JHS$DIABETESV1)
chisq.test(JHS$INCOMEV1,JHS$DIABETESV1)

#insurance
table(JHS$PRIVATEPUBLICINSV1,JHS$DIABETESV1)
chisq.test(JHS$PRIVATEPUBLICINSV1,JHS$DIABETESV1)

#insurance ~ diabetes 
chisq.test(JHS$PRIVATEPUBLICINSV1,JHS$DIABETESV1,correct = TRUE)
fisher.test(JHS$PRIVATEPUBLICINSV1,JHS$DIABETESV1)
prop.table(table(JHS$PRIVATEPUBLICINSV1,JHS$DIABETESV1),2)
tabonly=matrix(c(1014, 182, 128, 65),nrow=2, byrow=T)
#visit 1 ~ visit 2
gl=JHS[JHS$DIABETESV1=='No'&JHS$DIABETESV2=='No'&JHS$DIABETESV3=='No',]

t.test(gl$FPGV1,gl$FPGV2)

#visit 3 ~ visit 2
t.test(gl$FPGV3,gl$FPGV2)
