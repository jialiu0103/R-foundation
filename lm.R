jhst$diabetes[jhst$DiabetesV3=='Yes']=1
jhst$diabetes[jhst$DiabetesV3=='No']=0
jhst[jhst==""]<-NA
###drop diabetes when analyse fasting plasma glucose
data_fpg=jhst[-which(jhst$DiabetesV3=='Yes'),]
fpg=data_fpg[is.na(data_fpg$FPGV3)==FALSE,]
table(fpg$PrivatepublicInsV3,useNA = 'ifany')


###part1a
#unadjusted
my_fpg=fpg[is.na(fpg$PrivatepublicInsV3)==FALSE,]
table(my_fpg$PrivatepublicInsV3)
table(my_fpg$sex)
m1=lm(FPGV3~PrivatepublicInsV3,my_fpg)
summary(m1)
#adjusted
#ad_data=my_fpg[is.na(my_fpg$sex | my_fpg$ageV3)==FALSE,]
m2=lm(FPGV3~PrivatepublicInsV3+ageV3+sex,my_fpg)
summary(m2)

###part1b
no_fpg=jhst[is.na(jhst$PrivatepublicInsV3)==FALSE,]
no_fpg=no_fpg[is.na(no_fpg$diabetes)==FALSE,]
table(no_fpg$PrivatepublicInsV3,useNA = 'ifany')
table(no_fpg$diabetes,useNA = 'ifany')
#unadjustes
m3<-glm(diabetes~PrivatepublicInsV3,family = binomial(link="logit"),data=no_fpg)
summary(m3)
exp(cbind(OR=coef(m3),confint(m3)))
#adjustes
m4<-glm(diabetes~PrivatepublicInsV3+ageV3+sex,family = binomial(link="logit"),data=no_fpg)
summary(m4)
exp(cbind(OR=coef(m4),confint(m4)))


###part2a
a2_fpg=fpg[is.na(fpg$sbpV3)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$dbpV3)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$totcholV3)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$BMIV3)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$idealhealthSMKv3)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$idealhealthPAV3)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$idealHealthNutritionv3)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$sex)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$ageV3)==FALSE,]
a2_fpg=a2_fpg[is.na(a2_fpg$Simple6)==FALSE,]
#table(a2_fpg$sbpV3,useNA = 'ifany')

#age/sex adjusted
m5_1=lm(FPGV3~sex+ageV3, data=a2_fpg)
summary(m5_1)

m5_2=lm(FPGV3~BMIV3+sex+ageV3, data=a2_fpg)
summary(m5_2)

m5_3=lm(FPGV3~sbpV3+sex+ageV3, data=a2_fpg)
summary(m5_3)

m5_4=lm(FPGV3~dbpV3+sex+ageV3, data=a2_fpg)
summary(m5_4)

m5_5=lm(FPGV3~totcholV3+sex+ageV3, data=a2_fpg)
summary(m5_5)

m5_6=lm(FPGV3~idealhealthSMKv3+sex+ageV3, data=a2_fpg)
summary(m5_6)

m5_7=lm(FPGV3~idealhealthPAV3+sex+ageV3, data=a2_fpg)
summary(m5_7)

m5_8=lm(FPGV3~idealHealthNutritionv3+sex+ageV3, data=a2_fpg)
summary(m5_8)

m5_9=lm(FPGV3~Simple6+sex+ageV3, data=a2_fpg)
summary(m5_9)
#m5=lm(FPGV3~c(sbpV3+dbpV3+totcholV3+BMIV3+idealhealthSMKv3+idealhealthPAV3+idealHealthNutritionv3)+sex+ageV3, data = a2_fpg)

#multivariable adjusted
m6=lm(FPGV3~sbpV3+dbpV3+totcholV3+BMIV3+idealhealthSMKv3+idealhealthPAV3+idealHealthNutritionv3+sex+ageV3, data = a2_fpg)
summary(m6)


###part2b
b2=jhst[is.na(jhst$diabetes)==FALSE,]
b2=b2[is.na(b2$sbpV3)==FALSE,]
b2=b2[is.na(b2$dbpV3)==FALSE,]
b2=b2[is.na(b2$totcholV3)==FALSE,]
b2=b2[is.na(b2$BMIV3)==FALSE,]
b2=b2[is.na(b2$idealhealthSMKv3)==FALSE,]
b2=b2[is.na(b2$idealhealthPAV3)==FALSE,]
b2=b2[is.na(b2$idealHealthNutritionv3)==FALSE,]
b2=b2[is.na(b2$sex)==FALSE,]
b2=b2[is.na(b2$ageV3)==FALSE,]
b2=b2[is.na(b2$Simple6)==FALSE,]
table(b2$diabetes)

#sex /age adjusted
#age/sex adjusted
m7_1=glm(diabetes~sex+ageV3, data=b2)
summary(m7_1)
exp(cbind(OR=coef(m7_1),confint(m7_1)))

m7_2=glm(diabetes~BMIV3+sex+ageV3, data=b2)
summary(m7_2)
exp(cbind(OR=coef(m7_2),confint(m7_2)))

m7_3=glm(diabetes~sbpV3+sex+ageV3, data=b2)
summary(m7_3)
exp(cbind(OR=coef(m7_3),confint(m7_3)))

m7_4=glm(diabetes~dbpV3+sex+ageV3, data=b2)
summary(m7_4)
exp(cbind(OR=coef(m7_4),confint(m7_4)))

m7_5=glm(diabetes~totcholV3+sex+ageV3, data=b2)
summary(m7_5)
exp(cbind(OR=coef(m7_5),confint(m7_5)))

m7_6=glm(diabetes~idealhealthSMKv3+sex+ageV3, data=b2)
summary(m7_6)
exp(cbind(OR=coef(m7_6),confint(m7_6)))

m7_7=glm(diabetes~idealhealthPAV3+sex+ageV3, data=b2)
summary(m7_7)
exp(cbind(OR=coef(m7_7),confint(m7_7)))

m7_8=glm(diabetes~idealHealthNutritionv3+sex+ageV3, data=b2)
summary(m7_8)
exp(cbind(OR=coef(m7_8),confint(m7_8)))

m7_9=glm(diabetes~Simple6+sex+ageV3, data=b2)
summary(m7_9)
exp(cbind(OR=coef(m7_9),confint(m7_9)))

#multivariable adjusted
m8=lm(diabetes~sbpV3+dbpV3+totcholV3+BMIV3+idealhealthSMKv3+idealhealthPAV3+idealHealthNutritionv3+sex+ageV3, data = b2)
summary(m8)
exp(cbind(OR=coef(m8),confint(m8)))

###part3
#model1
m9<-lm(FPGV3~ageV3+sex+BMIV3+totcholV3+sex:totcholV3, data=fpg)
summary(m9)
#model2
m10<-lm(FPGV3~ageV3+sex+BMIV3+totcholV3+sex:BMIV3, data=fpg)
summary(m10)
