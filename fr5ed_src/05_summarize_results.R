#Load model with the contrast between countries for each estimate and without this contrast
library(rethinking)
load("posterior_samples_by_country.RData")
load("posterior_samples_contrasts.RData")
load("posterior_samples_single.RData")

nrow(d)

#Compare the two models
compare(m1,mC)

stancode(mC)


#Comparison moral vs nonmoral messages
str(post1)

colMeans(post1$aC)

#donation
moral<-apply(post1$aC[,3:10],1,mean)
mean(moral)
sd(moral)

#moral vs prudential
round(mean(moral-post1$aC[,2]),2)
round(sd(moral-post1$aC[,2]),2)
round(PI(moral-post1$aC[,2]),2)

#moral vs none
round(mean(moral-post1$aC[,1]),2)
round(sd(moral-post1$aC[,1]),2)
round(PI(moral-post1$aC[,1]),2)


#Precuation
moral<-apply(post1$aCP[,3:10],1,mean)
mean(moral)
sd(moral)

#moral vs prudential
round(mean(moral-post1$aCP[,2]),2)
round(sd(moral-post1$aCP[,2]),2)
round(PI(moral-post1$aCP[,2]),2)

#moral vs none
round(mean(moral-post1$aCP[,1]),2)
round(sd(moral-post1$aCP[,1]),2)
round(PI(moral-post1$aCP[,1]),2)


#ProSociality
moral<-apply(post1$aCS[,3:10],1,mean)
mean(moral)
sd(moral)

#moral vs prudential
round(mean(moral-post1$aCS[,2]),2)
round(sd(moral-post1$aCS[,2]),2)
round(PI(moral-post1$aCS[,2]),2)

#moral vs none
round(mean(moral-post1$aCS[,1]),2)
round(sd(moral-post1$aCS[,1]),2)
round(PI(moral-post1$aCS[,1]),2)




options(max.print=10000000)
precis(mC,depth=2)


tapply(dat$D,d$Country,mean)
tapply(d$donation,d$Country,mean)
tapply(d$donation,d$Country,sd)

str(postC)

#Contrasts between man and women between Country samples

mean(postUSA$aG_Fam[,1])
mean(postUSA$aG_Fam[,2])

table(d$Country,d$CountC)


#Here starts the text
round(mean(post1$bA),2)
round(PI(post1$bA),2)

round(mean(postUSA$bA),2)
round(PI(postUSA$bA),2)

round(mean(postIndia$bA),2)
round(PI(postIndia$bA),2)

round(mean(postC$bAC),2)
round(PI(postC$bAC),2)



round(mean(post1$bS),2)
round(PI(post1$bS),2)

round(mean(postUSA$bS),2)
round(PI(postUSA$bS),2)

round(mean(postIndia$bS),2)
round(PI(postIndia$bS),2)


round(mean(post1$aC[,2]),2)
round(PI(post1$aC[,2]),2)

round(mean(postUSA$aC[,10]),2)
round(PI(postUSA$aC[,10]),2)

round(mean(postIndia$aC[,1]),2)
round(PI(postIndia$aC[,1]),2)


round(mean(post1$bPro),2)
round(PI(post1$bPro),2)

round(mean(postUSA$bFai),2)
round(PI(postUSA$bFai),2)

round(mean(postIndia$bHer),2)
round(PI(postIndia$bHer),2)


round(mean(post1$bDg),2)
round(PI(post1$bDg),2)

#Precaution
round(mean(postUSA$aGP[,2]-postUSA$aGP[,1]),2)
round(PI(postUSA$aGP[,2]-postUSA$aGP[,1]),2)

round(mean(postIndia$aGP[,2]-postIndia$aGP[,1]),2)
round(PI(postIndia$aGP[,2]-postIndia$aGP[,1]),2)


round(mean(postIndia$bAP),2)
round(PI(postIndia$bAP),2)

round(mean(postUSA$bAP),2)
round(PI(postUSA$bAP),2)


round(mean(postUSA$aCP[,6]),2)
round(PI(postUSA$aCP[,6]),2)



round(mean(post1$aCP[,7]),2)
round(PI(post1$aCP[,7]),2)


round(mean(postUSA$aCP[,8]),2)
round(PI(postUSA$aCP[,8]),2)



round(mean(postUSA$bConP),2)
round(PI(postUSA$bConP),2)

round(mean(post1$bConP),2)
round(PI(post1$bConP),2)

round(mean(postIndia$bConP),2)
round(PI(postIndia$bConP),2)


round(mean(post1$bRecP),2)
round(PI(post1$bRecP),2)


round(mean(postIndia$bFamP),2)
round(PI(postIndia$bFamP),2)

round(mean(postIndia$bGroP),2)
round(PI(postIndia$bGroP),2)


round(mean(post1$bDgP),2)
round(PI(post1$bDgP),2)



#Prosociality
round(mean(postUSA$aGS[,2]-postUSA$aGS[,1]),2)
round(PI(postUSA$aGS[,2]-postUSA$aGS[,1]),2)

round(mean(postIndia$aGS[,2]-postIndia$aGS[,1]),2)
round(PI(postIndia$aGS[,2]-postIndia$aGS[,1]),2)

round(mean(post1$aCS[,7]),2)
round(PI(post1$aCS[,7]),2)


round(mean(post1$bGroS),2)
round(PI(post1$bGroS),2)


round(mean(postIndia$bFamS),2)
round(PI(postIndia$bFamS),2)

round(mean(postIndia$bRecS),2)
round(PI(postIndia$bRecS),2)

round(mean(postIndia$bProS),2)
round(PI(postIndia$bProS),2)


round(mean(postIndia$bDgS),2)
round(PI(postIndia$bDgS),2)


#MAC scores
round(mean(postUSA$aG_Fam[,2]-postUSA$aG_Fam[,1]),2)
round(mean(postUSA$aG_Gro[,2]-postUSA$aG_Gro[,1]),2)
round(mean(postUSA$aG_Rec[,2]-postUSA$aG_Rec[,1]),2)
round(mean(postUSA$aG_Her[,2]-postUSA$aG_Her[,1]),2)
round(mean(postUSA$aG_Def[,2]-postUSA$aG_Def[,1]),2)
round(mean(postUSA$aG_Fai[,2]-postUSA$aG_Fai[,1]),2)
round(mean(postUSA$aG_Pro[,2]-postUSA$aG_Pro[,1]),2)


round(mean(postIndia$aG_Fam[,2]-postIndia$aG_Fam[,1]),2)
round(mean(postIndia$aG_Gro[,2]-postIndia$aG_Gro[,1]),2)
round(mean(postIndia$aG_Rec[,2]-postIndia$aG_Rec[,1]),2)
round(mean(postIndia$aG_Her[,2]-postIndia$aG_Her[,1]),2)
round(mean(postIndia$aG_Def[,2]-postIndia$aG_Def[,1]),2)
round(mean(postIndia$aG_Fai[,2]-postIndia$aG_Fai[,1]),2)
round(mean(postIndia$aG_Pro[,2]-postIndia$aG_Pro[,1]),2)


round(mean(postC$aG_ProC[,2]),2)
round(PI(postC$aG_ProC[,2]),2)

round(mean(postC$aG_GroC[,2]),2)
round(PI(postC$aG_GroC[,2]),2)



round(mean(postC$aG_GroC[,1]),2)
round(PI(postC$aG_GroC[,1]),2)

round(mean(postC$aG_ProC[,1]),2)
round(PI(postC$aG_ProC[,1]),2)


round(mean(postC$bAge_FaiC),2)
round(PI(postC$bAge_FaiC),2)



round(mean(postC$bAge_FamC),2)
round(mean(postC$bAge_GroC),2)
round(mean(postC$bAge_RecC),2)
round(mean(postC$bAge_HerC),4)
round(mean(postC$bAge_DefC),4)
round(mean(postC$bAge_FaiC),2)
round(mean(postC$bAge_ProC),2)

round(PI(postC$bAge_FamC),2)
round(PI(postC$bAge_GroC),2)
round(PI(postC$bAge_RecC),2)
round(PI(postC$bAge_HerC),2)
round(PI(postC$bAge_DefC),2)
round(PI(postC$bAge_FaiC),2)
round(PI(postC$bAge_ProC),2)



round(mean(postC$bAge_FaiC),2)
round(PI(postC$bAge_FaiC),2)

round(mean(postC$bAge_HerC),2)
round(PI(postC$bAge_HerC),2)





effs<-c(
1-mean(postC$sigma_Fam)^2,
1-mean(postC$sigma_Gro)^2,
1-mean(postC$sigma_Rec)^2,
1-mean(postC$sigma_Her)^2,
1-mean(postC$sigma_Def)^2,
1-mean(postC$sigma_Fai)^2,
1-mean(postC$sigma_Pro)^2)

sqrt(1-effs)
effs

mean(effs)
sd(effs)


round(mean(postC$aG_DangC[,1]),2)
round(PI(postC$aG_DangC[,1]),2)

round(mean(postC$aG_DangC[,2]),2)
round(PI(postC$aG_DangC[,2]),2)


round(mean(postUSA$bAge_Dang),2)
round(PI(postUSA$bAge_Dang),2)


round(mean(postIndia$bAge_Dang),2)
round(PI(postIndia$bAge_Dang),2)


