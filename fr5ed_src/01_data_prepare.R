#Load the messy data from qualtrics
d1<-read.csv("QexportUSA.csv",sep=",",header=T,stringsAsFactors = F)
d2<-read.csv("QexportIndia.csv",sep=",",header=T,stringsAsFactors = F)

names(d1)==names(d2)

names(d1)[154:ncol(d1)]
names(d2)[154:ncol(d2)]

#Get rid of the extra column in the Indian sample
d2<-d2[,-which(names(d2)=="viga")]

#Now the names match exactly
names(d1)==names(d2)

names(d1)[18]<-"Agreement"
names(d2)[18]<-"Agreement"

questions<-d2[1,]
questions[which(names(d2)=="age")]
questions[which(names(d2)=="Agreement")]

names(d1)[which(names(d1)=="Q12")]<-"Age"
names(d1)[which(names(d1)=="Q13")]<-"Gender"

names(d1)[which(names(d1)=="Q49_1")]<-"danger.for.participant"
names(d1)[which(names(d1)=="Q10_1")]<-"donation"
names(d1)[which(names(d1)=="Q4_38")]<-"move.to.middle"
names(d1)[which(names(d1)=="Q11")]<-"email"
names(d1)[which(names(d1)=="isNew")]<-"is.new"
names(d1)[which(names(d1)=="Q_TotalDuration")]<-"duration"
names(d1)[which(names(d1)=="age")]<-"age.cohort"

#Do the same with the second dataset
names(d2)[which(names(d2)=="Q12")]<-"Age"
names(d2)[which(names(d2)=="Q13")]<-"Gender"

names(d2)[which(names(d2)=="Q49_1")]<-"danger.for.participant"
names(d2)[which(names(d2)=="Q10_1")]<-"donation"
names(d2)[which(names(d2)=="Q4_38")]<-"move.to.middle"
names(d2)[which(names(d2)=="Q11")]<-"email"
names(d2)[which(names(d2)=="isNew")]<-"is.new"
names(d2)[which(names(d2)=="Q_TotalDuration")]<-"duration"
names(d2)[which(names(d2)=="age")]<-"age.cohort"

names(d1)
names(d2)

d1save<-d1[-c(1,2),]
d2save<-d2[-c(1,2),]

nrow(d1save)
nrow(d2save)

#Create questions vector again with up to date colum names
questions<-d1[1,]

dsave<-rbind(d1save,d2save)
dsave$Country<-rep(c("USA","India"),c(nrow(d1save),nrow(d2save)))

#Check if the full dataset has correct number of rows
nrow(dsave)

write.table(dsave,"raw_data.txt",sep="\t",row.names = F)
write.table(questions,"questions.txt",sep="\t",row.names = F)

#Load the raw text data anew for a fresh start
d<-read.table("raw_data.txt",sep="\t",header=T,stringsAsFactors = F)
questions<-read.table("questions.txt",sep="\t",header=T,stringsAsFactors = F)

summary(d)
questions

#function that takes last N characters
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Other functions for easy check that teh vector contains only a single value
minmax<-function(x){
  max<-max(x,na.rm=T)
  min<-min(x,na.rm=T)
  if(max==min){
    return(max)
  }else{
    return("conflict")
  }
}

#Similar function that ruturns the order with maximum value
whichcond<-function(x){
  max<-max(x,na.rm=T)
  min<-min(x,na.rm=T)
  if(max==min){
    which(x==max)
  }else{
    return("conflict")
  }
}

names(d)

nam<-names(d)

questions[which(names(d)=="X5_Q7_1")]

#Extract responses to behavioral questionnaire
ATTENTION_CHECK<-apply(d[,substrRight(nam,5)=="Q50_1"],1,minmax)
INT_DIR_CONTACT<-apply(d[,substrRight(nam,4)=="Q7_1"],1,minmax)
INT_QUARANTINE<-apply(d[,substrRight(nam,4)=="Q7_2"],1,minmax)
INT_STAY_HOME<-apply(d[,substrRight(nam,4)=="Q7_3"],1,minmax)
INT_WASH_HANDS<-apply(d[,substrRight(nam,5)=="Q7_11"],1,minmax)
INT_CANCEL_VAC<-apply(d[,substrRight(nam,5)=="Q7_12"],1,minmax)
INT_NEIGHBOR_SHOP<-apply(d[,substrRight(nam,5)=="Q7_13"],1,minmax)
INT_NOT_HOARD<-apply(d[,substrRight(nam,5)=="Q7_14"],1,minmax)
INT_NEIGHBOR_CHILD<-apply(d[,substrRight(nam,5)=="Q7_15"],1,minmax)
INT_SUPPORT_COMP<-apply(d[,substrRight(nam,5)=="Q7_16"],1,minmax)

ATTENTION_CHECK
INT_DIR_CONTACT
INT_QUARANTINE
INT_STAY_HOME
INT_WASH_HANDS
INT_CANCEL_VAC
INT_NEIGHBOR_SHOP
INT_NOT_HOARD
INT_NEIGHBOR_CHILD
INT_SUPPORT_COMP

COND<-apply(d[,substrRight(nam,5)=="Q50_1"],1,whichcond)
CONDlabel<-as.factor(COND)
levels(CONDlabel)<-c("Control","Prudence","General","Family","Group","Reciprocity","Heroism","Deference","Fainess","Property")

c(1:ncol(d))[substr(names(d),1,2)=="Q4"]
nam[substr(names(d),1,2)=="Q4"]
            
nam[substr(names(d),1,2)=="Q4"]<-
  c("FAM1_MAC","LOY1_MAC","REC1_MAC","HER1_MAC","DEF1_MAC","FAI1_MAC","PRO1_MAC",              
    "FAM2_MAC","LOY2_MAC","REC2_MAC","HER2_MAC","DEF2_MAC","FAI2_MAC","PRO2_MAC",
    "FAM3_MAC","LOY3_MAC","REC3_MAC","HER3_MAC","DEF3_MAC","FAI3_MAC","PRO3_MAC")

MAC.fam<-rowMeans(d[,substr(nam,1,3)=="FAM"])
MAC.gro<-rowMeans(d[,substr(nam,1,3)=="LOY"])
MAC.rec<-rowMeans(d[,substr(nam,1,3)=="REC"])
MAC.her<-rowMeans(d[,substr(nam,1,3)=="HER"])
MAC.def<-rowMeans(d[,substr(nam,1,3)=="DEF"])
MAC.fai<-rowMeans(d[,substr(nam,1,3)=="FAI"])
MAC.pro<-rowMeans(d[,substr(nam,1,3)=="PRO"])

MAC.fam
MAC.gro
MAC.rec
MAC.her
MAC.def
MAC.fai
MAC.pro


names(d)[substr(names(d),1,2)=="Q4"]<-
  c("FAM1_MAC","LOY1_MAC","REC1_MAC","HER1_MAC","DEF1_MAC","FAI1_MAC","PRO1_MAC",              
    "FAM2_MAC","LOY2_MAC","REC2_MAC","HER2_MAC","DEF2_MAC","FAI2_MAC","PRO2_MAC",
    "FAM3_MAC","LOY3_MAC","REC3_MAC","HER3_MAC","DEF3_MAC","FAI3_MAC","PRO3_MAC")
names(d)

dnew<-data.frame(d[,c(1:44)],
                 MAC.fam,
                 MAC.gro,
                 MAC.rec,
                 MAC.her,
                 MAC.def,
                 MAC.fai,
                 MAC.pro,
                 ATTENTION_CHECK,
                 INT_DIR_CONTACT,
                 INT_QUARANTINE,
                 INT_STAY_HOME,
                 INT_WASH_HANDS,
                 INT_CANCEL_VAC,
                 INT_NEIGHBOR_SHOP,
                 INT_NOT_HOARD,
                 INT_NEIGHBOR_CHILD,
                 INT_SUPPORT_COMP,
                 COND,
                 CONDlabel,
                 d[,c(145,147:156,160)]
)

names(dnew)
nrow(dnew)

summary(dnew$duration)
summary(dnew$move.to.middle)

hist(dnew$duration[dnew$duration<1000])

#We will exclude those who took so little time to finish. They probably did not pay enough attention
dnew2<-dnew[dnew$duration>=240,]
nrow(dnew2)
#All responses in the dataset fit this inclusion criterion

#Discard people above 80 years of age
d<-dnew2
d<-d[d$Age<=80,]


png("age_comp.png",width=12,height=9,units="cm",res=600)
hist(d$Age[d$Country=="USA"],breaks=seq(10,110,5),col="#FF000080",main="",xlab="Age",border=NA)
hist(d$Age[d$Country=="India"],breaks=seq(10,110,5),col="#0000FF80",add=T,border=NA)
legend("topright",c("USA","India"),col=c("#FF000080","#0000FF80"),pch=15,bty="n")
dev.off()



library(psych)
library(lavaan)

#Firstly we need to check the factor structure in USA and India separatelz as specified in preregistration
intentions<-d[,substr(names(d),1,3)=="INT"]
names(intentions)

intentionsUSA<-intentions[d$Country=="USA",]
intentionsIndia<-intentions[d$Country=="India",]

#From the pilot, we suspected a 2-factor structure
model <- ' precaution   =~ INT_DIR_CONTACT + INT_STAY_HOME + INT_WASH_HANDS + INT_QUARANTINE + INT_CANCEL_VAC 
           prosociality =~ INT_NEIGHBOR_SHOP + INT_NEIGHBOR_CHILD + INT_SUPPORT_COMP + INT_NOT_HOARD'


fit <- cfa(model, data=intentions)
fitUSA <- cfa(model, data=intentionsUSA)
fitIndia <- cfa(model, data=intentionsIndia)

#Check the fit measures
summary(fit,fit.measures=TRUE)
summary(fitUSA,fit.measures=TRUE)
summary(fitIndia,fit.measures=TRUE)

#2 factor structure suggested
fa.parallel(intentions)
fa.parallel(intentionsUSA)
fa.parallel(intentionsIndia)

#We allow for oblimin rotation
INT_fact<-fa(intentions,2,rotate="oblimin",scores="tenBerge")
INT_factUSA<-fa(intentionsUSA,2,rotate="oblimin",scores="tenBerge")
INT_factIndia<-fa(intentionsIndia,2,rotate="oblimin",scores="tenBerge")

#Summary
INT_fact
INT_factUSA
INT_factIndia

#Quick visualization
diagram(INT_fact)
diagram(INT_factUSA)
diagram(INT_factIndia)

#Factor score extraction
PrecautionUSA<-INT_factUSA$scores[,1]
PrecautionIndia<-INT_factIndia$scores[,1]

ProsocialityUSA<-INT_factUSA$scores[,2]
ProsocialityIndia<-INT_factIndia$scores[,2]

str(intentionsUSA)
PrecautionUSA2<-rowMeans(intentionsUSA[,1:5])
PrecautionIndia2<-rowMeans(intentionsIndia[,1:5])

ProsocialityUSA2<-rowMeans(intentionsUSA[,6:9])
ProsocialityIndia2<-rowMeans(intentionsIndia[,6:9])

cor(PrecautionUSA,PrecautionUSA2)
cor(PrecautionIndia,PrecautionIndia2)

cor(ProsocialityUSA,ProsocialityUSA2)
cor(ProsocialityIndia,ProsocialityIndia2)


d$Precaution<-c(PrecautionUSA,PrecautionIndia)
d$Prosociality<-c(ProsocialityUSA,ProsocialityIndia)

cor(d$Precaution,INT_fact$scores[,1])
cor(d$Prosociality,INT_fact$scores[,2])

# Contrast between countries as a new variable
d$CountC<-as.numeric(as.integer(as.factor(d$Country))-1.5)
table(d$Country,d$CountC)

#write the data for futher use in structural models
write.table(d,"data.clean.txt",sep="\t",row.names = F)


#Check the correlaton between MAC dimensions
MAC<-d[,substr(names(d),1,3)=="MAC"]
cor(MAC)


summary(as.factor(d$Country))

table(d$Gender,d$Country)

tapply(d$Age, d$Country, mean)
tapply(d$Age, d$Country, sd)
tapply(d$Age, d$Country, range)

sum(d$Age[d$Country=="India"]>80)
sum(d$Age[d$Country=="India"]>100)

tapply(d$donation, d$Country, mean)
tapply(d$donation, d$Country, sd)
tapply(d$donation, d$Country, range)

