#Load model with the contrast between countries for each estimate and without this contrast
library(rethinking)
load("posterior_samples_by_country.RData")
load("posterior_samples_contrasts.RData")
load("posterior_samples_single.RData")


scaleA<-function(x){(x-attributes(dat$A)[[1]])/attributes(dat$A)[[2]]}
descaleA<-function(x){x*attributes(dat$A)[[2]]+attributes(dat$A)[[1]]}


scaleMfm<-function(x){(x-attributes(dat$Mfm)[[1]])/attributes(dat$Mfm)[[2]]}
descaleMfm<-function(x){x*attributes(dat$Mfm)[[2]]+attributes(dat$Mfm)[[1]]}

scaleMg<-function(x){(x-attributes(dat$Mg)[[1]])/attributes(dat$Mg)[[2]]}
descaleMg<-function(x){x*attributes(dat$Mg)[[2]]+attributes(dat$Mg)[[1]]}

scaleMr<-function(x){(x-attributes(dat$Mr)[[1]])/attributes(dat$Mr)[[2]]}
descaleMr<-function(x){x*attributes(dat$Mr)[[2]]+attributes(dat$Mr)[[1]]}

scaleMh<-function(x){(x-attributes(dat$Mh)[[1]])/attributes(dat$Mh)[[2]]}
descaleMh<-function(x){x*attributes(dat$Mh)[[2]]+attributes(dat$Mh)[[1]]}

scaleMd<-function(x){(x-attributes(dat$Md)[[1]])/attributes(dat$Md)[[2]]}
descaleMd<-function(x){x*attributes(dat$Md)[[2]]+attributes(dat$Md)[[1]]}

scaleMfi<-function(x){(x-attributes(dat$Mfi)[[1]])/attributes(dat$Mfi)[[2]]}
descaleMfi<-function(x){x*attributes(dat$Mfi)[[2]]+attributes(dat$Mfi)[[1]]}

scaleMp<-function(x){(x-attributes(dat$Mp)[[1]])/attributes(dat$Mp)[[2]]}
descaleMp<-function(x){x*attributes(dat$Mp)[[2]]+attributes(dat$Mp)[[1]]}

scaleD<-function(x){(x-attributes(dat$Dgr)[[1]])/attributes(dat$Dgr)[[2]]}
descaleD<-function(x){x*attributes(dat$Dgr)[[2]]+attributes(dat$Dgr)[[1]]}




colU<-"#FF0000"
colI<-"#0000FF"

colU2<-"#800000"
colI2<-"#000080"

ages<-seq(18,80,1)



tiff("lines.tif",width=14,height=8,units="cm",res=600,compression = "lzw")

layout(matrix(1:10,nrow=2,byrow = T),widths=c(1.3,1,1,1,0.35),heights = c(1,1.3))

pcex<-0.8

par(mgp=c(2,0.7,0))

par(mar=c(0.5,3.5,3,0.5))

plot(d$Age,d$MAC.fam,type="n",xlim=c(18,80),ylim=c(0,100),xlab="",ylab="Score",xaxt="n",bty="n")
points(d$Age[d$Country=="USA"],d$MAC.fam[d$Country=="USA"],col=col.alpha(colU,0.10),pch=16,cex=pcex)
points(d$Age[d$Country=="India"],d$MAC.fam[d$Country=="India"],col=col.alpha(colI,0.10),pch=16,cex=pcex)

predsU<-sapply(scaleA(ages),function(x){descaleMfm(rowMeans(postUSA$aG_Fam)+x*postUSA$bAge_Fam)})
predsI<-sapply(scaleA(ages),function(x){descaleMfm(rowMeans(postIndia$aG_Fam)+x*postIndia$bAge_Fam)})

muU<-apply(predsU,2,mean)
CIU<-apply(predsU,2,PI)

muI<-apply(predsI,2,mean)
CII<-apply(predsI,2,PI)

shade(CIU,ages,col=col.alpha(colU2,0.25))
shade(CII,ages,col=col.alpha(colI2,0.25))

lines(ages,muU,col=colU)
lines(ages,muI,col=colI)

cexm<-1.1
title(main="MAC family",adj=0,cex.main=cexm)




par(mar=c(0.5,0.5,3,0.5))

plot(d$Age,d$MAC.gro,type="n",xlim=c(18,80),ylim=c(0,100),xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
points(d$Age[d$Country=="USA"],d$MAC.gro[d$Country=="USA"],col=col.alpha(colU,0.10),pch=16,cex=pcex)
points(d$Age[d$Country=="India"],d$MAC.gro[d$Country=="India"],col=col.alpha(colI,0.10),pch=16,cex=pcex)

predsU<-sapply(scaleA(ages),function(x){descaleMg(rowMeans(postUSA$aG_Gro)+x*postUSA$bAge_Gro)})
predsI<-sapply(scaleA(ages),function(x){descaleMg(rowMeans(postIndia$aG_Gro)+x*postIndia$bAge_Gro)})

muU<-apply(predsU,2,mean)
CIU<-apply(predsU,2,PI)

muI<-apply(predsI,2,mean)
CII<-apply(predsI,2,PI)

shade(CIU,ages,col=col.alpha(colU2,0.25))
shade(CII,ages,col=col.alpha(colI2,0.25))

lines(ages,muU,col=colU)
lines(ages,muI,col=colI)

title(main="MAC group",adj=0,cex.main=cexm)



par(mar=c(0.5,0.5,3,0.5))

plot(d$Age,d$MAC.rec,type="n",xlim=c(18,80),ylim=c(0,100),xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
points(d$Age[d$Country=="USA"],d$MAC.rec[d$Country=="USA"],col=col.alpha(colU,0.10),pch=16,cex=pcex)
points(d$Age[d$Country=="India"],d$MAC.rec[d$Country=="India"],col=col.alpha(colI,0.10),pch=16,cex=pcex)

predsU<-sapply(scaleA(ages),function(x){descaleMr(rowMeans(postUSA$aG_Rec)+x*postUSA$bAge_Rec)})
predsI<-sapply(scaleA(ages),function(x){descaleMr(rowMeans(postIndia$aG_Rec)+x*postIndia$bAge_Rec)})

muU<-apply(predsU,2,mean)
CIU<-apply(predsU,2,PI)

muI<-apply(predsI,2,mean)
CII<-apply(predsI,2,PI)

shade(CIU,ages,col=col.alpha(colU2,0.25))
shade(CII,ages,col=col.alpha(colI2,0.25))

lines(ages,muU,col=colU)
lines(ages,muI,col=colI)

title(main="MAC reciprocity",adj=0,cex.main=cexm)


par(mar=c(0.5,0.5,3,0.5))

plot(d$Age,d$MAC.her,type="n",xlim=c(18,80),ylim=c(0,100),xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
points(d$Age[d$Country=="USA"],d$MAC.her[d$Country=="USA"],col=col.alpha(colU,0.10),pch=16,cex=pcex)
points(d$Age[d$Country=="India"],d$MAC.her[d$Country=="India"],col=col.alpha(colI,0.10),pch=16,cex=pcex)

predsU<-sapply(scaleA(ages),function(x){descaleMh(rowMeans(postUSA$aG_Her)+x*postUSA$bAge_Her)})
predsI<-sapply(scaleA(ages),function(x){descaleMh(rowMeans(postIndia$aG_Her)+x*postIndia$bAge_Her)})

muU<-apply(predsU,2,mean)
CIU<-apply(predsU,2,PI)

muI<-apply(predsI,2,mean)
CII<-apply(predsI,2,PI)

shade(CIU,ages,col=col.alpha(colU2,0.25))
shade(CII,ages,col=col.alpha(colI2,0.25))

lines(ages,muU,col=colU)
lines(ages,muI,col=colI)

title(main="MAC heroism",adj=0,cex.main=cexm)




par(mar=c(0,0,0,0))
plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
legend("bottomright",c("USA","India"),pch=15,col=c(col.alpha(colU,0.2),col.alpha(colI,0.2)),bty="n")
legend("bottomright",c("USA","India"),pch=0,col=c(2,4),bty="n")



par(mar=c(3.5,3.5,3,0.5))

plot(d$Age,d$MAC.fam,type="n",xlim=c(18,80),ylim=c(0,100),xlab="Age",ylab="Score",bty="n")
points(d$Age[d$Country=="USA"],d$MAC.def[d$Country=="USA"],col=col.alpha(colU,0.10),pch=16,cex=pcex)
points(d$Age[d$Country=="India"],d$MAC.def[d$Country=="India"],col=col.alpha(colI,0.10),pch=16,cex=pcex)

predsU<-sapply(scaleA(ages),function(x){descaleMd(rowMeans(postUSA$aG_Def)+x*postUSA$bAge_Def)})
predsI<-sapply(scaleA(ages),function(x){descaleMd(rowMeans(postIndia$aG_Def)+x*postIndia$bAge_Def)})

muU<-apply(predsU,2,mean)
CIU<-apply(predsU,2,PI)

muI<-apply(predsI,2,mean)
CII<-apply(predsI,2,PI)

shade(CIU,ages,col=col.alpha(colU2,0.25))
shade(CII,ages,col=col.alpha(colI2,0.25))

lines(ages,muU,col=colU)
lines(ages,muI,col=colI)

title(main="MAC deference",adj=0,cex.main=cexm)



par(mar=c(3.5,0.5,3,0.5))

plot(d$Age,d$MAC.fam,type="n",xlim=c(18,80),ylim=c(0,100),xlab="Age",ylab="",bty="n",yaxt="n")
points(d$Age[d$Country=="USA"],d$MAC.fai[d$Country=="USA"],col=col.alpha(colU,0.10),pch=16,cex=pcex)
points(d$Age[d$Country=="India"],d$MAC.fai[d$Country=="India"],col=col.alpha(colI,0.10),pch=16,cex=pcex)
predsU<-sapply(scaleA(ages),function(x){descaleMfi(rowMeans(postUSA$aG_Fai)+x*postUSA$bAge_Fai)})
predsI<-sapply(scaleA(ages),function(x){descaleMfi(rowMeans(postIndia$aG_Fai)+x*postIndia$bAge_Fai)})

muU<-apply(predsU,2,mean)
CIU<-apply(predsU,2,PI)

muI<-apply(predsI,2,mean)
CII<-apply(predsI,2,PI)

shade(CIU,ages,col=col.alpha(colU2,0.25))
shade(CII,ages,col=col.alpha(colI2,0.25))

lines(ages,muU,col=colU)
lines(ages,muI,col=colI)

title(main="MAC fairness",adj=0,cex.main=cexm)



par(mar=c(3.5,0.5,3,0.5))

plot(d$Age,d$MAC.fam,type="n",xlim=c(18,80),ylim=c(0,100),xlab="Age",ylab="",bty="n",yaxt="n")
points(d$Age[d$Country=="USA"],d$MAC.pro[d$Country=="USA"],col=col.alpha(colU,0.10),pch=16,cex=pcex)
points(d$Age[d$Country=="India"],d$MAC.pro[d$Country=="India"],col=col.alpha(colI,0.10),pch=16,cex=pcex)

predsU<-sapply(scaleA(ages),function(x){descaleMp(rowMeans(postUSA$aG_Pro)+x*postUSA$bAge_Pro)})
predsI<-sapply(scaleA(ages),function(x){descaleMp(rowMeans(postIndia$aG_Pro)+x*postIndia$bAge_Pro)})

muU<-apply(predsU,2,mean)
CIU<-apply(predsU,2,PI)

muI<-apply(predsI,2,mean)
CII<-apply(predsI,2,PI)

shade(CIU,ages,col=col.alpha(colU2,0.25))
shade(CII,ages,col=col.alpha(colI2,0.25))

lines(ages,muU,col=colU)
lines(ages,muI,col=colI)

title(main="MAC property",adj=0,cex.main=cexm)



par(mar=c(3.5,0.5,3,0.5))

plot(d$Age,d$danger.for.participant,type="n",xlim=c(18,80),ylim=c(0,100),xlab="Age",ylab="",bty="n",yaxt="n")
points(d$Age[d$Country=="USA"],d$danger.for.participant[d$Country=="USA"],col=col.alpha(colU,0.10),pch=16,cex=pcex)
points(d$Age[d$Country=="India"],d$danger.for.participant[d$Country=="India"],col=col.alpha(colI,0.10),pch=16,cex=pcex)

predsU<-sapply(scaleA(ages),function(x){descaleD(rowMeans(postUSA$aG_Dang)+x*postUSA$bAge_Dang)})
predsI<-sapply(scaleA(ages),function(x){descaleD(rowMeans(postIndia$aG_Dang)+x*postIndia$bAge_Dang)})

muU<-apply(predsU,2,mean)
CIU<-apply(predsU,2,PI)

muI<-apply(predsI,2,mean)
CII<-apply(predsI,2,PI)

shade(CIU,ages,col=col.alpha(colU2,0.25))
shade(CII,ages,col=col.alpha(colI2,0.25))

lines(ages,muU,col=colU)
lines(ages,muI,col=colI)

title(main="COVID danger",adj=0,cex.main=cexm)


dev.off()

