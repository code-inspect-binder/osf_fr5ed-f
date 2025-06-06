library(vioplot)

#load("posterior_samples_single.RData")

names(d)[45:51]

MAC<-list(
  d$MAC.fam[d$Gender=="Man"&d$Country=="USA"],
  d$MAC.fam[d$Gender=="Woman"&d$Country=="USA"],
  d$MAC.fam[d$Gender=="Man"&d$Country=="India"],
  d$MAC.fam[d$Gender=="Woman"&d$Country=="India"],
  
  d$MAC.gro[d$Gender=="Man"&d$Country=="USA"],
  d$MAC.gro[d$Gender=="Woman"&d$Country=="USA"],
  d$MAC.gro[d$Gender=="Man"&d$Country=="India"],
  d$MAC.gro[d$Gender=="Woman"&d$Country=="India"],
  
  d$MAC.rec[d$Gender=="Man"&d$Country=="USA"],
  d$MAC.rec[d$Gender=="Woman"&d$Country=="USA"],
  d$MAC.rec[d$Gender=="Man"&d$Country=="India"],
  d$MAC.rec[d$Gender=="Woman"&d$Country=="India"],
  
  d$MAC.her[d$Gender=="Man"&d$Country=="USA"],
  d$MAC.her[d$Gender=="Woman"&d$Country=="USA"],
  d$MAC.her[d$Gender=="Man"&d$Country=="India"],
  d$MAC.her[d$Gender=="Woman"&d$Country=="India"],
  
  d$MAC.def[d$Gender=="Man"&d$Country=="USA"],
  d$MAC.def[d$Gender=="Woman"&d$Country=="USA"],
  d$MAC.def[d$Gender=="Man"&d$Country=="India"],
  d$MAC.def[d$Gender=="Woman"&d$Country=="India"],
  
  d$MAC.fai[d$Gender=="Man"&d$Country=="USA"],
  d$MAC.fai[d$Gender=="Woman"&d$Country=="USA"],
  d$MAC.fai[d$Gender=="Man"&d$Country=="India"],
  d$MAC.fai[d$Gender=="Woman"&d$Country=="India"],
  
  d$MAC.pro[d$Gender=="Man"&d$Country=="USA"],
  d$MAC.pro[d$Gender=="Woman"&d$Country=="USA"],
  d$MAC.pro[d$Gender=="Man"&d$Country=="India"],
  d$MAC.pro[d$Gender=="Woman"&d$Country=="India"]
)

str(MAC)


tiff("vioplots.tif",width=20,height=8,units="cm",res=600,compression = "lzw")
par(mar=c(3,3.5,2.5,0))
ats<-1:28+rep(0.5*(0:6),each=4)
layout(matrix(c(1,2),nrow=1),widths=c(1,0.15))
par(mgp=c(2.1,0.7,0.3))
vioplot(MAC,col=rep(c("#00AAFF80","#FFA32180"),times=14),xlim=c(0.5,28.5),xaxs="i",bty="n",axes=F,names=rep("",28),at=ats,ylab="Mean MAC score")
rect(0.5,-20,max(ats)+0.5,-0.5,col="white",xpd=T,border=NA)
par(mgp=c(2.1,0.7,0))
for(i in 1:14){
axis(1,at=ats[((i-1)*2+1):((i-1)*2+2)],labels=rep("",2),col=ifelse(i%%2==1,2,4))
}
mtext("USA",1,col=2,line=0.7,adj=0)
mtext("India",1,col=4,line=0.7,adj=1)
labs<-c("Family","Group","Reciprocity","Heroism","Deference","Fainess","Property")
ats2<-0:6*4+rep(0.5*(0:6))+2.5
par(mgp=c(0,0,0))
axis(3,at=ats2,labels=labs,tck=0,col=NA)
mtext("MAC dimension",adj=0.5,line=1.3,font=2,cex=1.2)

par(mar=c(0,0,0,0))
plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
legend("center",c("Men","Women"),pch=15,col=c("#00AAFF80","#FFA32180"),bty="n")
legend("center",c("Men","Women"),pch=0,col=c(1,1),bty="n")

dev.off()

