library(corrplot)

d<-read.table("data.clean.txt",sep="\t",header=T,stringsAsFactors = F)
names(d)
Country<-d$Country

d<-d[c("Age","MAC.fam","MAC.gro","MAC.rec","MAC.her","MAC.def","MAC.fai","MAC.pro","Precaution","Prosociality","donation")]
names(d)[11]<-"Donation"

dUSA<-d[Country=="USA",]
dIn<-d[Country=="India",]

cortab<-cor(d)
cortabUSA<-cor(dUSA)
cortabIn<-cor(dIn)

tiff("corrplot_all.tif",width=16,height=16,units="cm",res=600,compression="lzw")
corrplot.mixed(cortab,upper="ellipse",tl.pos="lt",diag="u")
dev.off()

tiff("corrplot_USA.tif",width=16,height=16,units="cm",res=600,compression="lzw")
corrplot.mixed(cortabUSA,upper="ellipse",tl.pos="lt",diag="u")
dev.off()

tiff("corrplot_In.tif",width=16,height=16,units="cm",res=600,compression="lzw")
corrplot.mixed(cortabIn,upper="ellipse",tl.pos="lt",diag="u")
dev.off()
