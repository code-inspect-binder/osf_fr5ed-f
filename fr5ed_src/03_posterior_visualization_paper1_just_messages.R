library(rethinking)
library(pracma)
library(viridis)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

load("posterior_samples_single.RData")
load("posterior_samples_by_country.RData")

precis(m1,depth=2)

all.params<-read.table("params.txt",sep="\t",header = T)

hex<-all.params$hex[match(unique(all.params$col),all.params$col)]
cols<-all.params$col[match(unique(all.params$col),all.params$col)]

alpha<-"80"

all.params$hex<-paste("#",hex[match(all.params$col,cols)],alpha,sep="")

#divide the table to two tables - 1 for parameter estimates, 2 the difference between countries
describe<-all.params[all.params$table==1,]

names(describe)
sum(describe$plot)
de<-describe[describe$plot==T,]

de<-de[order(de$block),]

de$n<-1:nrow(de)
de$ofs[is.na(de$ofs)]<-0

de$y<-de$n+de$block-1+cumsum(de$ofs)


#We use a different (wide) layout here, so we copy Donations y coordinates to other p
de$y[de$dependent=="precaution"]<-de$y[de$dependent=="donation"&de$predictor!="precaution"&de$predictor!="prosociality"]
de$y[de$dependent=="prosociality"]<-de$y[de$dependent=="donation"&de$predictor!="precaution"&de$predictor!="prosociality"]


#extract only the important columns
tos<-unique(as.character(de$nam))
tos<-tos[tos!=""]

cs<-sapply(tos,function(i){eval(parse(text=paste("ncol(post1$",i,")",sep="")))})
cs[is.na(cs)]<-1
cs
#first element of each set
cumsum(cs)-cs+1

eval(parse(text=(
  paste("posts<-cbind(",paste(paste("post1$",tos,sep=""),collapse=","),")"))))

#We include national samples in this visualization
eval(parse(text=(
  paste("postsU<-cbind(",paste(paste("postUSA$",tos,sep=""),collapse=","),")"))))

eval(parse(text=(
  paste("postsI<-cbind(",paste(paste("postIndia$",tos,sep=""),collapse=","),")"))))


#constant to add on both sides of the y axis to expand the plotting region a bit
const1<-1
#constant that is there for an extra space for axis
const2<-1.8

db<-lapply(1:max(de$block),function(i){de[de$block==i,]})
ploth<-sapply(1:length(db),function(i){max(db[[i]]$y)-min(db[[i]]$y)})
rath<-c(const2,ploth+2*const1+const2)
rath

rat<-c(1,1)

w1<-20

laymat<-matrix(1:10,nr=2,byrow=T)

pdf.r<-0.5
#pdf("posterior_paper.pdf",width=(w1/rat[1])*sum(rat)*pdf.r,height=10*pdf.r)


tiff("posterior_paper_messages.tif",width=18,height=8,units="cm",res=600,compression = "lzw")

par(oma=c(1,1,0.5,0.5))
layout(laymat,widths=c(0.8,1,1,1,0.5),heights=c(0.1,1))

axes<-list(seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5))

xlims<-lapply(1:length(axes),function(i){c(min(axes[[i]])-diff(range(axes[[i]]))*0.1,max(axes[[i]])+diff(range(axes[[i]]))*0.1)})

par(mar=c(0.05,0.15,0.05,0.15),cex=0.9)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Message",pos=4,font=2,xpd=NA)

plot(NULL,ylim=c(const2,0),xlim=c(-1,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Donation",font=1,xpd=NA)

plot(NULL,ylim=c(const2,0),xlim=c(-1,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Precaution",font=1,xpd=NA)
text(0,const2-1.6,"Dependent variable",font=2,xpd=NA)

plot(NULL,ylim=c(const2,0),xlim=c(-1,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Prosociality",font=1)

plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")

dei<-db[[1]]
dei<-dei[dei$predictor=="message",]

plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
#text(0,dei$y,dei$dependent,pos=4)
#text(0,dei$y,"~",pos=4)
#text(0.1,dei$y,paste(dei$predictor,dei$condition),pos=4)
text(0.1,dei$y,substr(dei$condition,2,nchar(as.character(dei$condition))-1),pos=4)

for(block in 1:3){
  
  dei<-db[[block]]
  dei<-dei[dei$predictor=="message",]
  
  plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlim=xlims[[block]],bty="n")
  
  col.grid<-"#808080"
  lwd.grid<-1.5
  abline(h=dei$y,col=col.grid,lty=1,lwd=lwd.grid)
  
  #draw own axis
  lwd.ax<-1.5
  col.ax<-"#202020"
  
  tic<-0.35
  ofs<-0.65
  segments(axes[[block]],max(dei$y)+const1,axes[[block]],max(dei$y)+tic+const1,lwd=lwd.ax,col=col.ax)
  text(axes[[block]],max(dei$y)+tic+const1+ofs,labels=axes[[block]],col=col.ax,cex=0.9,font=2)
  lines(range(axes[[block]]),rep(max(dei$y)+const1,2),lwd=lwd.ax,col=col.ax)
  
  lwd.v<-1.5
  segments(axes[[block]],min(dei$y)-const1,axes[[block]],max(dei$y)+const1,lwd=lwd.v,col=col.ax,lty=3)
  segments(0,min(dei$y)-const1,0,max(dei$y)+const1,lwd=lwd.v,col=col.ax,lty=1)
  
  #Draw density polygons
  lwd.pol<-1.5
  col.pol<-col.grid
  
  #density areas are scaled within each block.
  area<-0.25*diff(xlims[[block]])
  
  for(i in 1:nrow(dei)){
    thispost<-posts[,dei$n[i]]
    dens<-density(thispost)
    polX<-c(dens$x,rev(dens$x))
    polY<-c(dens$y,rev(-dens$y))
    ar1<-abs(polyarea(polX,polY))
    perc<-area/ar1
    polygon(polX,polY*perc+dei$y[i],col=dei$hex[i],border=col.pol,lwd=lwd.pol)
  }
  for(i in 1:nrow(dei)){
    thispost<-postsU[,dei$n[i]]
    points(mean(thispost),dei$y[i],pch=16,cex=1,col="#FF000050")
    points(mean(thispost),dei$y[i],pch=1,cex=1,col="#FF0000")
  }
  
  for(i in 1:nrow(dei)){
    thispost<-postsI[,dei$n[i]]
    points(mean(thispost),dei$y[i],pch=16,cex=1,col="#0000FF50")
    points(mean(thispost),dei$y[i],pch=1,cex=1,col="#0000FF")
  }
  
  for(i in 1:nrow(dei)){
    thispost<-posts[,dei$n[i]]
    points(mean(thispost),dei$y[i],pch=16,cex=1,col="#FFFFFF")
    points(mean(thispost),dei$y[i],pch=1,cex=1,col="#000000")
  }
  
  rect(xlims[[block]][1],min(dei$y)-const1,xlims[[block]][2],max(dei$y)+const1,lwd=lwd.ax,border=col.ax,xpd=T)
  
}

plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
legend("center",c("Total","USA","India"),pch=16,col=c("white","#FF000050","#0000FF50"),bty="n")
legend("center",c("Total","USA","India"),pch=1,col=c(1,2,4),bty="n")


dev.off()


