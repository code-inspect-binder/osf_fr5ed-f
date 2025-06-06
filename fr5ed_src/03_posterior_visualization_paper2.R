library(rethinking)
library(pracma)
library(viridis)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

load("posterior_samples_by_country.RData")

precis(mUSA,depth=2)
precis(mIndia,depth=2)

all.params<-read.table("params.txt",sep="\t",header = T)

hex<-all.params$hex[match(unique(all.params$col),all.params$col)]
cols<-all.params$col[match(unique(all.params$col),all.params$col)]

alpha<-"80"

all.params$hex<-paste("#",hex[match(all.params$col,cols)],alpha,sep="")

#divide the table to two tables - 1 for parameter estimates, 2 the difference between countries
describe<-all.params[all.params$table==1,]

#Organize tables for easy plotting
names(describe)
sum(describe$plot)
de<-describe[describe$plot==T,]

de<-de[order(de$block),]

de$n<-1:nrow(de)
de$ofs[is.na(de$ofs)]<-0

de$y<-de$n+de$block-1+cumsum(de$ofs)

#extract only the important columns
tos<-unique(as.character(de$nam))
tos<-tos[tos!=""]

cs<-sapply(tos,function(i){eval(parse(text=paste("ncol(postUSA$",i,")",sep="")))})
cs[is.na(cs)]<-1
cs
#first element of each set
cumsum(cs)-cs+1

eval(parse(text=(
  paste("postsU<-cbind(",paste(paste("postUSA$",tos,sep=""),collapse=","),")"))))

eval(parse(text=(
  paste("postsI<-cbind(",paste(paste("postIndia$",tos,sep=""),collapse=","),")"))))

#constant to add on both sides of the y axis to expand the plotting region a bit
const1<-1
#constant that is there for an extra space for axis
const2<-1.8

db<-lapply(1:max(de$block),function(i){de[de$block==i,]})

#calculate heights of block plots
ploth<-sapply(1:length(db),function(i){max(db[[i]]$y)-min(db[[i]]$y)})
rath<-c(const2,ploth+2*const1+const2)
rath

rat<-c(1,1,1)

w1<-7

pdf.r<-0.5
tiff("posterior_paper2.tif",width=18,height=14,units="cm",res=600,compression = "lzw")

laymat<-matrix(1:9,nr=3,byrow=T)

par(oma=c(1,1,0.5,1))
layout(laymat,
       widths=c(1.5,1,1),heights=rath[c(1,5,6)])

axes<-list(seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5))


xlims<-lapply(1:length(axes),function(i){c(min(axes[[i]])-diff(range(axes[[i]]))*0.1,max(axes[[i]])+diff(range(axes[[i]]))*0.1)})

par(mar=c(0.05,0.1,0.05,0.1),cex=0.9)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Model parameter",pos=4,font=2)

plot(NULL,ylim=c(const2,0),xlim=c(-1,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.8,"USA",font=2)

plot(NULL,ylim=c(const2,0),xlim=c(-1,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.8,"India",font=2)

for(block in 4:length(db)){
  
  dei<-db[[block]]

  #the text
  plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
  text(0,dei$y,dei$dependent,pos=4)
  text(0.40,dei$y,"~",pos=4)
  text(0.45,dei$y,paste(dei$predictor,dei$condition),pos=4)
  
  #text(0,max(dei$y)+const1+const2-0.5,ifelse(block==5,"","Parameters"),pos=4,font=2,xpd=T)
  
  
  #USA
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
    thispost<-postsU[,dei$n[i]]
    dens<-density(thispost)
    polX<-c(dens$x,rev(dens$x))
    polY<-c(dens$y,rev(-dens$y))
    ar1<-abs(polyarea(polX,polY))
    perc<-area/ar1
    polygon(polX,polY*perc+dei$y[i],col=dei$hex[i],border=col.pol,lwd=lwd.pol)
    #CI<-PI(thispost,prob = 0.89)
    #lines(CI,rep(dei$y[i],2),lwd=3,col="#000000")
    points(mean(thispost),dei$y[i],pch=16,cex=1,col="#FFFFFF")
    points(mean(thispost),dei$y[i],pch=1,cex=1,col="#000000")
    
  }
  
  rect(xlims[[block]][1],min(dei$y)-const1,xlims[[block]][2],max(dei$y)+const1,lwd=lwd.ax,border=col.ax,xpd=T)
  
  
  
  #India
  plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlim=xlims[[block]],bty="n")
  
  col.grid<-"#808080"
  lwd.grid<-1.5
  abline(h=dei$y,col=col.grid,lty=1,lwd=lwd.grid)
  
  segments(axes[[block]],max(dei$y)+const1,axes[[block]],max(dei$y)+tic+const1,lwd=lwd.ax,col=col.ax)
  text(axes[[block]],max(dei$y)+tic+const1+ofs,labels=axes[[block]],col=col.ax,cex=0.9,font=2)
  lines(range(axes[[block]]),rep(max(dei$y)+const1,2),lwd=lwd.ax,col=col.ax)
  
  segments(axes[[block]],min(dei$y)-const1,axes[[block]],max(dei$y)+const1,lwd=lwd.v,col=col.ax,lty=3)
  segments(0,min(dei$y)-const1,0,max(dei$y)+const1,lwd=lwd.v,col=col.ax,lty=1)
  
  #density areas are scaled within each block.
  area<-0.25*diff(xlims[[block]])
  
  for(i in 1:nrow(dei)){
    thispost<-postsI[,dei$n[i]]
    dens<-density(thispost)
    polX<-c(dens$x,rev(dens$x))
    polY<-c(dens$y,rev(-dens$y))
    ar1<-abs(polyarea(polX,polY))
    perc<-area/ar1
    polygon(polX,polY*perc+dei$y[i],col=dei$hex[i],border=col.pol,lwd=lwd.pol)
    #CI<-PI(thispost,prob = 0.89)
    #lines(CI,rep(dei$y[i],2),lwd=3,col="#000000")
    points(mean(thispost),dei$y[i],pch=16,cex=1,col="#FFFFFF")
    points(mean(thispost),dei$y[i],pch=1,cex=1,col="#000000")
  }
  
  rect(xlims[[block]][1],min(dei$y)-const1,xlims[[block]][2],max(dei$y)+const1,lwd=lwd.ax,border=col.ax,xpd=T)

}

dev.off()










