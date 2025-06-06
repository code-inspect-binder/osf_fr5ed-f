# library(rethinking)
# library(pracma)
# library(viridis)
# 
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }
# 
# load("posterior_samples_contrasts.RData")
# 
# precis(mC,depth=2)

nam<-names(postC)

par.in<-unlist(lapply(postC,ncol))
par.in[is.na(par.in)]<-1

rep.par<-rep(nam,par.in)
npar<-length(rep.par)

#We estimate
npar #parameters

#We leave all repetitions of the same label blank
rep.par2<-rep.par
rep.par2[is.na(match(1:npar,match(nam,rep.par)))]<-""
rep.par2

#We create a list of parameters to visualize and fill the labels etc to the .txt file in any table editor (Excel)
write.table(data.frame(nam=rep.par2),"params_empty.txt",row.names=F,sep="\t")

#Fill the .txt file in Excel

all.params<-read.table("params.txt",sep="\t",header = T)

hex<-all.params$hex[match(unique(all.params$col),all.params$col)]
cols<-all.params$col[match(unique(all.params$col),all.params$col)]

alpha<-"80"

all.params$hex<-paste("#",hex[match(all.params$col,cols)],alpha,sep="")

#divide the table to two tables - 1 for parameter estimates, 2 the difference between countries
describe<-all.params[all.params$table==1,]
countcon<-all.params[all.params$table==2,]

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

cs<-sapply(tos,function(i){eval(parse(text=paste("ncol(postC$",i,")",sep="")))})
cs[is.na(cs)]<-1
cs
#first element of each set
cumsum(cs)-cs+1

eval(parse(text=(
  paste("posts<-cbind(",paste(paste("postC$",tos,sep=""),collapse=","),")"))))

#Organize tables of contrasts
names(countcon)
sum(countcon$plot)
cc<-countcon[countcon$plot==T,]

cc<-cc[order(cc$block),]

cc$n<-1:nrow(cc)
cc$ofs[is.na(cc$ofs)]<-0

cc$y<-cc$n+cc$block-1+cumsum(cc$ofs)

#extract only the important columns
tosc<-unique(as.character(cc$nam))
tosc<-tosc[tosc!=""]

eval(parse(text=(
  paste("postsc<-cbind(",paste(paste("postC$",tosc,sep=""),collapse=","),")"))))

#constant to add on both sides of the y axis to expand the plotting region a bit
const1<-1
#constant that is there for an extra space for axis
const2<-1.8

db<-lapply(1:max(de$block),function(i){de[de$block==i,]})
dc<-lapply(1:max(cc$block),function(i){cc[cc$block==i,]})

#calculate heights of block plots
ploth<-sapply(1:length(db),function(i){max(db[[i]]$y)-min(db[[i]]$y)})
rath<-c(const2,ploth+2*const1+const2)
rath

rat<-c(1.25,1,1)

w1<-7

pdf.r<-1.05
tiff("posterior_contrast.tif",width=(w1/rat[1])*sum(rat)*pdf.r*1.2,height=39*pdf.r,units="cm",res=600)


par(oma=c(1,1,0.5,2))
layout(matrix(rep(1:(length(rath)*3),rep(rat,length(rath))),nr=length(rath),byrow=T),
       widths=rat,heights=rath)

axes<-list(seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5),
           seq(-0.5,0.5,l=5))

xlims<-lapply(1:length(axes),function(i){c(min(axes[[i]])-diff(range(axes[[i]]))*0.1,max(axes[[i]])+diff(range(axes[[i]]))*0.1)})

par(mar=rep(0.05,4),cex=0.9)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Parameters",pos=4,font=2)

plot(NULL,ylim=c(const2,0),xlim=c(-1,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.8,"Posterior distributions of estimates",font=2)

plot(NULL,ylim=c(const2,0),xlim=c(-1,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.8,"Posterior of USA-India contrasts",font=2)

for(block in 1:length(db)){
  
  dei<-db[[block]]
  dec<-dc[[block]]
  
  plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
  text(0,dei$y,dei$dependent,pos=4)
  text(0.34,dei$y,"~",pos=4)
  text(0.385,dei$y,paste(dei$predictor,dei$condition),pos=4)
  
  text(0,max(dei$y)+const1+const2-0.5,ifelse(block==5,"","Parameters"),pos=4,font=2,xpd=T)
  
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
    #CI<-PI(thispost,prob = 0.89)
    #lines(CI,rep(dei$y[i],2),lwd=3,col="#000000")
    points(mean(thispost),dei$y[i],pch=16,cex=1,col="#FFFFFF")
    points(mean(thispost),dei$y[i],pch=1,cex=1,col="#000000")
    
  }
  
  rect(xlims[[block]][1],min(dei$y)-const1,xlims[[block]][2],max(dei$y)+const1,lwd=lwd.ax,border=col.ax,xpd=T)
  
  
  #plot contrasts estimates
  plot(NULL,ylim=c(max(dec$y)+const1+const2,min(dec$y)-const1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlim=xlims[[block]],bty="n")
  
  abline(h=dec$y,col=col.grid,lty=1,lwd=lwd.grid)
  
  segments(axes[[block]],max(dec$y)+const1,axes[[block]],max(dec$y)+tic+const1,lwd=lwd.ax,col=col.ax)
  text(axes[[block]],max(dec$y)+tic+const1+ofs,labels=axes[[block]],col=col.ax,cex=0.9,font=2)
  lines(range(axes[[block]]),rep(max(dec$y)+const1,2),lwd=lwd.ax,col=col.ax)

  segments(axes[[block]],min(dec$y)-const1,axes[[block]],max(dec$y)+const1,lwd=lwd.v,col=col.ax,lty=3)
  segments(0,min(dec$y)-const1,0,max(dec$y)+const1,lwd=lwd.v,col=col.ax,lty=1)
  
  #Draw density polygons
  #density areas are scaled within each block.
  area<-0.25*diff(xlims[[block]])
  
  for(i in 1:nrow(dec)){
    thispost<-postsc[,dec$n[i]]
    dens<-density(thispost)
    polX<-c(dens$x,rev(dens$x))
    polY<-c(dens$y,rev(-dens$y))
    ar1<-abs(polyarea(polX,polY))
    perc<-area/ar1
    polygon(polX,polY*perc+dei$y[i],col=dec$hex[i],border=col.pol,lwd=lwd.pol)
    #CI<-PI(thispost,prob = 0.89)
    #lines(CI,rep(dei$y[i],2),lwd=3,col="#000000")
    points(mean(thispost),dec$y[i],pch=16,cex=1,col="#FFFFFF")
    points(mean(thispost),dec$y[i],pch=1,cex=1,col="#000000")
    
  }
  
  rect(xlims[[block]][1],min(dec$y)-const1,xlims[[block]][2],max(dec$y)+const1,lwd=lwd.ax,border=col.ax,xpd=T)
  
}

dev.off()


# restab1<-precis(mC,prob=0.89,depth=2)
# restab2<-precis(mC,prob=0.97,depth=2)
# 
# restab<-round(cbind(restab1[,1:4],restab2[,3:4]),2)
# 
# restab<-data.frame(parameter=rownames(restab),regression=paste(all.params[,4],all.params[,5],all.params[,6]),restab)
# 
# 
# names(restab)[5:8]<-c("89% CI low","89% CI up","97% CI low","97% CI up")
# 
# write.table(restab,"results_numeric_contrast.txt",sep="\t",row.names = F)
# 
# 



