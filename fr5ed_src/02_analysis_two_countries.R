library(rethinking)

d<-read.table("data.clean.txt",sep="\t",header=T,stringsAsFactors = F)
names(d)

dat <- list(
  C=d$CountC,

  A = standardize(d$Age),
  G = as.integer(as.factor(d$Gender)),  #1 Man, 2 Woman
  CD = as.integer(d$COND),
  Mfm = standardize(d$MAC.fam),
  Mg = standardize(d$MAC.gro),
  Mr = standardize(d$MAC.rec),
  Mh = standardize(d$MAC.her),
  Md = standardize(d$MAC.def),
  Mfi = standardize(d$MAC.fai),
  Mp = standardize(d$MAC.pro),
  P = standardize(d$Precaution),
  S = standardize(d$Prosociality),
  D = standardize(d$donation),
  
  Dgr=standardize(d$danger.for.participant),
  
  CC = ifelse(d$COND==1,0,ifelse(d$COND==2,0,ifelse(d$COND==3,0,ifelse(d$COND==4,standardize(d$MAC.fam),ifelse(d$COND==5,standardize(d$MAC.gro),ifelse(d$COND==6,standardize(d$MAC.rec),ifelse(d$COND==7,standardize(d$MAC.her),ifelse(d$COND==8,standardize(d$MAC.def),ifelse(d$COND==9,standardize(d$MAC.fai),ifelse(d$COND==10,standardize(d$MAC.pro),NA)))))))))) #MAC dimension concordant with the condition
  
)

datUSA <- list(
  C = dat$C[d$Country=="USA"],
  
  A = dat$A[d$Country=="USA"],
  G = dat$G[d$Country=="USA"],  #1 Man, 2 Woman
  CD = dat$CD[d$Country=="USA"],
  Mfm = dat$Mfm[d$Country=="USA"],
  Mg = dat$Mg[d$Country=="USA"],
  Mr = dat$Mr[d$Country=="USA"],
  Mh = dat$Mh[d$Country=="USA"],
  Md = dat$Md[d$Country=="USA"],
  Mfi = dat$Mfi[d$Country=="USA"],
  Mp = dat$Mp[d$Country=="USA"],
  P = dat$P[d$Country=="USA"],
  S = dat$S[d$Country=="USA"],
  D = dat$D[d$Country=="USA"],
  
  Dgr = dat$Dgr[d$Country=="USA"],
  
  CC = dat$CC[d$Country=="USA"]#MAC dimension concordant with the condition
)

datIndia <- list(
  C = dat$C[d$Country=="India"],
  
  A = dat$A[d$Country=="India"],
  G = dat$G[d$Country=="India"],  #1 Man, 2 Woman
  CD = dat$CD[d$Country=="India"],
  Mfm = dat$Mfm[d$Country=="India"],
  Mg = dat$Mg[d$Country=="India"],
  Mr = dat$Mr[d$Country=="India"],
  Mh = dat$Mh[d$Country=="India"],
  Md = dat$Md[d$Country=="India"],
  Mfi = dat$Mfi[d$Country=="India"],
  Mp = dat$Mp[d$Country=="India"],
  P = dat$P[d$Country=="India"],
  S = dat$S[d$Country=="India"],
  D = dat$D[d$Country=="India"],
  
  Dgr = dat$Dgr[d$Country=="India"],
  
  CC = dat$CC[d$Country=="India"]#MAC dimension concordant with the condition
)



set.seed(42)
mUSA <- ulam(
  alist(
    
    D ~ dnorm(muD,sigmaD),
    
    muD<-aG[G]+bA*A+bP*P+bS*S+aC[CD]+bCon*CC+bFam*Mfm+bGro*Mg+bRec*Mr+bHer*Mh+bDef*Md+bFai*Mfi+bPro*Mp+bDg*Dgr,
    
    #Donation
    aG[G]~dnorm(0,0.2),
    bA~dnorm(0,0.5),
    
    bP~dnorm(0,0.5),
    bS~dnorm(0,0.5),
    
    aC[CD]~dnorm(0,0.2),
    
    bCon~dnorm(0,0.5),
    bFam~dnorm(0,0.5),
    bGro~dnorm(0,0.5),
    bRec~dnorm(0,0.5),
    bHer~dnorm(0,0.5),
    bDef~dnorm(0,0.5),
    bFai~dnorm(0,0.5),
    bPro~dnorm(0,0.5),
    
    bDg~dnorm(0,0.5),
    
    
    #Model of precaution and prosociality
    P ~ dnorm(muP,sigmaP),
    S ~ dnorm(muS,sigmaS),

    muP<-aGP[G]+bAP*A+aCP[CD]+bConP*CC+bFamP*Mfm+bGroP*Mg+bRecP*Mr+bHerP*Mh+bDefP*Md+bFaiP*Mfi+bProP*Mp+bDgP*Dgr,
    
    muS<-aGS[G]+bAS*A+aCS[CD]+bConS*CC+bFamS*Mfm+bGroS*Mg+bRecS*Mr+bHerS*Mh+bDefS*Md+bFaiS*Mfi+bProS*Mp+bDgS*Dgr,
    
    #Priors
    #Precaution
    aGP[G]~dnorm(0,0.2),
    bAP~dnorm(0,0.5),
    
    aCP[CD]~dnorm(0,0.2),
    
    bConP~dnorm(0,0.5),
    bFamP~dnorm(0,0.5),
    bGroP~dnorm(0,0.5),
    bRecP~dnorm(0,0.5),
    bHerP~dnorm(0,0.5),
    bDefP~dnorm(0,0.5),
    bFaiP~dnorm(0,0.5),
    bProP~dnorm(0,0.5),
    
    bDgP~dnorm(0,0.5),
    
    #ProSociality
    aGS[G]~dnorm(0,0.2),
    bAS~dnorm(0,0.5),
    
    aCS[CD]~dnorm(0,0.2),
    
    bConS~dnorm(0,0.5),
    bFamS~dnorm(0,0.5),
    bGroS~dnorm(0,0.5),
    bRecS~dnorm(0,0.5),
    bHerS~dnorm(0,0.5),
    bDefS~dnorm(0,0.5),
    bFaiS~dnorm(0,0.5),
    bProS~dnorm(0,0.5),
    
    bDgS~dnorm(0,0.5),
    
    #sigmas
    sigmaD~dexp(1),
    sigmaP~dexp(1),
    sigmaS~dexp(1),
    
    #Models of MAC dimensions
    Mfm ~ dnorm(mu_Fam,sigma_Fam),
    Mg ~ dnorm(mu_Gro,sigma_Gro),
    Mr ~ dnorm(mu_Rec,sigma_Rec),
    Mh ~ dnorm(mu_Her,sigma_Her),
    Md ~ dnorm(mu_Def,sigma_Def),
    Mfi ~ dnorm(mu_Fai,sigma_Fai),
    Mp ~ dnorm(mu_Pro,sigma_Pro),
    
    mu_Fam<-aG_Fam[G]+bAge_Fam*A,
    mu_Gro<-aG_Gro[G]+bAge_Gro*A,
    mu_Rec<-aG_Rec[G]+bAge_Rec*A,
    mu_Her<-aG_Her[G]+bAge_Her*A,
    mu_Def<-aG_Def[G]+bAge_Def*A,
    mu_Fai<-aG_Fai[G]+bAge_Fai*A,
    mu_Pro<-aG_Pro[G]+bAge_Pro*A,
    
    #priors of MAC intercepts and slopes
    aG_Fam[G]~dnorm(0,0.2),
    aG_Gro[G]~dnorm(0,0.2),
    aG_Rec[G]~dnorm(0,0.2),
    aG_Her[G]~dnorm(0,0.2),
    aG_Def[G]~dnorm(0,0.2),
    aG_Fai[G]~dnorm(0,0.2),
    aG_Pro[G]~dnorm(0,0.2),
    
    bAge_Fam~dnorm(0,0.5),
    bAge_Gro~dnorm(0,0.5),
    bAge_Rec~dnorm(0,0.5),
    bAge_Her~dnorm(0,0.5),
    bAge_Def~dnorm(0,0.5),
    bAge_Fai~dnorm(0,0.5),
    bAge_Pro~dnorm(0,0.5),
    
    #sigmas
    sigma_Fam~dexp(1),
    sigma_Gro~dexp(1),
    sigma_Rec~dexp(1),
    sigma_Her~dexp(1),
    sigma_Def~dexp(1),
    sigma_Fai~dexp(1),
    sigma_Pro~dexp(1),
    
    #model of how dangerous COVID is perceived for the participant
    Dgr ~ dnorm(mu_Dang,sigma_Dang),
    mu_Dang<-aG_Dang[G]+bAge_Dang*A,
    
    #priors
    aG_Dang[G]~dnorm(0,0.2),
    bAge_Dang~dnorm(0,0.5),
    
    sigma_Dang~dexp(1)
    
    ) , data=datUSA, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))


set.seed(42)
mIndia <- ulam(
  alist(
    
    D ~ dnorm(muD,sigmaD),
    
    muD<-aG[G]+bA*A+bP*P+bS*S+aC[CD]+bCon*CC+bFam*Mfm+bGro*Mg+bRec*Mr+bHer*Mh+bDef*Md+bFai*Mfi+bPro*Mp+bDg*Dgr,
    
    #Donation
    aG[G]~dnorm(0,0.2),
    bA~dnorm(0,0.5),
    
    bP~dnorm(0,0.5),
    bS~dnorm(0,0.5),
    
    aC[CD]~dnorm(0,0.2),
    
    bCon~dnorm(0,0.5),
    bFam~dnorm(0,0.5),
    bGro~dnorm(0,0.5),
    bRec~dnorm(0,0.5),
    bHer~dnorm(0,0.5),
    bDef~dnorm(0,0.5),
    bFai~dnorm(0,0.5),
    bPro~dnorm(0,0.5),
    
    bDg~dnorm(0,0.5),
    
    
    #Model of precaution and prosociality
    P ~ dnorm(muP,sigmaP),
    S ~ dnorm(muS,sigmaS),
    
    muP<-aGP[G]+bAP*A+aCP[CD]+bConP*CC+bFamP*Mfm+bGroP*Mg+bRecP*Mr+bHerP*Mh+bDefP*Md+bFaiP*Mfi+bProP*Mp+bDgP*Dgr,
    
    muS<-aGS[G]+bAS*A+aCS[CD]+bConS*CC+bFamS*Mfm+bGroS*Mg+bRecS*Mr+bHerS*Mh+bDefS*Md+bFaiS*Mfi+bProS*Mp+bDgS*Dgr,
    
    #Priors
    #Precaution
    aGP[G]~dnorm(0,0.2),
    bAP~dnorm(0,0.5),
    
    aCP[CD]~dnorm(0,0.2),
    
    bConP~dnorm(0,0.5),
    bFamP~dnorm(0,0.5),
    bGroP~dnorm(0,0.5),
    bRecP~dnorm(0,0.5),
    bHerP~dnorm(0,0.5),
    bDefP~dnorm(0,0.5),
    bFaiP~dnorm(0,0.5),
    bProP~dnorm(0,0.5),
    
    bDgP~dnorm(0,0.5),
    
    #ProSociality
    aGS[G]~dnorm(0,0.2),
    bAS~dnorm(0,0.5),
    
    aCS[CD]~dnorm(0,0.2),
    
    bConS~dnorm(0,0.5),
    bFamS~dnorm(0,0.5),
    bGroS~dnorm(0,0.5),
    bRecS~dnorm(0,0.5),
    bHerS~dnorm(0,0.5),
    bDefS~dnorm(0,0.5),
    bFaiS~dnorm(0,0.5),
    bProS~dnorm(0,0.5),
    
    bDgS~dnorm(0,0.5),
    
    #sigmas
    sigmaD~dexp(1),
    sigmaP~dexp(1),
    sigmaS~dexp(1),
    
    #Models of MAC dimensions
    Mfm ~ dnorm(mu_Fam,sigma_Fam),
    Mg ~ dnorm(mu_Gro,sigma_Gro),
    Mr ~ dnorm(mu_Rec,sigma_Rec),
    Mh ~ dnorm(mu_Her,sigma_Her),
    Md ~ dnorm(mu_Def,sigma_Def),
    Mfi ~ dnorm(mu_Fai,sigma_Fai),
    Mp ~ dnorm(mu_Pro,sigma_Pro),
    
    mu_Fam<-aG_Fam[G]+bAge_Fam*A,
    mu_Gro<-aG_Gro[G]+bAge_Gro*A,
    mu_Rec<-aG_Rec[G]+bAge_Rec*A,
    mu_Her<-aG_Her[G]+bAge_Her*A,
    mu_Def<-aG_Def[G]+bAge_Def*A,
    mu_Fai<-aG_Fai[G]+bAge_Fai*A,
    mu_Pro<-aG_Pro[G]+bAge_Pro*A,
    
    #priors of MAC intercepts and slopes
    aG_Fam[G]~dnorm(0,0.2),
    aG_Gro[G]~dnorm(0,0.2),
    aG_Rec[G]~dnorm(0,0.2),
    aG_Her[G]~dnorm(0,0.2),
    aG_Def[G]~dnorm(0,0.2),
    aG_Fai[G]~dnorm(0,0.2),
    aG_Pro[G]~dnorm(0,0.2),
    
    bAge_Fam~dnorm(0,0.5),
    bAge_Gro~dnorm(0,0.5),
    bAge_Rec~dnorm(0,0.5),
    bAge_Her~dnorm(0,0.5),
    bAge_Def~dnorm(0,0.5),
    bAge_Fai~dnorm(0,0.5),
    bAge_Pro~dnorm(0,0.5),
    
    #sigmas
    sigma_Fam~dexp(1),
    sigma_Gro~dexp(1),
    sigma_Rec~dexp(1),
    sigma_Her~dexp(1),
    sigma_Def~dexp(1),
    sigma_Fai~dexp(1),
    sigma_Pro~dexp(1),
    
    #model of how dangerous COVID is perceived for the participant
    Dgr ~ dnorm(mu_Dang,sigma_Dang),
    mu_Dang<-aG_Dang[G]+bAge_Dang*A,
    
    #priors
    aG_Dang[G]~dnorm(0,0.2),
    bAge_Dang~dnorm(0,0.5),
    
    sigma_Dang~dexp(1)
    
  ) , data=datIndia, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))

#Sumarize the model
precis(mUSA,depth=2)
precis(mIndia,depth=2)

#Sample posetrior and prior for graphical comparison
postUSA<-extract.samples(mUSA)
postIndia<-extract.samples(mIndia)

save.image(file="posterior_samples_by_country.RData")

