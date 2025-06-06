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

set.seed(42)
m1pre <- ulam(
  alist(
    P ~ dnorm(muP,sigmaP),

        muP<-aGP[G]+bAP*A+aCP[CD]+bConP*CC+bFamP*Mfm+bGroP*Mg+bRecP*Mr+bHerP*Mh+bDefP*Md+bFaiP*Mfi+bProP*Mp+bDgP*Dgr,

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
    
    #sigmas
    sigmaP~dexp(1)

  ) , data=dat, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))


set.seed(42)
m1soc <- ulam(
  alist(

    S ~ dnorm(muS,sigmaS),

    muS<-aGS[G]+bAS*A+aCS[CD]+bConS*CC+bFamS*Mfm+bGroS*Mg+bRecS*Mr+bHerS*Mh+bDefS*Md+bFaiS*Mfi+bProS*Mp+bDgS*Dgr,
    
    #Priors
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
    sigmaS~dexp(1)
    
  ) , data=dat, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))


set.seed(42)
m1don <- ulam(
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
    
    #sigmas
    sigmaD~dexp(1)
    
  ) , data=dat, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))



#Contrast models
set.seed(42)
mCpre <- ulam(
  alist(
    
    P ~ dnorm(muP,sigmaP),
    
    muP<-aGP[G]+aGPC[G]*C+bAP*A+bAPC*A*C+aCP[CD]+aCPC[CD]*C+bConP*CC+bConPC*CC*C+bFamP*Mfm+bFamPC*Mfm*C+bGroP*Mg+bGroPC*Mg*C+bRecP*Mr+bRecPC*Mr*C+bHerP*Mh+bHerPC*Mh*C+bDefP*Md+bDefPC*Md*C+bFaiP*Mfi+bFaiPC*Mfi*C+bProP*Mp+bProPC*Mp*C+bDgP*Dgr+bDgPC*Dgr*C,
    
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
    
    #sigmas
    sigmaP~dexp(1),
    
    #Contrasts priors
    #Precaution
    aGPC[G]~dnorm(0,0.2),
    bAPC~dnorm(0,0.2),
    
    aCPC[CD]~dnorm(0,0.2),
    
    bConPC~dnorm(0,0.2),
    bFamPC~dnorm(0,0.2),
    bGroPC~dnorm(0,0.2),
    bRecPC~dnorm(0,0.2),
    bHerPC~dnorm(0,0.2),
    bDefPC~dnorm(0,0.2),
    bFaiPC~dnorm(0,0.2),
    bProPC~dnorm(0,0.2),
    bDgPC~dnorm(0,0.2)
    
  ) , data=dat, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))

set.seed(42)
mCsoc <- ulam(
  alist(
    
    S ~ dnorm(muS,sigmaS),

    muS<-aGS[G]+aGSC[G]*C+bAS*A+bASC*A*C+aCS[CD]+aCSC[CD]*C+bConS*CC+bConSC*CC*C+bFamS*Mfm+bFamSC*Mfm*C+bGroS*Mg+bGroSC*Mg*C+bRecS*Mr+bRecSC*Mr*C+bHerS*Mh+bHerSC*Mh*C+bDefS*Md+bDefSC*Md*C+bFaiS*Mfi+bFaiSC*Mfi*C+bProS*Mp+bProSC*Mp*C+bDgS*Dgr+bDgSC*Dgr*C,
    
    #Priors
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
    sigmaS~dexp(1),
   
    #Contrasts priors
    #Prosociality
    aGSC[G]~dnorm(0,0.2),
    bASC~dnorm(0,0.2),
    
    aCSC[CD]~dnorm(0,0.2),
    
    bConSC~dnorm(0,0.2),
    bFamSC~dnorm(0,0.2),
    bGroSC~dnorm(0,0.2),
    bRecSC~dnorm(0,0.2),
    bHerSC~dnorm(0,0.2),
    bDefSC~dnorm(0,0.2),
    bFaiSC~dnorm(0,0.2),
    bProSC~dnorm(0,0.2),
    bDgSC~dnorm(0,0.2)
    
  ) , data=dat, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))

set.seed(42)
mCdon <- ulam(
  alist(
    
    D ~ dnorm(muD,sigmaD),
    
    muD<-aG[G]+aGC[G]*C+bA*A+bAC*A*C+bP*P+bPC*P*C+bS*S+bSC*S*C+aC[CD]+aCC[CD]*C+bCon*CC+bConC*CC*C+bFam*Mfm+bFamC*Mfm*C+bGro*Mg+bGroC*Mg*C+bRec*Mr+bRecC*Mr*C+bHer*Mh+bHerC*Mh*C+bDef*Md+bDefC*Md*C+bFai*Mfi+bFaiC*Mfi*C+bPro*Mp+bProC*Mp*C+bDg*Dgr+bDgC*Dgr*C,
    
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
    
    #sigmas
    sigmaD~dexp(1),
    
    #Contrasts priors
    #Donation
    aGC[G]~dnorm(0,0.2),
    bAC~dnorm(0,0.2),
    
    bPC~dnorm(0,0.2),
    bSC~dnorm(0,0.2),
    
    aCC[CD]~dnorm(0,0.2),
    
    bConC~dnorm(0,0.2),
    bFamC~dnorm(0,0.2),
    bGroC~dnorm(0,0.2),
    bRecC~dnorm(0,0.2),
    bHerC~dnorm(0,0.2),
    bDefC~dnorm(0,0.2),
    bFaiC~dnorm(0,0.2),
    bProC~dnorm(0,0.2),
    bDgC~dnorm(0,0.2)
    
  ) , data=dat, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))



save.image(file="model_estimates_precaut_prosoc.RData")

load("model_estimates_precaut_prosoc.RData")

compare(m1pre,mCpre)
compare(m1soc,mCsoc)
compare(m1don,mCdon)
