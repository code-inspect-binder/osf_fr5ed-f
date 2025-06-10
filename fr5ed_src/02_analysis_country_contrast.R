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
mC <- ulam(
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
    
    
    #Model of precaution and prosociality
    P ~ dnorm(muP,sigmaP),
    S ~ dnorm(muS,sigmaS),

    muP<-aGP[G]+aGPC[G]*C+bAP*A+bAPC*A*C+aCP[CD]+aCPC[CD]*C+bConP*CC+bConPC*CC*C+bFamP*Mfm+bFamPC*Mfm*C+bGroP*Mg+bGroPC*Mg*C+bRecP*Mr+bRecPC*Mr*C+bHerP*Mh+bHerPC*Mh*C+bDefP*Md+bDefPC*Md*C+bFaiP*Mfi+bFaiPC*Mfi*C+bProP*Mp+bProPC*Mp*C+bDgP*Dgr+bDgPC*Dgr*C,
    
    muS<-aGS[G]+aGSC[G]*C+bAS*A+bASC*A*C+aCS[CD]+aCSC[CD]*C+bConS*CC+bConSC*CC*C+bFamS*Mfm+bFamSC*Mfm*C+bGroS*Mg+bGroSC*Mg*C+bRecS*Mr+bRecSC*Mr*C+bHerS*Mh+bHerSC*Mh*C+bDefS*Md+bDefSC*Md*C+bFaiS*Mfi+bFaiSC*Mfi*C+bProS*Mp+bProSC*Mp*C+bDgS*Dgr+bDgSC*Dgr*C,
    
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
    
    mu_Fam<-aG_Fam[G]+aG_FamC[G]*C+bAge_Fam*A+bAge_FamC*A*C,
    mu_Gro<-aG_Gro[G]+aG_GroC[G]*C+bAge_Gro*A+bAge_GroC*A*C,
    mu_Rec<-aG_Rec[G]+aG_RecC[G]*C+bAge_Rec*A+bAge_RecC*A*C,
    mu_Her<-aG_Her[G]+aG_HerC[G]*C+bAge_Her*A+bAge_HerC*A*C,
    mu_Def<-aG_Def[G]+aG_DefC[G]*C+bAge_Def*A+bAge_DefC*A*C,
    mu_Fai<-aG_Fai[G]+aG_FaiC[G]*C+bAge_Fai*A+bAge_FaiC*A*C,
    mu_Pro<-aG_Pro[G]+aG_ProC[G]*C+bAge_Pro*A+bAge_ProC*A*C,
    
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
    mu_Dang<-aG_Dang[G]+aG_DangC[G]*C+bAge_Dang*A+bAge_DangC*A*C,
    
    #priors
    aG_Dang[G]~dnorm(0,0.2),
    bAge_Dang~dnorm(0,0.5),
    
    sigma_Dang~dexp(1),
    
    
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
    bDgC~dnorm(0,0.2),
    
    
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
    bDgPC~dnorm(0,0.2),
    
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
    bDgSC~dnorm(0,0.2),
    
    
    #contrasts in MAC dimension predictors
    aG_FamC[G]~dnorm(0,0.2),
    aG_GroC[G]~dnorm(0,0.2),
    aG_RecC[G]~dnorm(0,0.2),
    aG_HerC[G]~dnorm(0,0.2),
    aG_DefC[G]~dnorm(0,0.2),
    aG_FaiC[G]~dnorm(0,0.2),
    aG_ProC[G]~dnorm(0,0.2),
    
    bAge_FamC~dnorm(0,0.2),
    bAge_GroC~dnorm(0,0.2),
    bAge_RecC~dnorm(0,0.2),
    bAge_HerC~dnorm(0,0.2),
    bAge_DefC~dnorm(0,0.2),
    bAge_FaiC~dnorm(0,0.2),
    bAge_ProC~dnorm(0,0.2),
    
    #danger contrasts
    aG_DangC[G]~dnorm(0,0.2),
    bAge_DangC~dnorm(0,0.2)
    
    ) , data=dat, chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=10,adapt_delta=0.95))

#Sumarize the model
precis(mC,depth=2)

#Sample posetrior and prior for graphical comparison
postC<-extract.samples(mC)

set.seed(42)
prioC<-extract.prior(mC,n=10000)

save.image(file="posterior_samples_contrasts.RData")

