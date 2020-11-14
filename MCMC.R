##----------------------Exemplo MCMC----------------------##

pars <- c(0.1, 0.2, 0.03, 0.03, 0.01, 0.01)
set.seed(2)
phy <- tree.bisse(pars, max.t=60, x0=0)
plot(phy)


## First, create a likelihood function:
lik <- make.bisse(phy, phy$tip.state)
lik(pars)

##MCMC
tmp <- mcmc(lik, pars, nsteps=50, w=.1)

names(tmp)
x<-tmp$lambda0
hist(x)
plot(1:50,x,type="l")


##-------------------Com os meus dados----------------##
library(diversitree)

pars_real<-c(0.06648875, 0.1933984, 0.00000009, 0.02994558, 0.3847630)
phy_real<-(MCC)



lik_real<- make.geosse(phy_real, state_area)
lik_real(pars_real)

lik_real_2<-constrain(lik_real, xA ~ xB, sAB ~ 0)


prior_t<- make.prior.exponential(pars_real)

prior_2<- make.prior.exponential(1/(2*(pars_real[1]-pars_real[3])) )

prior_3<-make.prior.exponential(0.5)

#x<-c(-Inf,-Inf,-Inf,-Inf,-Inf)
#y<-c(Inf,Inf,Inf,Inf,Inf)



MCMC_Falcons<-mcmc(lik_real_2, pars_real, nsteps=1000, w=0.5, prior=prior_3, lower=-Inf, upper=Inf)

MCMC_Falcons <- subset(MCMC_Falcons, i > 200)

#------------------------ Figuras -------------------------#
MCMC_Falcons$r<- MCMC_Falcons$sA-MCMC_Falcons$xB

col <- c("#004165", "#eaab00","#618e02")
profiles.plot(MCMC_Falcons[c("sA", "sB","r")], col.line=col, las=1,legend.pos="topright")
abline(v=c(.1, .2), col=col)



