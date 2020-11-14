library(diversitree)
library(qpcR)

#-------------------------------------------------------------------
# TESTE
#-------------------------------------------------------------------

#Filogenia teste
phy_1<-(mil_trees_Hacket_Falco[[1]])#Uma filogenia das 1000 pra testar


#------------------Tabela de estados permanente------------------#

#AET
state_area<-as.numeric(thresh_areas_falco_10[,1])#Transformando o data frame em vetor numerico
names(state_area)<-rownames(thresh_areas_falco_10)#colocando os nomes das especies

#Geografico
state_GEO<-as.numeric(thresh_trop_falco_10[,1])#Transformando o data frame em vetor numerico
names(state_GEO)<-rownames(thresh_trop_falco_10)#colocando os nomes das especies


#-------------------------GEOSSE--------------------------#

lik_teste<-make.geosse(phy_1, state_area)
p_teste<-starting.point.geosse(phy_1)
fit_teste<-find.mle(lik_teste,p_teste, method="subplex")
like<-logLik(fit_teste)


#------------------Constrain---------------------#

lik.d <- constrain(lik_teste, dA ~ dB, sA ~ sB, sAB~0)
fit.d <- find.mle(lik.d, p_teste[-c(7,2,3)])
resultados[1,c(1,4:5,6)]<-fit.d$par
resultados[1,1]<-NA


#-------------------------------------------------------------------
# Todas as árvores
#-------------------------------------------------------------------

resultados<-as.data.frame(matrix(NA,2,11))
colnames(resultados)<-c("Sa","Sb","Sab","Xa","Xb","Da","Db","lnLik","AIC","delta.AIC","Weights")

dir.create("Resultados Falconidae")#Criar diretorio para colocar os 1000 resultados
setwd("F:/Lorena/Mestrado/Produtividade Falcons/Resultados Falconidae")


for(i in 1:3){
  print(i)
  tree<- mil_trees_Hacket_Falco[[i]] #Arvores
  
  #Modelo sem constrain
  lik<-make.geosse(tree, state_area)
  p<-starting.point.geosse(tree)
  fit<-find.mle(lik,p, method="subplex")
  AIC_1<-AIC(fit)
  
  resultados[i,c(1:7)]<- fit$par #(vetor de parametros) GEOSSE sem constrain
  resultados[i,8]<-fit$lnLik #Likelihood
  resultados[i,9]<-AIC_1 #AIC
  
  #Primeiro constrain (sAB=0)
  lik.d_2 <- constrain(lik, sAB ~ 0) #primeiro constrain
  fit.d_2 <- find.mle(lik.d_2, p[-3])
  AIC_2<-AIC(fit.d_2)
  
  resultados[i+1,c(1:2, 4:7)]<- fit.d_2$par #resultados do primeiro constrain
  resultados[i+1,8]<-fit.d_2$lnLik
  resultados[i+1,9]<-AIC_2
  resultados[i+1,3]<-NA
  
  #Segundo constrain (sAB=0)
  lik.d_3 <- constrain(lik, xA ~ xB, sAB ~ 0 )
  fit.d_3 <- find.mle(lik.d_3, p[-c(5,3)])
  AIC_3<-AIC(fit.d_3)
  
  resultados[i+2,c(1:2, 4, 6:7)]<- fit.d_3$par 
  resultados[i+2,8]<-fit.d_3$lnLik
  resultados[i+2,9]<-AIC_3
  resultados[i+2, c(3,5)]<-NA
  
  #Terceiro constrain (sAB=0)
  lik.d_4 <- constrain(lik, dA ~ dB, sAB ~ 0 )
  fit.d_4 <- find.mle(lik.d_4, p[-c(7,3)]) 
  AIC_4<-AIC(fit.d_4)
  
  resultados[i+3,c(1:2, 4:6)]<- fit.d_4$par 
  resultados[i+3,8]<-fit.d_4$lnLik
  resultados[i+3,9]<-AIC_4
  resultados[i+3,c(3,7)]<-NA
  
  #Quarto constrain (sAB=0)
  lik.d_5 <- constrain(lik, sA ~ sB, sAB ~ 0 )
  fit.d_5 <- find.mle(lik.d_5, p[-c(2,3)])
  AIC_5<-AIC(fit.d_5)
  
  resultados[i+4,c(1, 4:7)]<- fit.d_5$par 
  resultados[i+4,8]<-fit.d_5$lnLik
  resultados[i+4,9]<-AIC_5
  resultados[i+4, c(2,3)]<-NA
  
  #Quinto constrain (sAB=0)
  lik.d_6 <- constrain(lik, xA ~ xB, dA ~ dB, sAB ~ 0 )
  fit.d_6 <- find.mle(lik.d_6, p[-c(5,3,7)])
  AIC_6<-AIC(fit.d_6)
  
  resultados[i+5,c(1:2, 4, 6)]<- fit.d_6$par 
  resultados[i+5,8]<-fit.d_6$lnLik
  resultados[i+5,9]<-AIC_6
  resultados[i+5, c(3,5,7)]<-NA
  
  #Sexto constrain (sAB=0)
  lik.d_7 <- constrain(lik, sA ~ sB, dA ~ dB, sAB ~ 0 )
  fit.d_7 <- find.mle(lik.d_7, p[-c(2,3,7)])
  AIC_7<-AIC(fit.d_7)
  
  resultados[i+6,c(1, 4:6)]<- fit.d_7$par 
  resultados[i+6,8]<-fit.d_7$lnLik
  resultados[i+6,9]<-AIC_7
  resultados[i+6, c(2,3,7)]<-NA
  
  #Setimo constrain (sAB=0)
  lik.d_8 <- constrain(lik, sA ~ sB, xA ~ xB, sAB ~ 0 )
  fit.d_8 <- find.mle(lik.d_8, p[-c(2,3,5)])
  AIC_8<-AIC(fit.d_8)
  
  resultados[i+7,c(1, 4, 6:7)]<- fit.d_8$par 
  resultados[i+7,8]<-fit.d_8$lnLik
  resultados[i+7,9]<-AIC_8
  resultados[i+7, c(2,3,5)]<-NA
  
  #Oitavo constrain (sAB=0)
  lik.d_9 <- constrain(lik, sA ~ sB, xA ~ xB, dA ~ dB, sAB ~ 0 )
  fit.d_9 <- find.mle(lik.d_9, p[-c(2,3,5,7)])
  AIC_9<-AIC(fit.d_9)
  
  resultados[i+8,c(1, 4, 6)]<- fit.d_9$par 
  resultados[i+8,8]<-fit.d_9$lnLik
  resultados[i+8,9]<-AIC_9
  resultados[i+8, c(2,3,5,7)]<-NA
  
  #Primeiro constrain (sAB livre)
  lik.d_10 <- constrain(lik, dA ~ dB)
  fit.d_10 <- find.mle(lik.d_10, p[-7])
  AIC_10<-AIC(fit.d_10)
  
  resultados[i+9,c(1:6)]<- fit.d_10$par 
  resultados[i+9,8]<-fit.d_10$lnLik
  resultados[i+9,9]<-AIC_10
  resultados[i+9,7]<-NA
  
  #Segundo constrain (sAB livre)
  lik.d_11 <- constrain(lik, sA ~ sB)
  fit.d_11 <- find.mle(lik.d_11, p[-2])
  AIC_11<-AIC(fit.d_11)
  
  resultados[i+10,c(1, 3:7)]<- fit.d_11$par 
  resultados[i+10,8]<-fit.d_11$lnLik
  resultados[i+10,9]<-AIC_11
  resultados[i+10,2]<-NA
  
  #Terceiro constrain (sAB livre)
  lik.d_12 <- constrain(lik, xA ~ xB)
  fit.d_12 <- find.mle(lik.d_12, p[-5])
  AIC_12<-AIC(fit.d_12)
  
  resultados[i+11,c(1:4, 6:7)]<- fit.d_12$par 
  resultados[i+11,8]<-fit.d_12$lnLik
  resultados[i+11,9]<-AIC_12
  resultados[i+11,5]<-NA
  
  #Quarto constrain (sAB livre)
  lik.d_13 <- constrain(lik, xA ~ xB, dA ~ dB)
  fit.d_13 <- find.mle(lik.d_13, p[-c(7,5)])
  AIC_13<-AIC(fit.d_13)
  
  resultados[i+12,c(1:4, 6)]<- fit.d_13$par 
  resultados[i+12,8]<-fit.d_13$lnLik
  resultados[i+12,9]<-AIC_13
  resultados[i+12,c(5,7)]<-NA
  
  #Quinto constrain (sAB livre)
  lik.d_14 <- constrain(lik, sA ~ sB, dA ~ dB)
  fit.d_14 <- find.mle(lik.d_14, p[-c(7,2)])
  AIC_14<-AIC(fit.d_14)
  
  resultados[i+13,c(1, 3, 4:6)]<- fit.d_14$par 
  resultados[i+13,8]<-fit.d_14$lnLik
  resultados[i+13,9]<-AIC_14
  resultados[i+13,c(2,7)]<-NA
  
  #Sexto constrain (sAB livre)
  lik.d_15 <- constrain(lik, sA ~ sB, xA ~ xB)
  fit.d_15 <- find.mle(lik.d_15, p[-c(5,2)])
  AIC_15<-AIC(fit.d_15)
  
  resultados[i+14,c(1, 3:4, 6:7)]<- fit.d_15$par 
  resultados[i+14,8]<-fit.d_15$lnLik
  resultados[i+14,9]<-AIC_15
  resultados[i+14,c(2,5)]<-NA
  
  #Sétimo constrain (sAB livre)
  lik.d_16 <- constrain(lik, sA ~ sB, xA ~ xB, dA ~ dB)
  fit.d_16 <- find.mle(lik.d_16, p[-c(7,5,2)])
  AIC_16<-AIC(fit.d_16)
  
  resultados[i+15,c(1, 3:4, 6)]<- fit.d_16$par 
  resultados[i+15,8]<-fit.d_16$lnLik
  resultados[i+15,9]<-AIC_16
  resultados[i+15,c(2,5,7)]<-NA
  
  #Akaike 
  All_AICs<-c(AIC_1,AIC_2,AIC_3,AIC_4,AIC_5,AIC_6,AIC_7,AIC_8,AIC_9,AIC_10,AIC_11,AIC_12,AIC_13,AIC_14,AIC_15,AIC_16) #concatena o resultado de todos os constrains...
  weig<-akaike.weights(All_AICs) #e faz o Akaike disso
  
  #Resultados do Akaike para cada modelo
  resultados[i,10]<- weig$deltaAIC[[1]] #resultados do delta-Akaike de cada modelo vao na ultima coluna depois de todos os resultados dos constrains
  resultados[i+1,10]<- weig$deltaAIC[[2]]
  resultados[i+2,10]<- weig$deltaAIC[[3]]
  resultados[i+3,10]<- weig$deltaAIC[[4]]
  resultados[i+4,10]<- weig$deltaAIC[[5]]
  resultados[i+5,10]<- weig$deltaAIC[[6]]
  resultados[i+6,10]<- weig$deltaAIC[[7]]
  resultados[i+7,10]<- weig$deltaAIC[[8]]
  resultados[i+8,10]<- weig$deltaAIC[[9]]
  resultados[i+9,10]<- weig$deltaAIC[[10]]
  resultados[i+10,10]<- weig$deltaAIC[[11]]
  resultados[i+11,10]<- weig$deltaAIC[[12]]
  resultados[i+12,10]<- weig$deltaAIC[[13]]
  resultados[i+13,10]<- weig$deltaAIC[[14]]
  resultados[i+14,10]<- weig$deltaAIC[[15]]
  resultados[i+15,10]<- weig$deltaAIC[[16]]
  
  resultados[i,11]<- weig$weights[[1]] #resultados dos weights para cada modelo
  resultados[i+1,11]<- weig$weights[[2]]
  resultados[i+2,11]<- weig$weights[[3]]
  resultados[i+3,11]<- weig$weights[[4]]
  resultados[i+4,11]<- weig$weights[[5]]
  resultados[i+5,11]<- weig$weights[[6]]
  resultados[i+6,11]<- weig$weights[[7]]
  resultados[i+7,11]<- weig$weights[[8]]
  resultados[i+8,11]<- weig$weights[[9]]
  resultados[i+9,11]<- weig$weights[[10]]
  resultados[i+10,11]<- weig$weights[[11]]
  resultados[i+11,11]<- weig$weights[[12]]
  resultados[i+12,11]<- weig$weights[[13]]
  resultados[i+13,11]<- weig$weights[[14]]
  resultados[i+14,11]<- weig$weights[[15]]
  resultados[i+15,11]<- weig$weights[[16]]
  
  
  write.table(resultados,paste('resu_filo_',i,'.txt',sep=""),sep="\t")
  
}



