setwd("F:/Lorena/Mestrado/Produtividade Falcons/Objetos no R")

All_resu_geosse_AET_Falco<-read.table("Resu_AET_Falco.txt", header = T)
All_resu_geosse_GEO_Falco<-read.table("Resu_GEO_Falco.txt", header = T)


matrix_Falco_AET<-All_resu_geosse_AET_Falco[1:16000,c(3:9,15)]
matrix_Falco_GEO<-All_resu_geosse_GEO_Falco[1:16000,c(3:9,15)]

#teste<-matrix_Falco_AET[1:3,]


Model_average<-function(teste){ 

  mat_teste<-matrix(,16000,8) 
  colnames(mat_teste)<-c(colnames(teste))
  
  
for(i in 1:nrow(teste)){
  
  W_value<-teste[i,8]
  
  
  for(j in 1:ncol(teste)){
    
    teta_value<-teste[i,j]
    
    
    if(j==3){
      
      if (is.na(teste[i,3])){
        
        teta_value<-0
        
      } else{teta_value<-teste[i,j]}
      
    }
    
      if (j==2 | j==5 |j==7){
       
        if(is.na(teste[i,j])){
          
          teta_value<- teste[i,j-1]
          
        } else{teta_value<-teste[i,j]}
      }
    
       if (j==8){
         
         teta_value<-1
       }
    
    produt<-W_value*teta_value
    
    mat_teste [i,j]<- produt
    
  }
 
  
}

mat_teste_2<- mat_teste [,1:7]

all_columns_sum<- colSums(mat_teste_2) 

all_columns_sum

}

medium_model_Falco_AET<-Model_average(matrix_Falco_AET)

medium_model_Falco_GEO<-Model_average(matrix_Falco_GEO)

