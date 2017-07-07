# install.packages("Rglpk")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("reshape2")
install.packages("data.table")

library(Rglpk)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

Portfopti_score <- function(Return_data,score_data,alpha,omega){
  ## Inputs: Returndata---The logarithm return data
  ## score_data---The score of stocks
  ## alpha----The confident Level
  ## omega----The up bound of CDaR
  
  data_dim <- dim(Return_data)
  sum_data <- Return_data
  for(i in 2:data_dim[1]){
    sum_data[i,] <- apply(Return_data[1:i,],2,sum)
  }
  
  A <- matrix(0,data_dim[1],data_dim[1])
  for(i in 1:data_dim[1]){
    A[i,i] <- -1
    A[i,i-1] <- 1
  }
  B <- colMeans(score_data)
   
Opti_obj <- cbind(0,t(as.matrix(B,1,data_dim[2])), matrix(0,1,data_dim[1]),
            matrix(0,1,data_dim[1]))
c1 <- cbind(1, matrix(0,1,data_dim[2]),
          1/((1-alpha)*data_dim[1])*matrix(1,1,data_dim[1]),matrix(0,1,data_dim[1]));
c2 <- cbind(-matrix(1,data_dim[1],1),-as.matrix(sum_data),-diag(rep(1,data_dim[1])),
          diag(rep(1,data_dim[1])))
c3 <- cbind(matrix(0,data_dim[1],1),as.matrix(sum_data),matrix(0,data_dim[1],data_dim[1]),
          -diag(rep(1,data_dim[1])))
c4 <- cbind(matrix(0,data_dim[1],1),matrix(0,data_dim[1],data_dim[2]),
          matrix(0,data_dim[1],data_dim[1]),A)
c5 <- cbind(0,matrix(1,1,data_dim[2]),matrix(0,1,data_dim[1]),matrix(0,1,data_dim[1]))
            
mat <- rbind(c1,c2,c3,c4,c5)
                    
dir <- matrix("<=",3*data_dim[1]+2,1)
                    
rhs <- cbind(omega,matrix(0,1,3*data_dim[1]),1)
bounds <- list(lower = list(ind = c(1L),val = c(-Inf)),
               upper = list(ind = c(2:(data_dim[2]+1)),val = rep(1,data_dim[2])))
                    
Retur <- Rglpk::Rglpk_solve_LP(Opti_obj,mat,dir,rhs,bounds,max=TRUE)
                    
weights <- Retur$solution[2:(data_dim[2]+1)]

names(weights) <- colnames(Return_data)

data.table(INNERCODE = (names(weights)[weights > 0]),
           WEIGHT = round(weights[weights > 0.0], 6),
           key = "INNERCODE")
}


model <- readr::read_rds("E:/RWD/tan/model.rds")
colnames(model) <- c("InnerCode","EndDate", "SecuCode", "SecuAbbr", "SecuMarket", "ListedSector",
                     "MV","MV_Float", "ClosePrice", "Ind_Name1", "Ind_Name2", "SW_Ind_Name1", 
                     "SW_Ind_Name2","MV_Float_2","ModelScore", "ModelRank")

data <- dplyr::select(model, -c(SecuCode,SecuMarket,ListedSector,MV,Ind_Name1,
                                Ind_Name2,SW_Ind_Name1,SW_Ind_Name2,MV_Float,
                                ClosePrice,MV_Float_2,ModelRank,SecuAbbr))

score <- reshape2::dcast(data, EndDate~InnerCode)
rownames(score)=score[,1]
score <- score[,-1]

a<- colMeans(score,na.rm = TRUE)
score[is.na(score)]<- a[ceiling(which(is.na(score))/nrow(score))]


rtn_mat <- readr::read_rds("E:/RWD/tan/rtn_mat.rds")
name_score <- variable.names(score)
name_rtn_mat <- variable.names(rtn_mat)
name <- Reduce(intersect,list(v1 = name_score,v2 = name_rtn_mat))

score <- score[,name]
rtn_mat <- rtn_mat[,name]

Portfopti_score(rtn_mat,score,0.8,0.2)









