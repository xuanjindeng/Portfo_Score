# install.packages("Rglpk")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("data.table")

library(Rglpk)
library(dplyr)
library(tidyr)
library(data.table)

portfopti_score <- function(Return_data, score_data, alpha, omega) {
  ## Inputs: Returndata---The logarithm return data
  ## score_data---The score of stocks
  ## alpha----The confident Level
  ## omega----The up bound of CDaR
  ## Output:InnerCode---The Code of assets to invest
  ## Weight---The Optimization weight of assets
 
  data_dim <- dim(Return_data)
  sum_data <- Return_data
  for (i in 2:data_dim[1]) {
    sum_data[i, ] <- apply(Return_data[1:i, ], 2, sum)
  }
  
  A <- matrix(0, data_dim[1], data_dim[1])
  for (i in 1:data_dim[1]) {
    A[i, i] <- -1
    A[i, i - 1] <- 1
  }
  B <- colMeans(score_data)
  
  Opti_obj <-
    cbind(0,
          t(as.matrix(B, 1, data_dim[2])),
          matrix(0, 1, data_dim[1]),
          matrix(0, 1, data_dim[1]))
  c1 <- cbind(
    1,
    matrix(0, 1, data_dim[2]),
    1 / ((1 - alpha) * data_dim[1]) * matrix(1, 1, data_dim[1]),
    matrix(0, 1, data_dim[1])
  )
  
  c2 <-
    cbind(-matrix(1, data_dim[1], 1),
          -as.matrix(sum_data),
          -diag(rep(1, data_dim[1])),
          diag(rep(1, data_dim[1])))
  c3 <-
    cbind(
      matrix(0, data_dim[1], 1),
      as.matrix(sum_data),
      matrix(0, data_dim[1], data_dim[1]),-diag(rep(1, data_dim[1]))
    )
  c4 <-
    cbind(
      matrix(0, data_dim[1], 1),
      matrix(0, data_dim[1], data_dim[2]),
      matrix(0, data_dim[1], data_dim[1]),
      A
    )
  c5 <-
    cbind(0,
          matrix(1, 1, data_dim[2]),
          matrix(0, 1, data_dim[1]),
          matrix(0, 1, data_dim[1]))
  
  mat <- rbind(c1, c2, c3, c4, c5)
  
  dir <- matrix("<=", 3 * data_dim[1] + 2, 1)
  
  rhs <- cbind(omega, matrix(0, 1, 3 * data_dim[1]), 1)
  bounds <- list(lower = list(ind = c(1L), val = c(-Inf)),
                 upper = list(ind = c(2:(data_dim[2] + 1)), val = rep(0.05, data_dim[2])))
  
  Retur <- Rglpk::Rglpk_solve_LP(Opti_obj, mat, dir, rhs, bounds, max = TRUE)
  
  weights <- Retur$solution[2:(data_dim[2] + 1)]
  
  names(weights) <- colnames(Return_data)
  
  data.table(
    INNERCODE = (names(weights)[weights > 0]),
    WEIGHT = round(weights[weights > 0.0], 6),
    key = "INNERCODE"
  )
}


data_proc <- function(rtn, score_model) {
  data <- score_model[, c("InnerCode", "EndDate", "ModelScore")]
  score <- data.table::dcast.data.table(data, EndDate ~ InnerCode)
  names <- as.character.Date(score[, 1])
  score <- score[,-1]
  row.names(score) <- names
  
  a <- colMeans(score, na.rm = TRUE)
  score[is.na(score)] <-
    a[ceiling(which(is.na(score)) / nrow(score))]
  
  name_score <- variable.names(score)
  name_rtn <- variable.names(rtn)
  name <- Reduce(intersect, list(v1 = name_score, v2 = name_rtn))
  
  score <- as.data.frame(score)
  score <- score[, name]
  row.names(score) <- names
  rtn <- rtn[, name]
  output <- list(Return_data = rtn, score_data = score)
  return(output)
}


model <- readr::read_rds("E:/RWD/Deng/score_portfolio/model.rds")
rtn_mat <-
  readr::read_rds("E:/RWD/Deng/score_portfolio/rtn_mat.rds")
Return_data <- data_proc(rtn_mat,model)$Return_data
score_data <- data_proc(rtn_mat,model)$score_data
portfopti_score(Return_data[1:100,1:10], score_data, 0.8, 0.2)






model <- readr::read_rds("E:/RWD/Deng/score_portfolio/model.rds")
rtn_mat <-
  readr::read_rds("E:/RWD/Deng/score_portfolio/rtn_mat.rds")
data <- model[, c("InnerCode", "EndDate", "ModelScore")]
# colnames(model) <- c("InnerCode","EndDate", "SecuCode", "SecuAbbr", "SecuMarket", "ListedSector",
#                      "MV","MV_Float", "ClosePrice", "Ind_Name1", "Ind_Name2", "SW_Ind_Name1",
#                      "SW_Ind_Name2","MV_Float_2","ModelScore", "ModelRank")
#
# data <- dplyr::select(model, -c(SecuCode,SecuMarket,ListedSector,MV,Ind_Name1,
#                                 Ind_Name2,SW_Ind_Name1,SW_Ind_Name2,MV_Float,
#                                 ClosePrice,MV_Float_2,ModelRank,SecuAbbr))

# score <- reshape2::dcast(data, EndDate~InnerCode)
score <- data.table::dcast.data.table(data, EndDate ~ InnerCode)
names <- as.character.Date(score[, 1])
score <- score[, -1]
row.names(score) <- names

a <- colMeans(score, na.rm = TRUE)
score[is.na(score)] <- a[ceiling(which(is.na(score)) / nrow(score))]

name_score <- variable.names(score)
name_rtn_mat <- variable.names(rtn_mat)
name <- Reduce(intersect, list(v1 = name_score, v2 = name_rtn_mat))

score <- as.data.frame(score)
score <- score[, name]
row.names(score) <- names
rtn_mat <- rtn_mat[, name]

Portfopti_score(rtn_mat[1:100,], score, 0.95, 0.2)



# http://www.cnblogs.com/nxld/p/6067137.html
