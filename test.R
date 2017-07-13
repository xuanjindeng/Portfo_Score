

library(Rglpk)
library(dplyr)
library(tidyr)
library(data.table)
data_proc <- function(deci_date, rtn, score_model) {
  data <- score_model[, c("InnerCode", "EndDate", "ModelScore")]
  score <- data.table::dcast.data.table(data, EndDate ~ InnerCode)
  score <- as.data.frame(score)
  names <- as.character.Date(score[, 1])
  score <- score[, -1]
  row.names(score) <- names
  
  score <-
    subset(score, row.names(score) <= as.character.Date(deci_date))
  score <- score[nrow(score), ]
  score <- t(na.omit(t(score)))
  
  rtn <- subset(rtn, row.names(rtn) <= as.character.Date(deci_date))
  rtn <- rtn[max(1, nrow(rtn) - 200):nrow(rtn), ]
  
  name_score <- variable.names(score)
  name_rtn <- variable.names(rtn)
  name <- Reduce(intersect, list(v1 = name_score, v2 = name_rtn))
  
  score <- as.data.frame(score)
  score <- score[, name]
  row.names(score) <- deci_date
  rtn <- rtn[, name]
  output <- list(Return_data = rtn, score_data = score)
  return(output)
}


model <- readr::read_rds("E:/RWD/Deng/score_portfolio/model.rds")
rtn_mat <-
  readr::read_rds("E:/RWD/Deng/score_portfolio/rtn_mat.rds")
code_industry <- model[, c("InnerCode", "Ind_Name1")]
code_industry <- code_industry[!duplicated(code_industry$InnerCode)]
code_industry[is.na(code_industry)] <- "ÆäËû"

industry <- code_industry[,"Ind_Name1"]
industry <- unique(industry)

group <-NULL
for (i in 1:nrow(industry)){
  group[i] <- filter(code_industry,code_industry$Ind_Name1==as.character(industry[i]))[1] 
}

deci_date <- "2016-01-22"
score_data <- data_proc(deci_date, rtn_mat, model)$score_data
Return_data <- data_proc(deci_date, rtn_mat, model)$Return_data
data_dim <- dim(Return_data)

name_score_data <- names(score_data)
H <- matrix(0,length(group),1+data_dim[2]+2*data_dim[1])

for (i in 1:length(group)){
  for (j in 1:ncol(score_data)){
    if (as.numeric(name_score_data[j]) %in% head(group[[i]])){
      H[i,j+1] <- 1
    }
  }
}

## To check the previous code
# test_return <- read_csv("E:/RWD/Deng/score_portfolio/test_return.csv")
# test_score <- read_csv("E:/RWD/Deng/score_portfolio/test_score.csv")
#
# test_score <- as.data.frame(test_score)
# names <- as.character.Date(test_score[, 1])
# test_score <- test_score[,-1]
# row.names(test_score) <- names
# View(test_score)
#
# test_return <- as.data.frame(test_return)
# names1 <- as.character.Date(test_return[, 1])
# test_return <- test_return[,-1]
# row.names(test_return) <- names1
# View(test_return)
#
# deci_date <- "2015-11-20"
# alpha <- 0.8
# omega <- 0.2
# Return_data <- test_return
# score_data <- test_score
#
# data_dim <- dim(Return_data)
# sum_data <- Return_data
# for (i in 2:data_dim[1]) {
#   sum_data[i, ] <- apply(Return_data[1:i, ], 2, sum)
# }
#
# A <- matrix(0, data_dim[1], data_dim[1])
# for (i in 1:data_dim[1]) {
#   A[i, i] <- -1
#   A[i, i - 1] <- 1
# }
# B <- score_data
#
# Opti_obj <-
#   cbind(0,
#         as.matrix(B, 1, data_dim[2]),
#         matrix(0, 1, data_dim[1]),
#         matrix(0, 1, data_dim[1]))
# c1 <- cbind(
#   1,
#   matrix(0, 1, data_dim[2]),
#   1 / ((1 - alpha) * data_dim[1]) * matrix(1, 1, data_dim[1]),
#   matrix(0, 1, data_dim[1])
# )
#
# c2 <-
#   cbind(-matrix(1, data_dim[1], 1),
#         -as.matrix(sum_data),
#         -diag(rep(1, data_dim[1])),
#         diag(rep(1, data_dim[1])))
# c3 <-
#   cbind(
#     matrix(0, data_dim[1], 1),
#     as.matrix(sum_data),
#     matrix(0, data_dim[1], data_dim[1]),-diag(rep(1, data_dim[1]))
#   )
# c4 <-
#   cbind(
#     matrix(0, data_dim[1], 1),
#     matrix(0, data_dim[1], data_dim[2]),
#     matrix(0, data_dim[1], data_dim[1]),
#     A
#   )
# c5 <-
#   cbind(0,
#         matrix(1, 1, data_dim[2]),
#         matrix(0, 1, data_dim[1]),
#         matrix(0, 1, data_dim[1]))
#
# mat <- rbind(c1, c2, c3, c4, c5)
#
# dir <- matrix("<=", 3 * data_dim[1] + 2, 1)
#
# rhs <- cbind(omega, matrix(0, 1, 3 * data_dim[1]), 1)
# bounds <- list(lower = list(ind = c(1L), val = c(-Inf)),
#                upper = list(ind = c(2:(data_dim[2] + 1)), val = rep(1, data_dim[2])))
#
# Retur <- Rglpk::Rglpk_solve_LP(Opti_obj, mat, dir, rhs, bounds, max = TRUE)
#
# weights <- Retur$solution[2:(data_dim[2] + 1)]
#
# names(weights) <- colnames(Return_data)
#
# data.table(
#   INNERCODE = (names(weights)[weights > 0]),
#   DATE = deci_date,
#   WEIGHT = round(weights[weights > 0.0], 6),
#   key = "INNERCODE"
# )

