library(Rglpk)
library(dplyr)
library(tidyr)
library(data.table)

## Given a decision date, the function of data_proc() are to return the latest
## scores, and the 100-periods of returns before the decision date.
data_proc <- function(deci_date, rtn, score_model) {
  data <- score_model[, c("InnerCode", "EndDate", "ModelScore")]
  score <- data.table::dcast.data.table(data, EndDate ~ InnerCode)
  score <- as.data.frame(score)
  names <- as.character.Date(score[, 1])
  score <- score[,-1]
  row.names(score) <- names
  
  score <-
    subset(score, row.names(score) <= as.character.Date(deci_date))
  score <- score[nrow(score),]
  score <- t(na.omit(t(score)))
  
  rtn <- subset(rtn, row.names(rtn) <= as.character.Date(deci_date))
  rtn <- rtn[max(1, nrow(rtn) - 50):nrow(rtn),]
  
  name_score <- variable.names(score)
  name_rtn <- variable.names(rtn)
  name <- Reduce(intersect, list(v1 = name_score, v2 = name_rtn))
  
  score <- as.data.frame(score)
  score <- score[, name]
  rtn <- rtn[, name]
  output <- list(Return_data = rtn, score_data = score)
  return(output)
}

industry_group <- function(score_model) {
  code_industry <- score_model[, c("InnerCode", "Ind_Name1")]
  code_industry <-
    code_industry[!duplicated(code_industry$InnerCode)]
  code_industry[is.na(code_industry)] <- "其他"
  
  industry <- code_industry[, "Ind_Name1"]
  industry <- unique(industry)
  
  group <- NULL
  for (i in 1:nrow(industry)) {
    group[i] <-
      filter(code_industry,
             code_industry$Ind_Name1 == as.character(industry[i]))[1]
  }
  return(group)
}

## The function of portfopti_score() is to find the optimal weights on each assets
## with the objection of Maximizing the score under the CDaR constraint.

portfopti_score <-
  function(deci_date, rtn, score_model, alpha, omega) {
    ## Inputs: deci_date---The decision date
    ## rtn---The accumulative logarithm return data
    ## score_data---The score of stocks
    ## alpha----The confident Level
    ## omega----The up bound of CDaR
    ## Output:InnerCode---The Code of assets to invest
    ## Weight---The Optimization weight of assets
    if (alpha <= 0 || alpha >= 1) {
      stop("Argument for 'alpha' must be in the interval (0, 1).\n")
    }
    if (omega <= 0 || omega >= 1) {
      stop("Argument for 'omega' must be in the interval (0, 1).\n")
    }
    call <- match.call()
    
    Return_data <- data_proc(deci_date, rtn, score_model)$Return_data
    score_data <- data_proc(deci_date, rtn, score_model)$score_data
    
    code_industry <- score_model[, c("InnerCode", "Ind_Name1")]
    code_industry <-
      code_industry[!duplicated(code_industry$InnerCode)]
    
    data_dim <- dim(Return_data)
    sum_data <- Return_data
    # for (i in 2:data_dim[1]) {
    #   sum_data[i,] <- apply(Return_data[1:i,], 2, sum)
    # }
    
    A <- matrix(0, data_dim[1], data_dim[1])
    for (i in 1:data_dim[1]) {
      A[i, i] <- -1
      A[i, i - 1] <- 1
    }
    B <- score_data
    
    Opti_obj <-
      cbind(0,
            as.matrix(B, 1, data_dim[2]),
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
    group <- industry_group(score_model)
    name_score_data <- names(score_data)
    
    H <- matrix(0, length(group), 1 + data_dim[2] + 2 * data_dim[1])
    for (i in 1:length(group)) {
      for (j in 1:ncol(score_data)) {
        if (as.numeric(name_score_data[j]) %in% group[[i]]) {
          H[i, j + 1] <- 1
        }
      }
    }
    
    mat <- rbind(c1, c2, c3, c4, c5, H)
    
    dir <- matrix("<=", 3 * data_dim[1] + 2 + length(group), 1)
    
    rhs <-
      cbind(omega, matrix(0, 1, 3 * data_dim[1]), 1, matrix(0.2, 1, length(group)))
    bounds <- list(lower = list(ind = c(1L), val = c(-Inf)),
                   upper = list(ind = c(2:(data_dim[2] + 1)), val = rep(0.06, data_dim[2])))
    
    Retur <-
      Rglpk::Rglpk_solve_LP(Opti_obj, mat, dir, rhs, bounds, max = TRUE)
    weights <- Retur$solution[2:(data_dim[2] + 1)]
    
    names(weights) <- colnames(Return_data)
    code_industry <- score_model[, c("InnerCode", "Ind_Name1")]
    code_industry <-
      code_industry[!duplicated(code_industry$InnerCode)]
    code_industry[is.na(code_industry)] <- "其他"
    INDUSTRY = code_industry[InnerCode %in% names(weights[weights > 0]), Ind_Name1]
    
    data.table(
      INNERCODE = names(weights[weights > 0]),
      SCORE_DATE = row.names(score_data),
      INDUSTRY,
      WEIGHT = round(weights[weights > 0.0], 6),
      key = "INNERCODE"
    )
  }



model <- readr::read_rds("E:/RWD/Deng/score_portfolio/model.rds")
rtn_mat <-
  readr::read_rds("E:/RWD/Deng/score_portfolio/rtn_mat.rds")
# deci_date <- "2016-01-22"
deci_date <- "2016-12-20"

portfopti_score(deci_date, rtn_mat, model, 0.8, 0.2)
