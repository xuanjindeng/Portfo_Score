install.packages("FRAPO")
library(FRAPO)
data(StockIndex)
popt <- PCDaR(PriceData = StockIndex, alpha = 0.95,
              bound = 0.1, softBudget = TRUE)

library(GCAMCQT)
read_gildata(c("qt_dailyquote", "qt_adjustingfactor", "secumain"), from_to = c(20100101, 20170531))
cum_rtn_matrix <- function(from_to) {
  
  from_to <- as_from_to(from_to)
  
  res <- gildata$qt_dailyquote[TRADINGDAY <= from_to[2L] & TRADINGDAY >= from_to[1L],
                               .(INNERCODE, TRADINGDAY, CLOSEPRICE, TURNOVERVALUE)]
  res[, ADJ_CLOSE := {
    gildata$qt_adjustingfactor[res, GCAMCPUB::na_fill(RATIOADJUSTINGFACTOR, 1.0), roll = TRUE] * CLOSEPRICE
  }]
  
  univ <- res[TRADINGDAY == max(TRADINGDAY) & TURNOVERVALUE > 1e7, unique(INNERCODE)]
  
  all_i <- CJ(INNERCODE = univ, TRADINGDAY = sort(unique(res$TRADINGDAY)))
  res <- res[all_i, roll = TRUE]
  res[, SYMBOL := gildata$secumain[J(res$INNERCODE), SECUCODE]]
  res[, HAS_NA := anyNA(ADJ_CLOSE), by = INNERCODE]
  res <- res[HAS_NA == FALSE]
  
  setkey(res, INNERCODE, TRADINGDAY)
  res[, CUMR := ADJ_CLOSE / ADJ_CLOSE[1L] - 1L, keyby = INNERCODE]
  
  res <- data.table::dcast.data.table(res, TRADINGDAY ~ INNERCODE, value.var = "CUMR", fill = 0.0)
  
  dates <- res$TRADINGDAY
  res[, TRADINGDAY := NULL]
  setDF(res)
  rownames(res) = dates
  res
}


rtn_mat <- cum_rtn_matrix(from_to = c(201501, 201705))
readr::write_rds(rtn_mat, "D:/rtn_mat.rds")

rtn_mat <- readr::read_rds("E:/RWD/tan/rtn_mat.rds")
model <- readr::read_rds("E:/RWD/tan/model.rds")


optimize_cdar <- function(cum_rtn_matrix, alpha = 0.95, bound = 0.05, softBudget = TRUE,  ...) {
  
  if (is.null(dim(cum_rtn_matrix))) {
    stop("Argument for 'cum_rtn_matrix' must be rectangular.\n")
  }
  if (any(is.na(cum_rtn_matrix))) {
    stop("NA-values contained in object for 'cum_rtn_matrix'.\n")
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("Argument for 'alpha' must be in the interval (0, 1).\n")
  }
  if (bound <= 0 || bound >= 1) {
    stop("Argument for 'bound' must be in the interval (0, 1).\n")
  }
  call <- match.call()
  # cum_rtn_matrix <- as.matrix(FRAPO::returnseries(PriceData, method = "discrete", percentage = FALSE, compound = TRUE))
  rownames(cum_rtn_matrix) <- NULL
  N <- ncol(cum_rtn_matrix)
  J <- nrow(cum_rtn_matrix)
  w <- rep(0, N)
  u <- rep(0, J)
  v <- rep(0, J)
  z <- 0
  x <- c(w, u, v, z)
  obj <- c(as.numeric(cum_rtn_matrix[J, ]), rep(0, J), rep(0, J), 0)
  a1 <- cbind(diag(N), matrix(0, nrow = N, ncol = 2 * J + 1))
  d1 <- rep(">=", N)
  b1 <- rep(0, N)
  a2 <- c(rep(1, N), rep(0, 2 * J + 1))
  ifelse(softBudget, d2 <- "<=", d2 <- "==")
  b2 <- 1
  a3 <- cbind(cum_rtn_matrix, -1 * diag(J), diag(J), matrix(1, ncol = 1, nrow = J))
  d3 <- rep(">=", J)
  b3 <- rep(0, J)
  a4 <- cbind(matrix(0, ncol = N, nrow = J), matrix(0, ncol = J, nrow = J), diag(J), matrix(0, ncol = 1, nrow = J))
  d4 <- rep(">=", J)
  b4 <- rep(0, J)
  a5 <- c(rep(0, N), rep(0, J), (1/J) * (1/(1 - alpha)) * rep(1, J), 1)
  d5 <- "<="
  b5 <- bound
  a6 <- cbind(-1 * cum_rtn_matrix, diag(J), matrix(0, nrow = J, ncol = J + 1))
  d6 <- rep(">=", J)
  b6 <- rep(0, J)
  D1 <- -1 * diag(J)
  udiag <- embed(1:J, 2)[, c(2, 1)]
  D1[udiag] <- 1
  a7 <- cbind(matrix(0, ncol = N, nrow = J), D1, matrix(0, nrow = J, ncol = J + 1))
  a7 <- a7[-J, ]
  d7 <- rep(">=", J - 1)
  b7 <- rep(0, J - 1)
  a8 <- c(rep(0, N), 1, rep(0, J - 1), rep(0, J), 0)
  d8 <- "=="
  b8 <- 0
  Amat <- rbind(a1, a2, a3, a4, a5, a6, a7, a8)
  Dvec <- c(d1, d2, d3, d4, d5, d6, d7, d8)
  Bvec <- c(b1, b2, b3, b4, b5, b6, b7, b8)
  opt <- Rglpk::Rglpk_solve_LP(obj = obj, mat = Amat, dir = Dvec, rhs = Bvec, max = TRUE, ...)
  if (opt$status != 0) {
    warning(paste("GLPK had exit status:", opt$status))
  }
  weights <- opt$solution[1:N]
  names(weights) <- colnames(cum_rtn_matrix)
  
  data.table(INNERCODE = (names(weights)[weights > 0]),
             WEIGHT = round(weights[weights > 0.0], 6),
             key = "INNERCODE")
}


sample_data <- readr::read_csv("20170614-cdar-proto/data.csv")
sample_data <- sample_data[, 2:6]
setDT(sample_data)
for (i in seq_len(ncol(sample_data))) {
  
  sample_data[[i]] <- cumsum(sample_data[[i]]) 
}
optimize_cdar(as.matrix(sample_data), softBudget = F, bound = 0.2531, alpha = 0.8)$weight


tmp <- optimize_cdar(as.matrix(price_data), softBudget = TRUE)
