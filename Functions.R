# Author : Bohdan Monastyrskyy
# Date : 2015-10-23
# Description : functions related to mortgage calcualations

# load libraries
library("ggplot2")
# load user-defined utils
source("Utils.R")

# Function calculates the interest per month.
# args:
#      loan - amount of loan
#      APR - annual interest rate
interestPerMonth <- function(loan = 250000.00, APR = 4.0){
  res <- ifelse(loan > 0.0, round(loan*(APR/1200), 2), 0.0);
  res;
}

# Function morgagePaymentFixed calculates the monthly payments for "fixed rate scenario"
# args:
#      loan - money lended (default 250,000.00)
#      APR - annual percent rate (default 4.0%)
#      period - period in months (default 360 (30 years))
#      payment - payment per month (default = NULL)
# return :
#      payment, debt, interest: per month
mortgagePaymentFixed <- function(loan = 250000.00, APR = 4.0, period = 360, payment = NA){
  loan <- as.double(loan)
  APR <- as.double(APR)
  debt <- c(loan) # debt variable starts from the beginning of the first month
  interest <- c(0)
  minpayment <- round(loan*(APR/1200 * (1+APR/1200)^period)/((1+APR/1200)^period-1),2);
  if(is.null(payment) | is.na(payment)){
    payment <- round(loan*(APR/1200 * (1+APR/1200)^period)/((1+APR/1200)^period-1),2)
    payment <- c(0, rep(payment, period) )
  } else {
    if (is.numeric(payment))  {
      payment <- payment[1]; # take only first element
      # the payment can't be less than minpayment
      if (payment < minpayment){
        payment <- minpayment
      }
      # convert payment into a list
      payment <- c(0, rep(payment, period))
      #last <- payment[length(payment)]
      #payment <- c(0, rep(payment, period - length(payment) + 1))
    }
  }
  for(i in 1:period){
    interest <- c(interest, interestPerMonth(debt[length(debt)], APR))
    debt <- c(debt, round(max(debt[length(debt)] + interestPerMonth(debt[length(debt)], APR) - payment[i+1], 0), 2))
    if (i < period){
       payment[i+2] <- min(payment[i+1], interestPerMonth(debt[length(debt)], APR) + debt[length(debt)])
    }
  }
  df <-  data.frame(payment = payment, interest = interest, debt = debt)
  rownames(df) <- 0:period
  # add some attributes to result
  attr(df, "APR") <- APR
  attr(df, "breaks") <- NULL
  attr(df, "loan") <- loan
  attr(df, "payment") <- unique(df$payment[-1])
  attr(df, "period") <- period
  df
}

# Function morgagePaymentFixed calculates the monthly payments for "adjusted rate scenario"
# args:
#      loan - money lended (default 250,000.00)
#      APR - annual percent rate (default 4.0%) (array of rates)
#      breaks - months where the change of rate happened
#      period - period in months (default 360 (30 years))
#      payment - payment per month (default = NULL)
# return :
#      payment, debt, interest: per month
mortgagePaymentAdjusted <- function(loan = 250000.00, APR = 4.0, period = 360, breaks = NULL, payment = NULL){
  loan <- as.double(loan)
  APR <- as.double(APR)
  if(is.null(breaks)){
    if (is.null(payment)){
      return(mortgagePaymentFixed(loan = loan, APR = APR, period = period));
    } else{
      return(mortgagePaymentFixed(loan = loan, APR = APR, period = period, payment = payment));
    }
  }
  # TODO: check order of breaks
  if (length(breaks) + 1 > length(APR) ){
    lastAPR <- APR[length(APR)]
    APR <- c(APR, rep(lastAPR, length(breaks) - length(APR) + 1))
  }
  if (length(breaks) + 1 < length(APR)){
    APR <- APR[1:(length(breaks) + 1)]
  }
  if(!is.null(payment)){
    tmp <- mortgagePaymentFixed(loan = loan, APR = APR[1], period = period, payment = payment);
  } else {
    tmp <- mortgagePaymentFixed(loan = loan, APR = APR[1], period = period);
  }
  result <- tmp[1:(breaks[1] + 1),]

  debt <- tail(result$debt, n=1)[1]
  for(i in 1:length(breaks)){
    if(!is.null(payment)){
      tmp <- mortgagePaymentFixed(loan = debt, APR = APR[i + 1], period = period - breaks[i], payment = payment);
    } else {
      tmp <- mortgagePaymentFixed(loan = debt, APR = APR[i + 1], period = period - breaks[i]);
    }
    if (i == length(breaks)){
      result <- rbind(result, tmp[-1,]);
    } else {
      result <- rbind(result, tmp[2:(breaks[i + 1] - breaks[i] + 1),]);
    }
    debt <- tail(result$debt, n=1)
  }
  rownames(result) <- 0:period
  # add some attributes to result
  attr(result, "APR") <- APR
  attr(result, "breaks") <- breaks
  attr(result, "loan") <- loan
  attr(result, "payment") <- unique(result$payment[-1])
  attr(result, "period") <- period
  result
}

# Function prints out the summary of the loan
loan.summary<-function(df){
  cat("========================", fill = TRUE)
  cat("LOAN SUMMARY:", fill = TRUE)
  printf("Loan ammount: %s\n", format(attributes(df)$loan, big.mark = ',', nsmall = 2), fill=TRUE)
  cat("", fill = TRUE)
  printf("APR: %s\n", paste(format(attributes(df)$APR,  big.mark = ',', nsmall = 2), collapse = " "), fill=TRUE)
  cat("", fill = TRUE)
  cat("Monthly payment:", fill=TRUE)
  ind <- which (df$payment[c(-length(df$payment))] - df$payment[-1] != 0)
  ind <- unique(c(ind , attributes(df)$period))
  print(format(df[as.character(ind),c("payment", "debt")], big.mark = ',', nsmall = 2))
  cat("",fill = TRUE)
  printf("Total interest: %s", format(sum(df$interest), big.mark = ',', nsmall = 2), fill=TRUE)
  cat("", fill = TRUE)
  printf("Principle + Interest: %s\n", format(sum(df$interest) + df$debt[1] , big.mark = ',', nsmall = 2), fill=TRUE)
  cat("========================", fill = TRUE)
}

# The function plots the payments coloring the interest and principle parts.
plotPayment <- function(df){
  n <- nrow(df)
  #df$prnc <- df$payment - df$interest
  df$month <- 0:(n-1)
  p <- ggplot(df, aes(x=month))
  p <- p + geom_bar(aes(y=payment), stat = "identity", fill = "red", binwidth = 1, width = 1) +
    geom_bar(aes(y=interest), stat = "identity", fill = "green", binwidth = 1, width = 1)
  print(p)
}

# The function generats the amortization plot
plotAmortization <- function(df){
  n <- nrow(df)
  df$prnc <- cumsum(df$payment - df$interest)
  df$month <- 0:(n-1)
  p <- ggplot(df, aes(x=month))
  p <- p + geom_line(aes(y=debt), color="blue") + geom_line(aes(y=prnc), color = "red")
  print(p)
}
