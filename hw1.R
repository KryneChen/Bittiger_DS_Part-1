setwd("~/Desktop/DS501/BitTiger-DS501-1805/lending-club-loan-data")
loan <- read.csv('loan.csv', header = T, stringsAsFactors = F)
str(loan)
summary(loan)
dim(loan)
loan_colnames <- colnames(loan)
plot(density(loan$loan_amnt))
plot(density(loan$int_rate))
length(which(is.na(loan)))

library(mice)
md.pattern(loan)

num.NA <- sort(sapply(loan, function(x) {sum(is.na(x))}), 
              decreasing=TRUE)
remain.col <- names(num.NA)[which(num.NA <= 0.8 * dim(loan)[1])]
loan <- loan[, remain.col]

sum(is.na(loan)) / (nrow(loan)*ncol(loan))
pMiss <- function(x){
  sum(is.na(x))/length(x)*100
}

missing <- apply(loan, 2, pMiss)
table(missing)

names(missing[which(missing > 50)])
str(loan)

library(corrplot)
library(dplyr)
loan.chr <- select_if(loan, is.character)
chr_or_num <- sapply(loan, is.numeric)
loan.num <- loan[, chr_or_num]
colnames(loan.num)
loan.num <- loan.num[,colnames(loan.num) != 
                       c('id','member_id')]
loan.corr <- cor(loan.num, use = "pairwise.complete.obs")
loan.corr <- loan.corr[1:31, 1:31]
corrplot(loan.corr, method="circle",type = 'upper')

int_rate_corr <- loan.corr[,colnames(loan.corr) == 'int_rate']
which(int_rate_corr == 1)
int_rate_corr <- int_rate_corr[-18]
int_rate_corr <- sort(int_rate_corr, decreasing = T)
int_rate_corr <- as.data.frame(int_rate_corr)
barplot(int_rate_corr$int_rate_corr, names.arg = rownames(int_rate_corr))


library(ggplot2)
colnames(loan.chr)
bwplot(int_rate~term, data = loan)
bwplot(int_rate~emp_length, data = loan)
bwplot(int_rate~home_ownership, data = loan)
bwplot(int_rate~verification_status, data = loan)
bwplot(int_rate~pymnt_plan, data = loan)
bwplot(int_rate~purpose, data = loan)
bwplot(int_rate~addr_state, data = loan)
bwplot(int_rate~initial_list_status, data = loan)



library(zoo)
as.Date(as.yearmon(loan$issue_d[1:5], "%b-%Y"))
loan$issue_d_1 = as.Date(as.yearmon(loan$issue_d, "%b-%Y"))
loan$issue_year = format(loan$issue_d_1, '%Y')
loan$issue_mon = format(loan$issue_d_1, '%m')

int.rate.by.time = by(loan, loan$issue_d_1, function(x) {
  return(median(x$int_rate))
})
plot(as.Date(names(int.rate.by.time)), int.rate.by.time, 
     type = 'l', xlab = 'Date')

int.rate.by.year = by(loan, loan$issue_year, function(x) {
  return(median(x$int_rate))
})
plot(names(int.rate.by.year), int.rate.by.year, 
     type = 'l', xlab = 'Year')

int.rate.by.month = by(loan, loan$issue_mon, function(x){
  return(median(x$int_rate))
})
plot(names(int.rate.by.month), int.rate.by.month,
     type = 'l', xlab = 'Month')

bwplot(int_rate ~ issue_year, data = loan)
