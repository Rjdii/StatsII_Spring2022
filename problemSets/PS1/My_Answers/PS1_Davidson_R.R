
## Q1

###Notes###

stuff <- (rcauchy(1000, location = 0, scale = 1))
stuff
sort(stuff)

as.data.frame(stuff)

x_sort <- sort(stuff)
x_tb <- table(x_sort)
fr_df <- as.data.frame(x_tb)
fr_df$cf <- cumsum(fr_df$Freq)
fr_df$fsx <- with(fr_df,cf/sum(Freq))

mx <- mean(stuff)
sdx <- sd(stuff)

fr_df$Zscore <- (x_sort-mean(x_sort))/sd(x_sort)

fr_df$ftx <- pnorm(q=fr_df$Zscore)

fr_df$Dvals <- (fr_df$fsx - fr_df$ftx)

D <- max(fr_df$Dvals)

alpha = 0.05
qntl(Dcrit_MC, D, 1-alpha)
print(Dcrit_MC)

ecdf(x_sort)

####Defining The Function####

ks <- function(x) {
  
#Arranging the data
  x_sort <- sort(x)
  x_tb <- table(x_sort)
  fr_df <- as.data.frame(x_tb)
  fr_df$cf <- cumsum(fr_df$Freq)
  fr_df$fsx <- with(fr_df,cf/sum(Freq))

#Getting Z scores
  fr_df$Zscore <- (x_sort-mean(x_sort))/sd(x_sort)

#Getting P values
  fr_df$Pvalue <- pnorm(q=fr_df$Zscore)
  
#Finding D
  
  fr_df$Dvals <- (fr_df$fsx - fr_df$ftx)
  
  D <- max(fr_df$Dvals)
}

#I really couldn't understand the second equation in the homework, so this is 
#as far as I got with the function.

## Q2

set.seed(123)
data <- data.frame( x = runif (200 , 1 , 10) )
data$y <- 0 + 2.75*data$x + rnorm(200 , 0 , 1.5 )

##I'm a bit lost on this one, too. 


