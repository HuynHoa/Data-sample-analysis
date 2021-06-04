## Two-stage cluster sampling

data <- read.csv('books.csv',header=TRUE,sep=",")

## a. Draw the boxplot for the replacement costs of books on each shelf
boxplot(data$replace~data$shelf,
        main = "The replacement cost of books on each shelf",
        xlab = "Shelf",ylab = "Replacement cost")

data <- read.csv('books.csv',header=TRUE,sep=",")
head(data)

  
## b. Estimate the total replacement cost for the library 
N <- 44
n <- length(data$shelf)/5
n
mi <- 5

df <- data.frame(data)

shelves <- c("2","4","11","14","20","22","23","31","37","38","40","43")
mshelf <- vector(mode="numeric",length = 0) #average cost per shelf yi
bshelf <- vector(mode="numeric",length = 0) #books number per shelf
cshelf <- vector(mode="numeric",length = 0) #cost per shelf ti

for (i in 1:length(shelves)) {
       shelf <- shelves[i]
       mshelf[shelf] <- mean(data$replace[which(data$shelf==shelf)])
       bshelf[shelf] <- df$Mi[which(df$shelf==shelf)]
       cshelf[shelf] <- mshelf[shelf]*bshelf[shelf]
       }

sumdf <- data.frame(
       "Mean" = mshelf, 
       "TotalCost" = cshelf
       )
sumdf

# Total replacement cost tunb = (N/n)*sum(ti)
tunb <- (N/n)*sum(cshelf)
tunb

# Sample variance of secondary sampling units in primary sampling unit i
si <- c()
for (i in 1:length(shelves)) {
     shelf <- shelves[i]
     m_shelf <- mshelf[shelf]
     
       si_books <- c()  
       for (j in df$replace[which(df$shelf==shelf)]) {
           si_book <- j - m_shelf
           si_books <- c(si_books,si_book^2)
           }
       
         si_shelf <- sum(si_books)/(mi-1)
         si <- c(si,si_shelf)
      }
 
st <- var(cshelf)
st

Vtunb <- N^2*((1-n/N)*(st/n))+(N/n)*sum(1-(mi/Mi))*(Mi^2)*((si^2)/mi)
Vtunb

# Standard error
SEtunb <- sqrt(Vtunb)
SEtunb

# Coefficient of variation
CVt <- SEtunb/tunb
CVt


## c. Estimate the average replacement cost per book
# yr = sum(ti)/sum(Mi)
yr <- sum(cshelf)/sum(Mi)
yr

sr2 <- sum((cshelf - (Mi*yr))^2)/(n-1)
sr2

Vyr <- (1-(n/N))*(sr2/(n*mean(Mi)^2))+ (1/(n*N*(mean(Mi)^2))*(sum(1-(mi/Mi))*(Mi^2)*((si^2)/mi)))
Vyr

# Standard error
SEyr <- sqrt(Vyr)
SEyr

# coefficient of variation
CVy <- SEyr/yr
CVy