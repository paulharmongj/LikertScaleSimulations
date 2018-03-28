#simulate likert-type data
library(tidyr); library(magrittr)
#simulation study
dat <- matrix(0,nrow = 100, ncol = 5)

#package depends on spatsta.utils
library(spatstat.utils)

probs = c(1,1,1,1,1)/5

getLikertDat <- function(probs = c(1,1,1,1,1)/5,num = 100){
  likert_vec <- rep(0,num)
  
  for(j in 1:num){
    # generate a random number between 1 and 100
    r <- sample(1:100,1)
    
    # now we check where the random number fits in the 
    ##define test expressions
    test_expression1 <- r/100 < probs[1]
    test_expression2 <- inside.range(r/100, c(probs[1], sum(probs[c(1,2)])))
    test_expression3 <- inside.range(r/100, c(sum(probs[1:2]), sum(probs[c(1:3)])))
    test_expression4 <- inside.range(r/100, c(sum(probs[1:3]), sum(probs[c(1:4)])))
    
    #iterate through assignment depending on the value of r
    if ( test_expression1) {
      x<- 1
    } else if ( test_expression2) {
      x <- 2
    } else if ( test_expression3) {
      x <- 3
    } else if ( test_expression4) {
      x <- 4
    } else {
      x <- 5
    }
    
    #assign the value of x to the likert_scale
    likert_vec[j] <- x
    
    #and repeat until 'num' times
  }
  return(likert_vec)}

#here we test with nearly even odds
table(getLikertDat(num = 1000))

#test with slightly tilted probabilities (towards extremes)
table(getLikertDat(probs = c(.3,.2,0,.2,.3),num = 100))


##Okay. Now we can generate five questions
fdat <- matrix(0,nrow = 100, ncol = 5)
means <- c(.05,.15,.35,.2,.25)
for(j in 1:5){
  fdat[,j] <-getLikertDat(means,num = 100) 
}

fdat <- as.data.frame(fdat)
colnames(fdat) <- paste("Q",c(1:5), sep = "")
fdat$SUM <- apply(fdat,1,sum)


##OK, so we can generate pre-and post-test likert data, along with some other vars (randomly generated):

#SPEAKER GROUP
pre <- c(.05,.15,.35,.2,.25)
post <- c(0,.1,.4,.3,.2)

predat <- matrix(0,nrow = 100, ncol = 5)
for(j in 1:5){
  predat[,j] <-getLikertDat(means,num = 100) 
}

postdat <- matrix(0,nrow = 100, ncol = 5)
for(j in 1:5){
  postdat[,j] <-getLikertDat(means,num = 100) 
}


#PAPER GROUP
##same pre-scores
post2 <- c(.1,.2,.3,.3,.1)

predat2 <- matrix(0,nrow = 100, ncol = 5)
for(j in 1:5){
  predat2[,j] <-getLikertDat(means,num = 100) 
}

postdat2 <- matrix(0,nrow = 100, ncol = 5)
for(j in 1:5){
  postdat2[,j] <-getLikertDat(means,num = 100) 
}

#now we can create a data frame
pres <- cbind(predat,postdat)
paper <- cbind(predat2,postdat2)

df <- data.frame(rbind(pres,paper))
colnames(df) <- c(paste("preQ",c(1:5), sep = ""),paste("postQ",c(1:5), sep = ""))
df$preSUM <- apply(df[,1:5],1,sum)
df$postSUM <- apply(df[,6:10],1,sum)
df$DIFF <- df$preSUM - df$postSUM

#add some quantitative variables
df$MEALS <- sample(1:21, nrow(df), replace = TRUE)
df$TRT <- c(rep(1,100),rep(0,100))
df$GENDER <- sample(1:2,nrow(df), replace = TRUE)


##linear model
mod1 <- lm(DIFF ~ TRT + MEALS + GENDER, data = df)
summary(mod1)

plot(mod1)



