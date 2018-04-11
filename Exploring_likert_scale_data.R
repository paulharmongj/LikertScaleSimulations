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

##OK let's try simulating some likert-scale data
#Consider a survey with 10 questions on a five point likert scale with two latent traits
set.seed(23)
latent_1 <- matrix(0,nrow = 100, ncol = 5)
means <- c(.05,.15,.35,.2,.25)
for(j in 1:5){
  latent_1[,j] <-getLikertDat(means,num = 100) 
}
set.seed(55)
latent_2  <- matrix(0,nrow = 100, ncol = 5)
means <- c(.15,.05,.55,.05,.2)
for(j in 1:5){
  latent_2[,j] <-getLikertDat(means,num = 100) 
}

sim_dat <- data.frame(cbind(latent_1,latent_2))


##How to approach with linear regression:
#try to model the responses linearly

m1 <- lm()



###Simulation Study: 
#Survey given before and after intervention is assigned


#SPEAKER GROUP
pre <- c(.8,.05,.05,.1,0)
post <- c(0,.16,.14,.3,.4)

predat <- matrix(0,nrow = 100, ncol = 5)
for(j in 1:5){
  predat[,j] <-getLikertDat(means,num = 100) 
}

postdat <- matrix(0,nrow = 100, ncol = 5)
for(j in 1:5){
  postdat[,j] <-getLikertDat(means,num = 100) 
}

final_dat <- data.frame(rbind(predat, postdat))
final_dat$PrePost <- factor(c(rep(0,100),rep(1,100)))
final_dat$GENDER <- factor(sample(1:2,nrow(df), replace = TRUE))
head(final_dat)


#THE MODEL
#sum responses
final_dat$SUM <- apply(final_dat[,1:5],1,sum)
lm1 <- lm(SUM ~ PrePost + GENDER, data = final_dat)
summary(lm1)

#Multinomial model

library(nnet) #for multinom function

mfit <- multinom(X1 ~ PrePost + GENDER, data=final_dat)
summary(mfit) # Not too helpful
source("http://www.math.montana.edu/shancock/courses/stat539/r/GillenRFunctions.R")
pander(summ.mfit(mfit))

#here we obtain significant reslults for both variables on certain comparisons

model1 <- 'Religion =~ X + X2
Politics =~ X3 + X4
Socioeconomics =~ X5 + X6 + X8
Geography =~ X7
Total_Score =~ Religion + Politics + Socioeconomics + Geography'

onyx(model1)




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

#factor analysis on GAP Data
pc1 <- prcomp(gap)
summary(pc1) #this suggests you could get away with a single Principal Component
plot(pc1$x[,1], pc1$x[,2]) 

#library psych
library(psych)
f1 <- factanal(gap,2,rotation = 'varimax')







