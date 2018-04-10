#Star Wars Factor Analysis

#Exploratory analysis

jedi <- read.csv("sw.csv", header = TRUE)
head(jedi)
names(jedi)

jdat <- na.omit(jedi[,c(18:29)])
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
factor_coerce <- function(x) {as.numeric.factor(factor(x))}
apply(jdat,2,factor_coerce)

##
factanal(jdat, 4) #keep going, need 5
x <- factanal(jdat,5)




##Principal Components Analysis
pc1 <- prcomp(jdat)
plot(pc1$x[,1], pc1$x[,2], type = "n", main = "Principal Components")
text(pc1$x[,1], pc1$x[,2], labels = colnames(jdat))

biplot(pc1, var.axes = TRUE, col = c('gray80','red3'), xlim = c(-.18,.15))

summary(pc1) #suggests we'd need several factors

##SEM

model.jedi <- 'Do.you.consider.yourself.to.be.a.fan.of.the.Star.Trek.franchise =~ Prequel + Sequel + Noob
Demographics =~ Have.you.seen.any.of.the.6.films.in.the.Star.Wars.franchise. + Household.Income + Gender + Education
Prequel =~ 
Sequel =~ 


'

