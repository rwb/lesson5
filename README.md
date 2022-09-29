### Lesson 5 - Thursday 9/29/22

* We begin by reading in our Minneapolis dataset from last week.

```r
df <- read.csv(file="minn.txt",sep=",",header=T)
head(df,n=10)
tail(df,n=10)
```

* Here is our output:

```rout
> df <- read.csv(file="minn.txt",sep=",",header=T)
> head(df,n=10)
   id ta td aggcirc y
1   1  1  1       1 1
2   2  1  1       1 1
3   3  1  1       1 1
4   4  1  1       1 1
5   5  1  1       1 1
6   6  1  1       1 1
7   7  1  1       1 1
8   8  1  1       1 0
9   9  1  1       1 0
10 10  1  1       1 0
> tail(df,n=10)
     id ta td aggcirc y
304 304  3  3       0 0
305 305  3  3       0 0
306 306  3  3       0 0
307 307  3  3       0 0
308 308  3  3       0 0
309 309  3  3       0 0
310 310  3  3       0 0
311 311  3  3       0 0
312 312  3  3       0 0
313 313  3  3       0 0
> 
```

* Next, we carry out a recode operation as follows:

```R
table(df$y,df$ta,exclude=NULL)

df$x <- rep(NA,313)
df$x[df$ta==1] <- 1
df$x[df$ta==2 | df$ta==3] <- 0
table(df$x,df$ta,exclude=NULL)

x <- df$x
y <- df$y
```

which yields the following output:


```Rout
> table(df$y,df$ta,exclude=NULL)
   
     1  2  3
  0 82 87 87
  1 10 21 26
> 
> df$x <- rep(NA,313)
> df$x[df$ta==1] <- 1
> df$x[df$ta==2 | df$ta==3] <- 0
> table(df$x,df$ta,exclude=NULL)
   
      1   2   3
  0   0 108 113
  1  92   0   0
> 
```

* Now, we use the ```glm()''' function to estimate a logistic regression model:

```r
# estimate logistic regression model

lr <- glm(y~1+x,data=df,family="binomial"(link="logit"))
summary(lr)
logLik(lr)
```

* Here are our results:

```rout
> # estimate logistic regression model
> 
> lr <- glm(y~1+x,family="binomial"(link="logit"))
> summary(lr)

Call:
glm(formula = y ~ 1 + x, family = binomial(link = "logit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6915  -0.6915  -0.6915  -0.4797   2.1067  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.3089     0.1644  -7.962 1.69e-15 ***
x            -0.7952     0.3731  -2.131   0.0331 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.98  on 311  degrees of freedom
AIC: 295.98

Number of Fisher Scoring iterations: 4

> logLik(lr)
'log Lik.' -145.9891 (df=2)
> 
```

* Now, we want to look at how we find the maximum likelihood solution for this
problem. This is a grid-search analysis to find the approximate maximum likelihood
solution:

```r
# logistic regression likelihood maximization exercise

a <- seq(from=-2,to=2,by=0.01)
b <- seq(from=-2,to=2,by=0.01)
c <- expand.grid(a,b)
nrow(c)

llf <- vector()
for(i in 1:nrow(c)) {
  py1 <- exp(c$Var1[i]+c$Var2[i]*x)/(1+exp(c$Var1[i]+c$Var2[i]*x))
  py0 <- 1-(exp(c$Var1[i]+c$Var2[i]*x)/(1+exp(c$Var1[i]+c$Var2[i]*x)))
  llf[i] <- sum(y*log(py1)+(1-y)*log(py0))
  }

max.df <- data.frame(c$Var1,c$Var2,llf)[order(-llf),]    
head(max.df,n=30)
```

* And, here are the results:

```rout
> # logistic regression likelihood maximization exercise
> 
> a <- seq(from=-2,to=2,by=0.01)
> b <- seq(from=-2,to=2,by=0.01)
> c <- expand.grid(a,b)
> nrow(c)
[1] 160801
> 
> llf <- vector()
> for(i in 1:nrow(c)) {
+   py1 <- exp(c$Var1[i]+c$Var2[i]*x)/(1+exp(c$Var1[i]+c$Var2[i]*x))
+   py0 <- 1-(exp(c$Var1[i]+c$Var2[i]*x)/(1+exp(c$Var1[i]+c$Var2[i]*x)))
+   llf[i] <- sum(y*log(py1)+(1-y)*log(py0))
+   }
> 
> max.df <- data.frame(c$Var1,c$Var2,llf)[order(-llf),]    
> head(max.df,n=30)
      c.Var1 c.Var2       llf
48591  -1.31  -0.79 -145.9892
48190  -1.31  -0.80 -145.9893
48992  -1.31  -0.78 -145.9900
47789  -1.31  -0.81 -145.9903
48191  -1.30  -0.80 -145.9907
47790  -1.30  -0.81 -145.9907
48991  -1.32  -0.78 -145.9915
48592  -1.30  -0.79 -145.9915
48590  -1.32  -0.79 -145.9915
47389  -1.30  -0.82 -145.9917
49393  -1.31  -0.77 -145.9918
47388  -1.31  -0.82 -145.9921
49392  -1.32  -0.77 -145.9923
48189  -1.32  -0.80 -145.9925
48993  -1.30  -0.78 -145.9932
46988  -1.30  -0.83 -145.9935
49793  -1.32  -0.76 -145.9940
47788  -1.32  -0.81 -145.9944
49794  -1.31  -0.76 -145.9944
46987  -1.31  -0.83 -145.9948
49394  -1.30  -0.77 -145.9958
47791  -1.29  -0.81 -145.9958
47390  -1.29  -0.82 -145.9959
46587  -1.30  -0.84 -145.9963
50194  -1.32  -0.75 -145.9966
48192  -1.29  -0.80 -145.9966
46989  -1.29  -0.83 -145.9969
47387  -1.32  -0.82 -145.9971
49391  -1.33  -0.77 -145.9974
48990  -1.33  -0.78 -145.9975
> 
```

* These results confirm that the maximum likelihood solution is indeed close
to the exact solution produced by the logistic regression program.
* Next, we estimate a probit regression model:

```r
# estimate probit regression model

pr <- glm(y~1+x,family="binomial"(link="probit"))
summary(pr)
logLik(pr)
```

* Here are the results:

```rout
> # estimate probit regression model
> 
> pr <- glm(y~1+x,family="binomial"(link="probit"))
> summary(pr)

Call:
glm(formula = y ~ 1 + x, family = binomial(link = "probit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6915  -0.6915  -0.6915  -0.4797   2.1067  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.7972     0.0948  -8.409   <2e-16 ***
x            -0.4363     0.1982  -2.201   0.0277 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.98  on 311  degrees of freedom
AIC: 295.98

Number of Fisher Scoring iterations: 4

> logLik(pr)
'log Lik.' -145.9891 (df=2)
> 
```

* Notice that the probit log-likelihood is virtually identical
to the logistic regression log-likelihood.
* We can also conduct a grid search to find the approximate
maximum likelihood estimate:

```
# probit regression likelihood maximization exercise

a <- seq(from=-2,to=2,by=0.01)
b <- seq(from=-2,to=2,by=0.01)
c <- expand.grid(a,b)
nrow(c)

llf <- vector()
for(i in 1:nrow(c)) {
  py1 <- pnorm(c$Var1[i]+c$Var2[i]*x)
  py0 <- 1-pnorm(c$Var1[i]+c$Var2[i]*x)
  llf[i] <- sum(y*log(py1)+(1-y)*log(py0))
  }

max.df <- data.frame(c$Var1,c$Var2,llf)[order(-llf),]    
head(max.df,n=30)
```

* Here are the grid search results for the probit regression:

```rout
> # probit regression likelihood maximization exercise
> 
> a <- seq(from=-2,to=2,by=0.01)
> b <- seq(from=-2,to=2,by=0.01)
> c <- expand.grid(a,b)
> nrow(c)
[1] 160801
> 
> llf <- vector()
> for(i in 1:nrow(c)) {
+   py1 <- pnorm(c$Var1[i]+c$Var2[i]*x)
+   py0 <- 1-pnorm(c$Var1[i]+c$Var2[i]*x)
+   llf[i] <- sum(y*log(py1)+(1-y)*log(py0))
+   }
> 
> max.df <- data.frame(c$Var1,c$Var2,llf)[order(-llf),]    
> head(max.df,n=30)
      c.Var1 c.Var2       llf
63078  -0.80  -0.43 -145.9898
62677  -0.80  -0.44 -145.9903
62678  -0.79  -0.44 -145.9922
63479  -0.80  -0.42 -145.9926
62277  -0.79  -0.45 -145.9927
62276  -0.80  -0.45 -145.9940
63079  -0.79  -0.43 -145.9950
61876  -0.79  -0.46 -145.9965
63478  -0.81  -0.42 -145.9984
63880  -0.80  -0.41 -145.9987
63077  -0.81  -0.43 -145.9989
61875  -0.80  -0.46 -146.0011
63480  -0.79  -0.42 -146.0012
63879  -0.81  -0.41 -146.0012
62676  -0.81  -0.44 -146.0027
61475  -0.79  -0.47 -146.0035
62278  -0.78  -0.45 -146.0058
61877  -0.78  -0.46 -146.0063
64280  -0.81  -0.40 -146.0074
64281  -0.80  -0.40 -146.0082
62679  -0.78  -0.44 -146.0086
62275  -0.81  -0.45 -146.0098
61476  -0.78  -0.47 -146.0101
63881  -0.79  -0.41 -146.0107
61474  -0.80  -0.47 -146.0114
61074  -0.79  -0.48 -146.0138
63080  -0.78  -0.43 -146.0148
64681  -0.81  -0.39 -146.0169
61075  -0.78  -0.48 -146.0171
63878  -0.82  -0.41 -146.0182
> 
```

* Notice that the parameter estimates are different for the 
probit model. This doesn't mean that any of our interpretations
will be different, however. We will return to this issue shortly.

* Now, we turn to the linear probability model:

```r
# estimate linear probability model

lp <- lm(y~1+x)
summary(lp)
logLik(lp)
```

* And, here are the results:

```rout
> # estimate linear probability model
> 
> lp <- lm(y~1+x)
> summary(lp)

Call:
lm(formula = y ~ 1 + x)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.2127 -0.2127 -0.2127 -0.1087  0.8913 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.21267    0.02585   8.228 5.28e-15 ***
x           -0.10397    0.04768  -2.181   0.0299 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3842 on 311 degrees of freedom
Multiple R-squared:  0.01506,	Adjusted R-squared:  0.0119 
F-statistic: 4.756 on 1 and 311 DF,  p-value: 0.02994

> logLik(lp)
'log Lik.' -143.7486 (df=3)
> 
```

* We can also find the approximate maximum likelihood solution for
this problem as follows. Notice that now we have three parameters 
to estimate -- which increases the complexity of the problem:

```r
# normal (ls) maximum likelihood

a <- seq(from=0,to=1,by=0.01)
b <- seq(from=-1,to=1,by=0.01)
s <- seq(from=0,to=1,by=0.01)
c <- expand.grid(a,b,s)
nrow(c)

llf <- vector()
for(i in 1:nrow(c)) {
  pt1 <- 1/(c$Var3[i]*sqrt(2*pi))
  pt2 <- y-(c$Var1[i]+c$Var2[i]*x)
  pt3 <- (pt2/c$Var3[i])^2 
  pt4 <- exp(-0.5*pt3)
  pdf <- pt1*pt4
  llf[i] <- sum(log(pdf))
  }

max.df <- data.frame(c$Var1,c$Var2,c$Var3,llf)[order(-llf),]    
head(max.df,n=30)
```

* Here are the results:

```rout
> # normal (ls) maximum likelihood
> 
> a <- seq(from=0,to=1,by=0.01)
> b <- seq(from=-1,to=1,by=0.01)
> s <- seq(from=0,to=1,by=0.01)
> c <- expand.grid(a,b,s)
> nrow(c)
[1] 2050401
> 
> llf <- vector()
> for(i in 1:nrow(c)) {
+   pt1 <- 1/(c$Var3[i]*sqrt(2*pi))
+   pt2 <- y-(c$Var1[i]+c$Var2[i]*x)
+   pt3 <- (pt2/c$Var3[i])^2 
+   pt4 <- exp(-0.5*pt3)
+   pdf <- pt1*pt4
+   llf[i] <- sum(log(pdf))
+   }
> 
> max.df <- data.frame(c$Var1,c$Var2,c$Var3,llf)[order(-llf),]    
> head(max.df,n=30)
       c.Var1 c.Var2 c.Var3       llf
780550   0.21  -0.10   0.38 -143.7743
780449   0.21  -0.11   0.38 -143.7979
780450   0.22  -0.11   0.38 -143.8100
780651   0.21  -0.09   0.38 -143.8145
780349   0.22  -0.12   0.38 -143.8335
780551   0.22  -0.10   0.38 -143.8501
800851   0.21  -0.10   0.39 -143.8553
800750   0.21  -0.11   0.39 -143.8777
780348   0.21  -0.12   0.38 -143.8851
800751   0.22  -0.11   0.39 -143.8892
780650   0.20  -0.09   0.38 -143.8917
800952   0.21  -0.09   0.39 -143.8934
800650   0.22  -0.12   0.39 -143.9115
780549   0.20  -0.10   0.38 -143.9152
780752   0.21  -0.08   0.38 -143.9184
780248   0.22  -0.13   0.38 -143.9208
800852   0.22  -0.10   0.39 -143.9273
780751   0.20  -0.08   0.38 -143.9319
780652   0.22  -0.09   0.38 -143.9540
800649   0.21  -0.12   0.39 -143.9605
800951   0.20  -0.09   0.39 -143.9667
800850   0.20  -0.10   0.39 -143.9891
801053   0.21  -0.08   0.39 -143.9921
800549   0.22  -0.13   0.39 -143.9944
780350   0.23  -0.12   0.38 -143.9987
780448   0.20  -0.11   0.38 -144.0025
801052   0.20  -0.08   0.39 -144.0049
780249   0.23  -0.13   0.38 -144.0222
800953   0.22  -0.09   0.39 -144.0259
780852   0.20  -0.07   0.38 -144.0357
> 
```

* A problem that arises with the linear probability model is that
the error variance is not constant (heteroscedasticity). One remedy
for this problem is to calculate a weighted least squares regression:

```r
# calculate weights

intercept <- coef(lp)[1]
intercept
reg.coeff <- coef(lp)[2]
reg.coeff
lp.yhat <- intercept+reg.coeff*x
wt <- 1/(lp.yhat*(1-lp.yhat))

# estimate weighted least squares model

wlp <- lm(y~1+x,weights=wt)
summary(wlp)
```

* Here is the output:

```rout
> # calculate weights
> 
> intercept <- coef(lp)[1]
> intercept
(Intercept) 
  0.2126697 
> reg.coeff <- coef(lp)[2]
> reg.coeff
        x 
-0.103974 
> lp.yhat <- intercept+reg.coeff*x
> wt <- 1/(lp.yhat*(1-lp.yhat))
> 
> # estimate weighted least squares model
> 
> wlp <- lm(y~1+x,weights=wt)
> summary(wlp)

Call:
lm(formula = y ~ 1 + x, weights = wt)

Weighted Residuals:
    Min      1Q  Median      3Q     Max 
-0.5197 -0.5197 -0.5197 -0.3492  2.8636 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.21267    0.02761   7.702 1.81e-13 ***
x           -0.10397    0.04269  -2.436   0.0154 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.003 on 311 degrees of freedom
Multiple R-squared:  0.01872,	Adjusted R-squared:  0.01556 
F-statistic: 5.932 on 1 and 311 DF,  p-value: 0.01543

> 
```

* Notice that the standard error of our intercept term has
gotten larger while the standard error for the slope coefficient
has gotten smaller.

* Weighted least squares is a minimization (hence, the term "least")
problem. We can solve this problem with another grid search:

```r
# minimization exercise

a <- seq(from=0,to=1,by=0.001)
b <- seq(from=-1,to=1,by=0.001)
c <- expand.grid(a,b)

fun <- vector()
for(i in 1:nrow(c)) {
  fun[i] <- sum(wt*(y-(c$Var1[i]+c$Var2[i]*x))^2)
  }

min.df <- data.frame(c$Var1,c$Var2,fun)[order(fun),]    
head(min.df,n=30)
```

* Here are the results:

```rout
> # minimization exercise
> 
> a <- seq(from=0,to=1,by=0.001)
> b <- seq(from=-1,to=1,by=0.001)
> c <- expand.grid(a,b)
> 
> fun <- vector()
> for(i in 1:nrow(c)) {
+   fun[i] <- sum(wt*(y-(c$Var1[i]+c$Var2[i]*x))^2)
+   }
> 
> min.df <- data.frame(c$Var1,c$Var2,fun)[order(fun),]    
> head(min.df,n=30)
       c.Var1 c.Var2      fun
897110  0.213 -0.104 313.0002
896109  0.213 -0.105 313.0006
898110  0.212 -0.103 313.0007
897109  0.212 -0.104 313.0011
898111  0.213 -0.103 313.0018
899111  0.212 -0.102 313.0022
896110  0.214 -0.105 313.0024
895109  0.214 -0.106 313.0028
895108  0.213 -0.106 313.0029
896108  0.212 -0.105 313.0033
899110  0.211 -0.102 313.0038
897111  0.214 -0.104 313.0040
898109  0.211 -0.103 313.0041
894108  0.214 -0.107 313.0051
899112  0.213 -0.102 313.0052
900111  0.211 -0.101 313.0053
900112  0.212 -0.101 313.0056
897108  0.211 -0.104 313.0064
894107  0.213 -0.107 313.0070
895110  0.215 -0.106 313.0073
898112  0.214 -0.103 313.0074
895107  0.212 -0.106 313.0075
894109  0.215 -0.107 313.0076
901112  0.211 -0.100 313.0087
896111  0.215 -0.105 313.0088
893107  0.214 -0.108 313.0092
900110  0.210 -0.101 313.0095
899109  0.210 -0.102 313.0099
893108  0.215 -0.108 313.0099
900113  0.213 -0.101 313.0105
> 
```

* Now, let's see how WLS differs from simple
linear regression in the context of repeated sampling.
* Let's assume we have a population regression function
that looks like the Minneapolis study. 

```r
# let's construct a simulation study to look at 
# the repeated sample performance of the ols and 
# wls estimators

b0 <- vector()
bx <- vector()
se.b0 <- vector()
se.bx <- vector()

wb0 <- vector()
wbx <- vector()
se.wb0 <- vector()
se.wbx <- vector()

for(j in 1:10000) {
  u <- runif(n=313,min=0,max=1)
  ostar <- -1.3089-0.7952*x+log(u/(1-u))
  o <- rep(NA,313)
  o[ostar>0] <- 1
  o[ostar<0] <- 0
  sm1 <- lm(o~1+x)
  b0[j] <- coef(sm1)[1]
  bx[j] <- coef(sm1)[2]
  se.b0[j] <- sqrt(diag(vcov(sm1)))[1]
  se.bx[j] <- sqrt(diag(vcov(sm1)))[2]
  p.hat <- coef(sm1)[1]+coef(sm1)[2]*x
  w <- 1/(p.hat*(1-p.hat))
  sm2 <- lm(o~1+x,weights=w)
  wb0[j] <- coef(sm2)[1]
  wbx[j] <- coef(sm2)[2]
  se.wb0[j] <- sqrt(diag(vcov(sm2)))[1]
  se.wbx[j] <- sqrt(diag(vcov(sm2)))[2]
  }
```

* Next, when this loop finishes, we can look at 
some of the resulting quantities:

```r
# intercept estimates

mean(b0)
sd(b0)
mean(se.b0)

mean(wb0)
sd(wb0)
mean(se.wb0)

# slope estimates

mean(bx)
sd(bx)
mean(se.bx)

mean(wbx)
sd(wbx)
mean(se.wbx)
```

* Here are the results:

```rout
> # intercept estimates
> 
> mean(b0)
[1] 0.2125652
> sd(b0)
[1] 0.02729389
> mean(se.b0)
[1] 0.02574354
> 
> mean(wb0)
[1] 0.2125652
> sd(wb0)
[1] 0.02729389
> mean(se.wb0)
[1] 0.02751639
> 
> # slope estimates
> 
> mean(bx)
[1] -0.1038717
> sd(bx)
[1] 0.04208526
> mean(se.bx)
[1] 0.04748392
> 
> mean(wbx)
[1] -0.1038717
> sd(wbx)
[1] 0.04208526
> mean(se.wbx)
[1] 0.04238425
> 
```

* Now, we turn to the task of model selection. We've
spent a good bit of time talking about the likelihood
ratio test but that only works when we can write the
constrained model as a special case of the free model.

* There are some other options:

```r
# Akaike Information Criterion (AIC)
# choose specification that maximizes AIC

aic.lp <- logLik(lp)-3
aic.lp
aic.lr <- logLik(lr)-2
aic.lr

# Bayesian Information (Schwarz) Criterion (BIC)
# choose specification that maximizes BIC

bic.lp <- logLik(lp)-(3/2)*log(313)
bic.lp
bic.lr <- logLik(lr)-(2/2)*log(313)
bic.lr
```

* Here are the results:

```rout
> # Akaike Information Criterion (AIC)
> # choose specification that maximizes AIC
> 
> aic.lp <- logLik(lp)-3
> aic.lp
'log Lik.' -146.7486 (df=3)
> aic.lr <- logLik(lr)-2
> aic.lr
'log Lik.' -147.9891 (df=2)
> 
> # Bayesian Information (Schwarz) Criterion (BIC)
> # choose specification that maximizes BIC
> 
> bic.lp <- logLik(lp)-(3/2)*log(313)
> bic.lp
'log Lik.' -152.3679 (df=3)
> bic.lr <- logLik(lr)-(2/2)*log(313)
> bic.lr
'log Lik.' -151.7353 (df=2)
> 
```

* The formulas for AIC and BIC differ in different textbooks and
articles. The ones I am using here come from Wasserman (2000; [link](https://www.sciencedirect.com/science/article/abs/pii/S0022249699912786) which you should look at (especially section 6).
* Next, we consider the interpretation of the results in terms of the classical treatment effect 
(relative risk and odds ratios could also be calculated.

```r
# p(y=1|x=0) - logit form

exp(-1.3089)/(1+exp(-1.3089))

# p(y=1|x=1) - logit form

exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))

# delta - logit form

exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))-exp(-1.3089)/(1+exp(-1.3089))

# p(y=1|x=0) - probit form

pnorm(-0.7972)
z <- rnorm(n=1000000,mean=0,sd=1)
ecdf(z)(-0.7972)

# p(y=1|x=1) - probit form

pnorm(-0.7972-0.4363)
ecdf(z)(-0.7972-0.4363)

# delta - probit form

pnorm(-0.7972-0.4363)-pnorm(-0.7972)
```

* Here are the results:

```rout
> # p(y=1|x=0) - logit form
> 
> exp(-1.3089)/(1+exp(-1.3089))
[1] 0.212671
> 
> # p(y=1|x=1) - logit form
> 
> exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))
[1] 0.108699
> 
> # delta - logit form
> 
> exp(-1.3089-0.7952)/(1+exp(-1.3089-0.7952))-exp(-1.3089)/(1+exp(-1.3089))
[1] -0.103972
> 
> # p(y=1|x=0) - probit form
> 
> pnorm(-0.7972)
[1] 0.2126674
> z <- rnorm(n=1000000,mean=0,sd=1)
> ecdf(z)(-0.7972)
[1] 0.213002
> 
> # p(y=1|x=1) - probit form
> 
> pnorm(-0.7972-0.4363)
[1] 0.1086946
> ecdf(z)(-0.7972-0.4363)
[1] 0.109067
> 
> # delta - probit form
> 
> pnorm(-0.7972-0.4363)-pnorm(-0.7972)
[1] -0.1039728
> 
```
