
cl = 1-2*pnorm(-1) ## Confidence level corresponding to standard error
alpha = 1 - cl
J = 3 ## Number of groups
N = 10 ## observations in a group

df1 = J - 1
df2 = J * (N - 1)

Fstat0 = qf(alpha/2, df1, df2) ## an F statistic just on the border of both 
Fstat = Fstat0+.001

set.seed(1) ## reset seed (so that it is consistent with supplement)

y = fake.data.F(Fstat, df1, df2)
aov.results = summary(aov(y~grp, data = y))
SStr = aov.results[[1]][1,2]
MSE = aov.results[[1]][2,3]
SSto = aov.results[[1]][1,2] + aov.results[[1]][2,2]
omega.sq.est = (SStr - df1 * MSE) / (SSto + MSE)

testCI = steigerCI.omega2(Fstat,df1,df2,conf.level=cl)
HPD = BayesHPD2.omega2(Fstat,df1,df2,conf.level=cl)
