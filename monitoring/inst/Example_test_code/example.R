
# Make some data
X <- c(rgamma(200,shape=0.9,scale=2.136),rgamma(150,shape=0.9,scale=3))

# Give it datetime names
names(X) <- Sys.time() + cumsum(3600*(X+10))

# Fit the gamma Cusum
t1 <- gammaCusum(X, beta.shift=1.25, historical=200, arl.0=500, verbose=TRUE,
                 title="Look at this title", fit.hist.only=TRUE)

# plot the gamma Cusum...
plot(t1,title="try.this",ooc.labels=5230+(1:150))
plot(t1)
plot(t1,title="try.this",ooc.labels=5230+(1:150), margin.title="Great",
     constant.y.axis=TRUE)

# redo a bit
t2 <- gammaCusum(X[201:350], beta.shift=1.25, ic.parms=list(alpha=t1$alpha.0,
                 beta=t1$beta.0), reset=FALSE, check=FALSE, h=t1$h)

plot(t2,title="try.this",ooc.labels=5230+(1:150))
plot(t2)
plot(t2,title="try.this",ooc.labels=5230+(1:150), margin.title="Great",
     constant.y.axis=TRUE)

# try again
Y <- X
names(Y) <- 301:650
t3 <- gammaCusum(Y[201:350], beta.shift=1.25, ic.parms=list(alpha=t1$alpha.0,
                 beta=t1$beta.0), check=FALSE, h=t1$h)

plot(t3,title="try.this",ooc.labels=5230+(1:150))
plot(t3)
plot(t3,title="try.this",ooc.labels=5230+(1:150), margin.title="Great",
     constant.y.axis=TRUE)


# yet again
t4 <- gammaCusum(X, beta.shift=1.25, process.historical=TRUE, historical=200,
                 h=t1$h)


plot(t4,title="try.this",ooc.labels=5230+(1:350))
plot(t4)
plot(t4,title="try.this",ooc.labels=5230+(1:350), margin.title="Great",
     constant.y.axis=TRUE, ylab=NULL)




# Make some data
X <- c(rgamma(50,shape=0.9,scale=2.136), 24,
       rgamma(50,shape=0.9,scale=2.136), 30,
       rgamma(50,shape=0.9,scale=2.136), 81,
       rgamma(50,shape=0.9,scale=2.136),
       rgamma(150,shape=0.9,scale=3))

names(X) <- Sys.time() + cumsum(3600*(X+10))
t5 <- gammaCusum(X, beta.shift=1.25, historical=200, arl.0=500, verbose=TRUE, fit.hist.only=TRUE)
t5 <- gammaCusum(X, beta.shift=1.25, historical=200, arl.0=500, verbose=TRUE)

plot(t5, ooc.labels=5323+1:150, margin.title="Test this stuff", title="That's good",
     include.data=TRUE, constant.y.axis=TRUE, ylab.right="GAS")
