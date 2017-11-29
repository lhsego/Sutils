# Bernoulli CUSUM

# Finding the adjusted value of p1 so that r2/r1 is an integer
# I tested find.pa (2/5/05 and it works)

Bernoulli.find.pa <- function(p0,gamma) {

    p1 <- p0*gamma
  
    r1 <- function(p) -log((1-p)/(1-p0))
    r2 <- function(p) log( (p*(1-p0)) / (p0*(1-p)) )

    m <- round(r2(p1)/r1(p1),0)

    findpa <- function(p1a) r2(p1a)/r1(p1a) - m

    findroot <- uniroot(findpa,
                        c(max(0.000000001,p1-1.15*p1),
                          min(0.99999999,p1+1.15*p1)),
                        tol=0.00000000001)

    return(list(p1.a=findroot$root,m=m,check=findroot$f.root))
}

