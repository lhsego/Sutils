# Script for creating a multi attribute utility function with SQM
library(SQM)

# Create single attribute utility (SAU) functionfor fault cost
faultCost <- saUtil("expUtil", zrange = c(2000, 14000), urange = c(1, 0), certEquiv = 11000)

# Look at the faultCost SAU function
plot(faultCost, xlab = "Value of Fault Cost")

# Create an SAU for energy use of the fault
faultEnergy <- saUtil("expUtil", zrange = c(20000, 140000), urange = c(1, 0), certEquiv = 100000)

# Look at the faultEnergy SAU function
plot(faultEnergy, xlab = "Value of Fault Energy")

# For example: we can use these functions to compute the single attribute utility if we want
faultCost(c(6000, 2000, 10000))
faultEnergy(c(40000, 50000, 60000))

# Now let's create the multiattribute utility function
maU <- maUtil(alpha = c(fc = 0.7, fe = 0.3), saUtilFun = list(fc = faultCost, fe = faultEnergy))

pdf(file="tmp.pdf")
plot(maU)
dev.off()

# Let's make some data to pass into our multi attribute utility function, using the same
# points we used above
x <- data.frame(fc = c(6000, 2000, 10000), fe = c(40000, 50000, 60000))
x

# Now evaluate the multiattribute utility with the data
maU(x)

# Check it
0.7 * faultCost(6000) + 0.3 * faultEnergy(40000)

