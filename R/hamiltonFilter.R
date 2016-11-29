#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Implementation of Hamilton (Why You Should Never Use the Hodrick-Prescott Filter, 2016, ch. 6)
# Alternative filter to the Hodrick-Prescott Filter
#
# Florian Eckert and Heiner Mikosch, KOF, ETH Zurich
# This version: October 3, 2016
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Functions
source("lib/convFunctions.R")

eagdp <- ds2ts("EMGDP...D")[[1]]

ystart <- 29

data <- {}
y <- eagdp

potential <- {}

for(i in ystart:length(y)){

  y <- window(eagdp, start=c(1995,1), end= time(eagdp)[i])


  fit <- dynlm(y ~ lag(y, -8) + lag(y, -9) + lag(y, -10) + lag(y, -11))
  summary(fit)

  potential <- c(potential,predict(fit)[length(predict(fit))])


}

potential <- ts(potential, start=time(eagdp)[ystart], frequency=4)

eagdp <- window(eagdp, start=c(2002,1))

series <- cbind(eagdp,potential)
colnames(series) <- c("Euro area real GDP",
                      "Euro area potential output according to the method outlined in Hamilton (2016, Section 6)")
kplot(series)
