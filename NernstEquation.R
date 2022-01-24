F <- 96485
R <- 8.3145
T <- 293
z <- c(1,1,1,2)
c.in <- c(150,12,50,1.25)
c.out <- c(10,120,100,250)

Q <- log(c.out/c.in)
nernst <- ((R*T)/(z*F))*Q
mV <- nernst*1000
mV