# Creating the dataframe

salt_sol <- c("Distilled Water","Salt 1","Salt 2","Salt 3")
concentration <- c(0,1,0.5,0.25)
conductivity <- c(69,16014,9415,4837)
lab1 <- data.frame(salt_sol,concentration,conductivity)

# Creating the graph with line of best fit using geom_smooth
# with method lm for linear regression

library(ggplot2)
ggplot (data = lab1, aes(x = concentration, y = conductivity)) +
  geom_point(color = "blue") +
  labs(x = "Concentration (%)", y = "Conductivity (microsiemens/cm)") +
  geom_smooth(method = "lm", se = FALSE, color = "red")

# Linear regression using lm function 
# lm (y axis ~ x axis)
# coef is to take out the values as coefficients and store as a vector
# then referred to values in vectors (see cf(1)) to call them and assign them

linearreg <- lm (conductivity ~ concentration, data = lab1)
cf <- coef(linearreg)
intercept <- as.numeric(cf[1])
slope <- as.numeric(cf[2])

#Used calculated slope and y-int to interpolate 
#concentration (x) values using conductivity (y) values (in conc_vals)
#print.data.frame allows you to generate a table

y_vals <- c(829,42599,48084)
conc_vals <- (y_vals - intercept)/slope
tab1 <- data.frame(conc_vals,y_vals)
print.data.frame(tab1)
