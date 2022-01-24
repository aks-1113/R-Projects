install.packages("gapminder")
library(gapminder)
data("gapminder")

summary(gapminder)
mean(gapminder$gdpPercap)

attach(gapminder)
mean(gdpPercap)
boxplot(lifeExp ~ continent)
plot(lifeExp ~ log(gdpPercap))
