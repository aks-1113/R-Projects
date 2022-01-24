#CREATING DATA TABLE

people <- c("Tanmoy","Cindy","Lisa","Akshay")
bkrollwins <- c(75,58,59,40)
bkrollplayed <- c(230,249,220,157)

bkrollpercent <- (bkrollwins/bkrollplayed)*100
bkrollpercent1 <- round(bkrollpercent,1)
bkrollpercent1

bkroll.data <- data.frame(people,bkrollwins,bkrollplayed,bkrollpercent)
bkroll.data

summary(bkroll.data)

#BAR GRAPH

library(ggplot2)
ggplot(bkroll.data, aes(x = people, y = bkrollpercent1, label = bkrollpercent1, fill = bkrollpercent1)) +
  geom_bar(stat = "identity") +
  labs(x = "Bumasses",y = "Win Percentage") +
  geom_text(size = 3, vjust = -1) +
  scale_fill_gradient(low = "darkgreen", high = "green", name = "Big Bucks Scale") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0))
  
#GRAPHS FOR NET WORTH

lisa.nw <- c(4235)
akshay.nw <- c(3015)
tanmoy.nw <- c(-545)
cindy.nw <- c(2395)

#Test for GIT