ggplot (data = mpg) +
  geom_point(mapping = aes(x = displ,y = hwy), 
             color = "blue") +
  facet_grid(drv ~ cyl)

ggplot (data = mpg) +
  geom_smooth(mapping = aes(x = displ,y = hwy,linetype = drv), 
             color = "blue")
