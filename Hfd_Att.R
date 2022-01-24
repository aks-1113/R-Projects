library(ggplot2)

hfd_df <- read.csv("/Users/akshayraju/Downloads/2021-08-24_Hereford_AttackingStats.csv",header = TRUE)
summary(hfd_df)
mean(hfd_df$Fin)

hfd_df$Fin <- as.numeric(hfd_df$Fin)
class(hfd_df$Fin)

#Removes ineligible Trial players
hfd = hfd_df[!hfd_df$Inf. == "Ine",]

#Makes attributes columns numeric values
cols <- c(4:13)
hfd1[,cols] <- apply(hfd1[,cols],2,as.numeric)

#Omits NA rows that were there
hfd2 <- na.omit(hfd1)

#Rename positions
PosToAdd <- c("GK","DL","DC","AMC","STC","DR","MC","STC","AML","MC","DC","MC","STC","STC","MC","STC","GK")

apply(hfd2$Position.Selected,2,
replace(hfd2$Position.Selected,hfd2$Position.Selected[12:33],PosToAdd))


for (i in length(PosToAdd)) 
{
  hfd2[(i+11),1] = PosToAdd[i]
  i <- i+1
}

#Graph time!
ggplot (data = hfd2) + 
  geom_col(mapping = aes(x = Name,y = Fin))
