
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# set the directory
setwd("/Results/")

# load the xylazine testing data
data01 <- read.csv("xylazine_data.csv", row.names = 1)
data01$Date <- ymd(data01$Date )

# plot the temporal concentrations for xylazine
ggplot(data01, aes(x=Date, y=Xylazine_ngL, color = Location))+
  geom_point(size = 2) +  geom_line(linewidth=0.4) +  ylim(-0.05, 1.6)+
  theme_light() +   theme_light() + facet_wrap(~Location, nrow = 4)


#### upload the flow data
flow <- read.csv("flowdata_Nov24.csv")
flow$Date <- mdy(flow$Date)
colnames(flow)[1] <- "Location"
df <- merge(data01, flow, c("Date", "Location"))


## calculate the mass load of xylazine, per ng per day
df$massload_ug <- df$Xylazine_ngL * df$AveFlowrateMGD * 3785412  / 1000

# pop size
population <- data.frame(
  Location = c("FH", "HS", "JT", "RB"),
  Popsize = c(98505, 125235, 127250, 399424))

df <- merge(df, population, by = "Location")

df$mass_per1000_ug <- (df$massload_ug / df$Popsize) * 1000
df$mass_per100k_mg <- (df$massload_ug / df$Popsize) * 100000 / 1000

ggplot(df, aes(x=Date, y=mass_per1000_ug, color = Location))+
  geom_point(size = 3) +  geom_line(linewidth=0.4) + 
  theme_light() +   theme_light() + facet_wrap(~Location, nrow = 4) + ylim(-30, 450) + 
  labs(y="Xylazine load (ug/day/1000 people)")

 
## the positiviity
result <- df %>%
  group_by(Location) %>%
  summarise(
    total = n(),
    positive = sum(Xylazine_ngL > 0),
    positive_rate = positive / total
  )

# Lollipop plot
ggplot(result, aes(x = reorder(Location, positive_rate), y = positive_rate, color = Location)) +
  geom_segment(aes(xend = Location, y = 0, yend = positive_rate), linewidth = 1) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Positive Rate by Location",
    x = "Location",
    y = "Positive Rate"
  ) +
  theme_bw() 
 
