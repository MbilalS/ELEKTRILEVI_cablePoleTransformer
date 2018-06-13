library(readr)
poleLifeSpan <- read_csv("~/Downloads/example_data/elektrilevi/cables_poles_trafos/poleLifeSpan.csv")

poleLifeSpan[poleLifeSpan == 0] <- NA

poleLifeSpan <- na.omit(poleLifeSpan)


library(dplyr)
library(ggplot2)

tmp <- poleLifeSpan[which(poleLifeSpan$CLASSID==650), ]
class_650_MP_wood = group_by(tmp, tmp$AGE)

tmp <- poleLifeSpan[which(poleLifeSpan$CLASSID==651), ]
class_651_MP_concrete = group_by(tmp, tmp$AGE)

tmp <- poleLifeSpan[which(poleLifeSpan$CLASSID==602), ]
class_602_wood = group_by(tmp, tmp$AGE)

tmp <- poleLifeSpan[which(poleLifeSpan$CLASSID==604), ]
class_604_concrete = group_by(tmp, tmp$AGE)


class_650_MP_wood$poleTpye <- 'class_650_MP_wood'
class_651_MP_concrete$poleTpye <- 'class_651_MP_concrete'
class_602_wood$poleTpye <- 'class_602_wood'
class_604_concrete$poleTpye <- 'class_604_concrete'

data <- rbind(class_650_MP_wood, class_651_MP_concrete, class_602_wood, class_604_concrete)
Years <- data$`tmp$AGE`

ggplot(data, aes(Years, fill = poleTpye)) +
  geom_histogram(alpha = 0.5, position = 'identity') +
  scale_x_continuous(breaks = seq(0, 155, 5))


grouped_data = group_by(poleLifeSpan, poleLifeSpan$CLASSID)
LifeSpan <- summarise(grouped_data, average_poleLifeSpan = round(mean(AGE)))


library(plotly)

d<-data.frame(0, 0)
names(d)<-c("poleLifeSpan$CLASSID", "average_poleLifeSpan")
LifeSpan <- rbind(d, LifeSpan)


Age <- as.factor(LifeSpan$average_poleLifeSpan)
Type <- as.factor(LifeSpan$`poleLifeSpan$CLASSID`)




LifeSpan %>%
  plot_ly() %>%
  layout(yaxis = list(range = c(0, 6))) %>%
  add_trace(x = ~Type, y = ~Age, type = 'bar',
            text = Type, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5)))