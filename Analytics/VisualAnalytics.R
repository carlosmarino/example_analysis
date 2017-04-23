########### VISUALIZATION CLASS ############

WHO=read.csv("WHO.csv")
plot(WHO$GNI,WHO$FertilityRate)
#install.packages("ggplot2")
#library(ggplot2)
scatterplot=ggplot(WHO, aes(x=GNI,y=FertilityRate))
scatterplot + geom_point()
scatterplot + geom_point(color="blue", size=3, shape=18)
scatterplot + geom_point(color="darkred", size=3, shape=8)+ggtitle("Fertility Rate versus Gross National Income")
fertilityGNIplot=scatterplot + geom_point(color="darkred", size=3, shape=8)+ggtitle("Fertility Rate versus Gross National Income")


scatterplot=ggplot(WHO, aes(x=Region,y=FertilityRate))
scatterplot + geom_point()

scatterplot=ggplot(WHO, aes(x=LiteracyRate,y=FertilityRate))
scatterplot + geom_point()

scatterplot=ggplot(WHO, aes(x=Region,y=CellularSubscribers))
scatterplot + geom_point()

ggplot(WHO, aes(x=GNI,y=FertilityRate, colour=Region))+ geom_point()

ggplot(WHO, aes(x=GNI,y=FertilityRate, colour=LifeExpectancy))+ geom_point()

ggplot(WHO, aes(x=FertilityRate, y=Under15, colour=LifeExpectancy))+ geom_point()

ggplot(WHO, aes(x=log(FertilityRate), y=Under15, colour=LifeExpectancy))+ geom_point()

model= lm(Under15~log(FertilityRate), data = WHO)

summary(model)

ggplot(WHO, aes(x=log(FertilityRate), y=Under15, colour=LifeExpectancy))+ geom_point() + stat_smooth(method="lm")


ggplot(WHO, aes(x=log(FertilityRate), y=Under15, colour=LifeExpectancy))+ geom_point() + stat_smooth(method="lm", level = 0.99)

ggplot(WHO, aes(x=log(FertilityRate), y=Under15, colour=LifeExpectancy))+ geom_point() + stat_smooth(method="lm", se=FALSE, color="Orange")

