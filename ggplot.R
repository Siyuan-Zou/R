fill <- "#4271AE"
line <- "#1F3552"
#rm(list = ls())

head(mtcars)

#1
Month=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
high=c(38,42,50,61,71,79,84,83,75,64,54,43)
low=c(27,29,35,45,54,64,69,68,61,50,42,28)

lowts <- ts(low)
hights <- ts(high)

plot(hights, col="blue", lwd =3, ylim=c(0,90),xaxt='n', xlab="Months", ylab="Fahrenheit")
lines(lowts, col="red", lwd =3)
axis(1, at= 1:12, labels = Month)
title(main="Monthly Average NYC Temperature")

legend(11, 90, legend=c("Low", "High"),col=c("red", "blue"), lty=1:1, cex=0.8, lwd=3)

temperature<-data.frame(high,low)
temperature$group<-colnames(temperature)
temperature.m <-melt(temperature, id.vars="group")

ggplot(temperature.m, aes(variable, value))+geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                                                  outlier.colour="black",outlier.shape=16,outlier.size=2,
                                                  notch=FALSE, show.legend = FALSE)+
  labs(x=" ", y="Frequency",title="High temperatures vs. Low temperatures")+
  theme_bw()

#2
set1=c(56,77,93,28,55,34,32,53,52,32,70,27,92,43,21,31,15,93,6,94)
set1Data<-data.frame(set1)
set1Data$group<-colnames(set1Data)


set2=c(72,51,65,31,31,52,61,42,37,45,29,38,21,44,61,63,62,58,76,66,40,68,33,50,38)
set2Data<-data.frame(set2)
set2Data$group<-colnames(set2Data)

set3=c(89,87,52,77,45,71,93,46,68,49,68,54,69,70,40)
set3Data<-data.frame(set3)
set3Data$group<-colnames(set3Data)

t<-rbind.fill(set1Data,set2Data,set3Data)
value<-array(c(set1,set2,set3))
df<-data.frame(value)
fff<-data.frame(df, t)
ggplot(fff, aes(group, value))+geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                                                         outlier.colour="black",outlier.shape=16,outlier.size=2,
                                                         notch=FALSE, show.legend = FALSE)+
  labs(x=" ", y="Frequency",title="Boxplot")+
  theme_bw()

#3
x<-c(55,61,94,94,69,77,68,54,85,77,92,92,81,73,69,81,75,84,70,81,81,89,59,72,82,62)
hist(x)

a<-data.frame(x)
ggplot(a, aes(a$x)) +geom_histogram(fill = fill, colour = line, alpha = 0.7,
                                  binwidth=10, show.legend = FALSE)+
  labs(x=" ", y="Frequency",title="Histogram")+
  theme_bw()


ggplot(a, aes(x="", a$x))+geom_boxplot(fill = fill, colour = line, alpha = 0.7, width=0.5,
                                      outlier.colour="black",outlier.shape=16,outlier.size=2,
                                      notch=FALSE, show.legend = FALSE)+
  labs(x=" ", y="Frequency",title="Boxplot")+
  theme_bw()

stem(x)

#4
defect<-c(45,67,21,25,9,18,6,2)
names(defect)<-c("Wrong Supplier","Excess Count","Too Few Count","Wrong Size","Wrong Sterile Instrument Set",
            "Missing Item","Damaged Item","Other")

pareto.chart(defect,xlab="Error Type", ylab = "Error frequency", col=rainbow(length(defect)))
pareto.chart(defect, cumperc = seq(0, 100, by = 5), ylab2 = "Cumulative Percentage")