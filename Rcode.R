rm(list = ls())
library(readr); library(dplyr);library(splines); library(ggplot2)
library(mgcv); library(parallel); library(splancs); library(PBSmapping)

#Clusters
if (detectCores()>1) {
  cl <- makeCluster(detectCores()-1)
} else cl <- NULL
cl


A <- read_csv("PLAY.csv")
A2 <- read_csv("GAME.csv")
B <- filter(A, type =="FGXP", dwn>0 )
C <- inner_join(B, A2)
C$Success <- C$pts == 3
C$distance <- 100 - C$yfog + 17
C$score.diff <- C$ptso - C$ptsd
C[is.na(C$wspd) =="TRUE",]$wspd <- 0
C[is.na(C$temp) =="TRUE",]$temp <- 60

fit.red0<-bam(Success~s(distance, k=30)+s(temp, k=30) +s(wspd, k=30) + seas,
              method="GCV.Cp", family=binomial(link="logit"), cluster=cl, data=C)


Five38Thm<-theme_bw()+theme(axis.text.x = element_text(family = "DecimaMonoPro",color="#3C3C3C",size=rel(1.6)),
                            axis.text.y = element_text(family = "DecimaMonoPro", color="#3C3C3C", size=rel(1.6)),
                            axis.title.x = element_text(size=rel(1.6), vjust=-1),
                            axis.title.y = element_blank(),
                            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),legend.position = "none",
                            plot.title = element_text(family="AtlasGrotesk-Bold",vjust=2, size=rel(1.6)))

distance <- seq(18, 65, by=.5)
temp <- seq(-1, 93, by = 1)
fitdata <- data.frame(distance = 33, temp = temp, wspd = 15, seas = 2014)
p1 <- predict(fit.red0, fitdata, type="response")
p1.se <- predict(fit.red0,type = "response", fitdata, se.fit=TRUE)$se.fit

plot1<-ggplot() +
  geom_ribbon(aes(ymin = (p1-2*p1.se), ymax =(p1+2*p1.se),x=-1:93),alpha=.2)+
  geom_line(aes(p1,x=-1:93),alpha=1,lwd=2)+
  scale_x_continuous("Temperature")+
  Five38Thm+ggtitle("Probability of an extra point")+
  geom_segment(aes(x = 10, y = 0.8, xend = 0, yend = 0.83), 
               arrow = arrow(length = unit(0.5, "cm")), colour = "black",alpha=0.7)+
  annotate(x = 10, y = 0.79, "text", label = "SEA @ MIN", size = 6)
plot1


fitdata1 <- data.frame(distance = 30, temp = temp, wspd = 15, seas = 2014, type = "thirty")
fitdata2 <- data.frame(distance = 40, temp = temp, wspd = 15, seas = 2014, type = "forty")
fitdata3 <- data.frame(distance = 50, temp = temp, wspd = 15, seas = 2014, type = "fifty")

p1 <- predict(fit.red0, fitdata1, type="response")
p2 <- predict(fit.red0, fitdata2, type="response")
p3 <- predict(fit.red0, fitdata3, type="response")
plot1<-ggplot() +
  geom_line(aes(p1,x=-1:93),alpha=1,lwd=2, colour = 1)+
  geom_line(aes(p2,x=-1:93),alpha=1,lwd=2, colour = 2)+
  geom_line(aes(p3,x=-1:93),alpha=1,lwd=2, colour = 3)+
  scale_x_continuous("Temperature")+
  Five38Thm+ggtitle("Field Goal probability")+
  annotate(x = 50, y = 0.88, "text", label = "30-yarder", size = 6, colour = 1)+
  annotate(x = 50, y = 0.78, "text", label = "40-yarder", size = 6, colour = 2)+
  annotate(x = 50, y = 0.58, "text", label = "50-yarder", size = 6, colour = 3)
plot1
