library(ggplot2)
library(wesanderson)

Flower <- read.csv("四叶草_单元.csv") 
N_Flower<-nrow(Flower)
crime <- read.csv("crimeRatesByState2005.tsv",
                  header = TRUE, sep = "\t", stringsAsFactors = F)

mydata<-crime[,c(2,4,3,5,6,7)]
N<-nrow(mydata)

Scale_x<-8
Scale_y<-4
xlabel<-seq(2.5,10,2.5)
xbreak<-(xlabel-min(mydata[,1]))/(max(mydata[,1])-min(mydata[,1]))*Scale_x+0.1
ylabel<-seq(0,280,100)
ybreak<-(ylabel-min(mydata[,2]))/(max(mydata[,2])-min(mydata[,2]))*Scale_y+0.1


mydata[,1]<-(mydata[,1]-min(mydata[,1]))/(max(mydata[,1])-min(mydata[,1]))*Scale_x+0.1
mydata[,2]<-(mydata[,2]-min(mydata[,2]))/(max(mydata[,2])-min(mydata[,2]))*Scale_y+0.1

for (j in 3:6){
  mydata[,j]<-(mydata[,j]-min(mydata[,j]))/(max(mydata[,j])-min(mydata[,j]))+0.1
  for (i in 1:N){
    temp<-cbind(Group_Scatter=rep(i,N_Flower,1),
                Group_Flower=rep(colnames(mydata)[j],N_Flower,1),
                x=rep(mydata[i,1],N_Flower,1),
                y=rep(mydata[i,2],N_Flower,1),
                mydata[i,j]*Flower[Flower$Group==(j-2),c(1,2)])
    if (i==1){
      temp_Flower<-temp
    }else
    {
      temp_Flower<-rbind(temp_Flower,temp)
    }
    
  }
  
  if (j==3){
    Flower_data<-temp_Flower
  }else{
    Flower_data<-rbind(Flower_data,temp_Flower)
  }
}

Flower_data$Group_Flower<-as.factor(Flower_data$Group_Flower)


ggplot(data=Flower_data)+
  geom_curve(aes(x=x,y=y,xend = x+0.00000001, yend =0),colour="grey50",size=0.1,curvature = 0.2,alpha=0.9)+
  geom_polygon(aes(x=x+Flower_x,y=y+Flower_y,fill=Group_Flower,
                   group=interaction(Group_Flower,Group_Scatter)),size=0.1,colour="black",alpha=0.9)+
  scale_fill_manual(values = wes_palette("Darjeeling",5)[c(1,2,3,5)])+
  scale_x_continuous(breaks=xbreak, labels=xlabel)+
  scale_y_continuous(breaks=ybreak, labels=ylabel)+
  xlab(colnames(mydata)[1])+
  ylab(colnames(mydata)[2])+
  coord_fixed()
