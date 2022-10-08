# Result5, Figuer A, iCAMP
# Author: Xing Wang
# Data: October 8th 2022

rm(list=ls(all=TRUE))
library(vegan)

rm(list=ls(all=TRUE))
library(dplyr)
library(reshape2)
setwd()

importance <- read.table("Pd.txt", sep="\t", header=T)
group <- read.table("treatment.txt",sep="\t", header=T, row.names=1)

data = transform(importance[,2:3], Selection = importance$Heterogeneous.Selection + importance$Homogeneous.Selection,
                 Dispersal = importance$Dispersal.Limitation + importance$Homogenizing.Dispersal,
                 Drift = importance$Drift.and.Others)
group1 = subset(group, developmental_stages==1)
data1 = data[data$sample1 %in% rownames(group1) & data$sample2 %in% rownames(group1), ]
data1 = transform(data1, developmental_stages = 1)
group2 = subset(group, developmental_stages==2)
data2 = data[data$sample2 %in% rownames(group2) & data$sample2 %in% rownames(group2), ]
data2 = transform(data2, developmental_stages = 2)
group3 = subset(group, developmental_stages==3)
data3 = data[data$sample2 %in% rownames(group3) & data$sample2 %in% rownames(group3), ]
data3 = transform(data3, developmental_stages = 3)
group4 = subset(group, developmental_stages==4)
data4 = data[data$sample2 %in% rownames(group4) & data$sample2 %in% rownames(group4), ]
data4 = transform(data4, developmental_stages = 4)


plotdata = rbind(data1, data2, data3, data4) %>%  dplyr::select('developmental_stages', 'Selection', 'Dispersal', 'Drift') %>% melt( id = "developmental_stages", variable.name = 'Process', value.name = 'Value')

library(ggthemes)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

theme_zg <- function(..., bg='white'){
  require(grid)
  theme_classic(...) +
    theme(rect=element_rect(fill=bg),
          plot.margin=unit(rep(0.5,4), 'lines'),
          panel.background=element_rect(fill='transparent',color='black'),
          panel.border=element_rect(fill='transparent', color='transparent'),
          panel.grid=element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(face = "plain",size = 25),
          axis.title.y=element_text(face = "plain",size = 25),
          axis.text = element_text(face = "plain",size = 25),          
          axis.ticks = element_line(color='black'),
          # axis.ticks.margin = unit(0.8,"lines"),
          legend.title=element_blank(),
          #legend.position=c(0.85, 0.8),
          #legend.direction = "horizontal",
          legend.text = element_text(face = "bold",size = 25,margin = margin(r=8)),
          legend.background = element_rect( linetype="solid",colour ="black")
    )
}

P = ggplot(plotdata, aes(developmental_stages,  Value*100, color = Process)) + geom_point(alpha = 0.5, size= 3) + geom_smooth(method = "lm",se=T,size=3) +
  guides(fill=guide_legend(title=NULL)) + 
  scale_color_manual(values = c("#6495ED", "#FFA500", "#FF4500"))+ 
  facet_wrap(.~ Process, 'free')+
  stat_poly_eq(aes(label = paste( stat(adj.rr.label), stat(p.value.label),sep = '~~~~')), formula = y ~ x,  
               size = 6,
               label.x = 0.2,  
               label.y = "top", parse = T, color = 'black') + 
  theme_zg()+ 
  theme(axis.text.x=element_text(colour="black",family="Times",size=25), 
        axis.text.y=element_text(family="Times",size=25,face="plain",colour="black"),
        strip.text.x = element_text(size=25), strip.text.y = element_text(size=25),
        axis.title.y=element_text(family="Times",size = 25,face="plain"), 
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), 
        legend.text=element_text(face="italic", family="Times", colour="black", 
        ),legend.position= c(1,0),legend.justification = c(1,0),legend.background = element_blank(),legend.key.size = unit(12, "pt"),
        # legend.title=element_text(face="italic", family="Times", colour="black",
        #  size=18),
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())+  
  ylab("Process importance")+xlab("developmental_stages") 
P
jpeg(file = "C:/Users/liujiale/Desktop/Pd.jpg",width =3000,height = 2000,units = "px",res =300)
print(P)
dev.off()

