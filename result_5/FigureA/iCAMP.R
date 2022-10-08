rm(list=ls(all=TRUE))
library(vegan)

#长数据变宽数据
rm(list=ls(all=TRUE))
library(dplyr)
library(reshape2)
setwd("C:/Users/liujiale/Desktop")

importance <- read.table("C:/Users/liujiale/Desktop/Pd.txt", sep="\t", header=T)
group <- read.table("C:/Users/liujiale/Desktop/treatment.txt",sep="\t", header=T, row.names=1)

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
          panel.grid=element_blank(),#去网格线
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(face = "plain",size = 25),#去x轴标签
          axis.title.y=element_text(face = "plain",size = 25),#y轴标签加粗及字体大小
          axis.text = element_text(face = "plain",size = 25),#坐标轴刻度标签加粗          
          axis.ticks = element_line(color='black'),
          # axis.ticks.margin = unit(0.8,"lines"),
          legend.title=element_blank(),
          #legend.position=c(0.85, 0.8),#图例在绘图区域的位置
          #legend.direction = "horizontal",
          legend.text = element_text(face = "bold",size = 25,margin = margin(r=8)),
          legend.background = element_rect( linetype="solid",colour ="black")
    )
}

P = ggplot(plotdata, aes(developmental_stages,  Value*100, color = Process)) + geom_point(alpha = 0.5, size= 3) + geom_smooth(method = "lm",se=T,size=3) +
  guides(fill=guide_legend(title=NULL)) + #去除图例背景
  scale_color_manual(values = c("#6495ED", "#FFA500", "#FF4500"))+ 
  facet_wrap(.~ Process, 'free')+
  stat_poly_eq(aes(label = paste( stat(adj.rr.label), stat(p.value.label),sep = '~~~~')), formula = y ~ x,  #添加回归方程和调整R方
               size = 6,
               label.x = 0.2,  #位置 ，0-1之间的比例
               label.y = "top", parse = T, color = 'black') + 
  theme_zg()+ 
  theme(axis.text.x=element_text(colour="black",family="Times",size=25), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为25
        axis.text.y=element_text(family="Times",size=25,face="plain",colour="black"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
        strip.text.x = element_text(size=25), strip.text.y = element_text(size=25),
        axis.title.y=element_text(family="Times",size = 25,face="plain"), #设置y轴标题的字体属性
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
        legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
        ),legend.position= c(1,0),legend.justification = c(1,0),legend.background = element_blank(),legend.key.size = unit(12, "pt"),
        # legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
        #  size=18),
        panel.grid.major = element_blank(),   #不显示网格线
        panel.grid.minor = element_blank())+  #不显示网格线
  ylab("Process importance")+xlab("developmental_stages") #设置x轴和y轴的标题
P
jpeg(file = "C:/Users/liujiale/Desktop/Pd.jpg",width =3000,height = 2000,units = "px",res =300)#结果保存
print(P)
dev.off()
