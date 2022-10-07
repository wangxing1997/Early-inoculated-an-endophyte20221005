#Result1,Figuer C,alpha_diversity_compartment
#Author: Xing Wang

getwd()
rm(list = ls())

library(tidyverse)
df<-read.csv("compartment.csv")
head(df)

library(ggplot2)

df$SampleID <- factor(df$SampleID, levels=c('ck', 'xs'))

p= ggplot(data=df,aes(x=factor(Compartments,level = c("ck_bu","Xs_bu","ck_rhi","Xs_rhi","ck_ro","Xs_ro","ck_st","Xs_st","ck_se","Xs_se")), y=Shannon, color=Compartments))+
  stat_boxplot(geom = "errorbar",
               width=0.25,
               position = position_dodge(0.9))+
  geom_boxplot(position = position_dodge(0.9))+
  
  geom_jitter(position=position_jitterdodge(0.01),size=2,alpha=0.7,shape=23)+
  theme_bw()+
  theme(legend.position = "NA")+theme(panel.grid=element_blank())+
  
  guides(fill=guide_legend(title=NULL))+
  labs(x="Compartments", y="Shannon index")+
  
  scale_color_manual(aesthetics="color",values = c("#845EC2","#B39CD0","#0089BA","#C34A36","#00C9A7","#845EC2","#B39CD0","#0089BA","#C34A36","#00C9A7"))

p
ggsave(paste("compartment.pdf", sep=""), p, width = 4, height = 4)

p
ggsave(paste("compartment.png", sep=""), p, width = 4, height = 4)


# 统计组间是否显著差异
# anova对指数与分组统计
Shannon_stats <- aov(Shannon ~ Compartments, data = df)
# 使用TukeyHSD对组间进行检验，效正pvalue
Tukey_HSD_Shannon <- TukeyHSD(Shannon_stats, ordered = FALSE, conf.level = 0.95)
# 结果中提取需要的结果
Tukey_HSD_Shannon_table <- as.data.frame(Tukey_HSD_Shannon$Compartments)
# 预览结果
Tukey_HSD_Shannon_table
# 保存结果到文件，按Pvaule值由小到大排序
write.table(Tukey_HSD_Shannon_table[order(Tukey_HSD_Shannon_table$p, decreasing=FALSE), ], file="compartments_stat.txt",append = FALSE, quote = FALSE, sep="\t",eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE)


