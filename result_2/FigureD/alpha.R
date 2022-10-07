#Result1,Figuer D,alpha_diversity
#Author: Xing Wang

getwd()
rm(list = ls())

library(tidyverse)
df<-read.csv("stem.csv")
head(df)

library(ggplot2)

df$Stem_endosphere <- factor(df$Stem_endosphere, levels=c('CK', 'Xs'))

p= ggplot(data=df,aes(x=factor(Development.stage,level = c("Seedling","Tillering","Booting","Maturity")), y=Shannon, fill=Stem_endosphere))+
  stat_boxplot(geom = "errorbar",
               width=0.3,
               position = position_dodge(0.9))+
  geom_boxplot(position = position_dodge(0.9),alpha=0.7)+
  
  geom_jitter(position=position_jitterdodge(0.01),size=2,shape=21,alpha=0.7)+
  theme_bw()+
  
  theme(legend.position = "top")+
  labs(x="Development.stage", y="Shannon index")+
  
  scale_fill_manual(values = c("#FF1493","#1E90FF"))

p
ggsave(paste("stem.pdf", sep=""), p, width = 3, height = 4)

p
ggsave(paste("stem.png", sep=""), p, width = 3, height = 4)

# 统计组间是否显著差异
# anova对指数与分组统计
Shannon_stats <- aov(Shannon ~ Anova, data = df)
summary(Shannon_stats)
# 使用TukeyHSD对组间进行检验，效正pvalue
Tukey_HSD_Shannon <- TukeyHSD(Shannon_stats, ordered = FALSE, conf.level = 0.95)
# 结果中提取需要的结果
Tukey_HSD_Shannon_table <- as.data.frame(Tukey_HSD_Shannon$Anova)
# 预览结果
Tukey_HSD_Shannon_table
# 保存结果到文件，按Pvaule值由小到大排序
write.table(Tukey_HSD_Shannon_table[order(Tukey_HSD_Shannon_table$p, decreasing=FALSE), ], file="stem_stat.txt",append = FALSE, quote = FALSE, sep="\t",eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE)


