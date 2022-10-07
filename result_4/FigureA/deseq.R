#title: "Result2,Deseq2"
#author: "Xing Wang"
#date: "2022/4/26"
#output: html_document

rm(list = ls())
getwd()

#未抽平或标准化数据
mycounts<-read.csv("seed.csv",row.names = 1)
dim(mycounts)
mycount <- mycounts[rowMeans(mycounts)>1,] #去除低丰度
dim(mycount)
#字符串转因子
mymeta<-read.csv("seed_meta.csv",stringsAsFactors = T) 
#检验分组数据是否和数据分组名一致
colnames(mycount) == mymeta$id


library(DESeq2)
dds <- DESeqDataSetFromMatrix(countData=mycount, 
                              colData=mymeta, 
                              design=~dex)
dds <- DESeq(dds)
res <- results(dds,contrast = c("dex","Xs","CK"))#后面的是对照
head(res)
class(res)
res_1<-data.frame(res)
class(res_1)
head(res_1)
write.csv(mycount,file = "seed_count.csv") #保留筛选后的count矩阵
write.csv(res_1,file = "seed_diffmycount.csv") #保留差异基因矩阵



#做带标签的火山图
df<-read.csv("seed_diffmycount.csv",header=T,stringsAsFactors = F)
head(df)
dim(df)
df$group<-ifelse(df$log2FoldChange>=2&df$pvalue<=0.05,"Enriched",
                 ifelse(df$log2FoldChange<=-2&df$pvalue<=0.05,
                        "Depleted","Not sig"))
table(df$group)

write.csv(df,file="seed_diff_summary.csv",quote = F)

library(ggplot2)
#install.packages("ggrepel")
library(ggrepel)
 ggplot(df,aes(x=log2FoldChange,y=-log10(pvalue)))+
   geom_point(aes(color=group),size=20)+
   scale_color_manual(values=c("#1E90FF","#FF6347","gray"))
df$pvalue_log10<-(-log10(df$pvalue))
df1<-df[df$pvalue_log10>=2,]
dim(df1)

p= ggplot(df,aes(x=log2FoldChange,y=-log10(pvalue)))+
   geom_point(aes(color=group),size=4,alpha=0.8)+
   scale_color_manual(values=c("#1E90FF","#FF6347","gray"))+
   geom_label_repel(data=df1,aes(x=log2FoldChange,y=-log10(pvalue),
                                label=Features,fosme=1000))+
   theme_bw()  + theme(legend.position = "none")+
  
   labs(y="-log10(pvalue)",x="log(Fold Change)",title="Seed depleted:1,enriched:0")

p
ggsave(paste("seed.pdf", sep=""), p, width = 4.5, height = 5)
ggsave(paste("seed.png", sep=""), p, width = 4.5, height = 5)