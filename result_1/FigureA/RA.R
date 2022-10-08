# Result1, Figuer A, microbiomes compsition
# Author: Xing Wang
# Data: October 8th 2022
getwd()
rm(list=ls()) # clean enviroment object
library("reshape2", quietly=T, warn.conflicts=F)
library(ggalluvial)

# Set ggplot2 drawing parameter, such as axis line and text size, lengend and title size, and so on.
main_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=.5, colour="black"),
                   axis.line.y=element_line(size=.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=7),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=7),
                   text=element_text(family="sans", size=10))

# Public file 1. "design.txt"  Design of experiment
design = read.table("design.txt", header=T, row.names= 1, sep="\t")

# Public file 2. "otu_table.txt"  raw reads count of each OTU in each sample
otu_table = read.delim("otu.txt", row.names= 1,  header=T, sep="\t")

# Public file 3. "rep_seqs_tax.txt"  taxonomy for each OTU, tab seperated
taxonomy = read.delim("tax.txt", row.names= 1,header=F, sep="\t")
colnames(taxonomy) = c("kingdom","phylum","class","order","family","genus")

# select p__Proteobacteria line
idx = taxonomy$phylum == "p__Proteobacteria"
taxonomy$full=as.character(taxonomy$phylum)
taxonomy[idx,]$full=as.character(taxonomy[idx,]$class)
tax_count = merge(taxonomy, otu_table, by="row.names")

tax_count_sum = aggregate(tax_count[,-(1:10)], by=tax_count[10], FUN=sum) # mean
rownames(tax_count_sum) = tax_count_sum$full
tax_count_sum = tax_count_sum[,-1]
per = t(t(tax_count_sum)/colSums(tax_count_sum,na=T)) * 100 # normalization to total 100

mean_sort = per[(order(-rowSums(per))), ] # decrease sort
colSums(mean_sort)

mean_sort=as.data.frame(mean_sort)
other = colSums(mean_sort[12:dim(mean_sort)[1], ])
mean_sort = mean_sort[1:(12-1), ]
mean_sort = rbind(mean_sort,other)
rownames(mean_sort)[12] = c("Low Abundance")


write.table(mean_sort, file="RA.txt", append = F, sep="\t", quote=F, row.names=T, col.names=T)
topN=rownames(mean_sort)

rm(list=ls()) 
sub_merge <- read.table('RA_upload.txt', row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
library(ggplot2)
sub_merge$Period<-factor(sub_merge$Period,levels=c("Seedling", "Tillering", "Booting", "Maturity"), 
                         labels = c("Seedling", "Tillering", "Booting", "Maturity"))
sub_merge$Phylum<-factor(sub_merge$Phylum,levels=c("Gammaproteobacteria",
                                                   "Betaproteobacteria",
                                                   "Actinobacteria",
                                                   "Alphaproteobacteria",
                                                   "Deltaproteobacteria",
                                                   "Gemmatimonadetes",
                                                   "Firmicutes",
                                                    "Bacteroidetes",
                                                    "Acidobacteria",
                                                    "Ignavibacteriae",
                                                    "Nitrospirae",
                                                    "Low_Abundance"))
phy.cols <- c("#FFFF00", "#FF8247", 
              "#FFE7BA", "#87CEFA",
              "#B0E0E6", "#48D1CC",
              "#5F9EA0", "#66CDAA",
              "#458B00", "#BCEE68",
              "#FFF68F", "#EEEE00") 
p=ggplot(sub_merge, aes(x = TreatmentID, y = RA, fill=Phylum)) +
  geom_bar(stat='identity', position = "fill")+  
  labs(x="Treatment",y="Relative abundance")+
  facet_grid(Comparts~Period+Treatment,scales= "free" ,space= "free")+
  theme_bw()+
  scale_fill_manual(values=phy.cols ) 
p=p+theme(axis.text.x = element_blank())
p
ggsave("RA-20221006.pdf", p, width = 10, height = 8)
ggsave("RA-20221006.png", p, width = 10, height = 8)
