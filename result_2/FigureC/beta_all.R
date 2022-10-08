#Result2, Figuer C,beta_diversity_all
#Author: Xing Wang
# Data: October 8th 2022

getwd()
rm(list = ls())

library(vegan)
otu_table <- read.table("pcoa.txt",sep="\t", row.names=1, header=T)
meta_tab <- read.table("pcoa_meta.txt", header=T, row.names=1, check.names=F)
otu_table <- data.frame(t(otu_table))

dune_dist <- vegdist(otu_table, method="bray", binary=F)
dune_pcoa <- cmdscale(dune_dist, k = (nrow(otu_table) - 1), eig = TRUE)

dune_pcoa_points <- as.data.frame(dune_pcoa$points)
sum_eig <- sum(dune_pcoa$eig)
eig_percent <- round(dune_pcoa$eig/sum_eig*100,1)
colnames(dune_pcoa_points) <- paste0("PCoA", 1:2)
dune_pcoa_result <- cbind(dune_pcoa_points, meta_tab)
head(dune_pcoa_result)

library(ggplot2)

ggplot(dune_pcoa_result, aes(x=PCoA1, y=PCoA2, color=SampleType)) +
  labs(x=paste("PCoA 1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA 2 (", eig_percent[2], "%)", sep="")) +
  geom_point(size=4
  ) +
  theme_classic()


set.seed(1)
dune.div <- adonis2(otu_table ~ SampleType, data = meta_tab, permutations = 999, method="bray")

dune.div

dune_adonis <- paste0("R-squared: ",round(dune.div$R2,2), "; P-value: ", dune.div$`Pr(>F)`)

p= ggplot(dune_pcoa_result, aes(x=PCoA1, y=PCoA2, color=SampleType, group = SampleType)) +
  labs(x=paste("PCoA 1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA 2 (", eig_percent[2], "%)", sep=""),
       title=dune_adonis) +
  geom_point(size=2,alpha=0.8) +scale_color_manual(values=c("Bulk_CK" = "#7A378B", "Bulk_Xs" = "#4682B4",
                               "Rhizosphere_CK" = "#FF1493", "Rhizosphere_Xs" = "#00FF7F",
                               "Root_CK" = "#AB82FF", "Root_Xs" = "#7FFF00",
                               "Stem_CK" = "#E066FF", "Stem_Xs" = "#FFFF00",
                               "Seed_CK" = "#FFC1C1", "Seed_Xs" = "#C0FF3E"))+
  theme_bw() +
  theme(panel.grid=element_blank()) + theme(legend.position="right")+theme(legend.key.height = unit(5, "pt"),legend.key.width = unit(5, "pt"))
  

p
ggsave(paste("pcoa.pdf", sep=""), p, width = 4, height = 3.5)
ggsave(paste("pcoa.png", sep=""), p, width = 4, height = 3.5)

