# Result2, Figuer A, beta_diversity
# Author: Xing Wang
# Data: October 8th 2022

#getwd()
#rm(list = ls())

library(vegan)
otu_table <- read.table("sc.txt",sep="\t", row.names=1, header=T)
meta_tab <- read.table("sc_meta.txt", header=T, row.names=1, check.names=F)
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

ggplot(dune_pcoa_result, aes(x=PCoA1, y=PCoA2, color=Stem_Maturity)) +
  labs(x=paste("PCoA 1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA 2 (", eig_percent[2], "%)", sep="")) +
  geom_point(size=4
  ) + stat_ellipse(level=0.6) +
  theme_classic()


# install.packages("ggalt")
library(ggalt)
ggplot(dune_pcoa_result, aes(x=PCoA1, y=PCoA2, color=Stem_Maturity, group = Stem_Maturity)) +
  labs(x=paste("PCoA 1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA 2 (", eig_percent[2], "%)", sep="")) +
  geom_point(size=5) + 
  geom_encircle(aes(fill=Stem_Maturity), alpha = 0.1, show.legend = F) +
  theme_classic() + coord_fixed(1)


set.seed(1)
dune.div <- adonis2(otu_table ~ Stem_Maturity, data = meta_tab, permutations = 999, method="bray")

dune.div


dune_adonis <- paste0("R-squared: ",round(dune.div$R2,2), "; P-value: ", dune.div$`Pr(>F)`)

# install.packages("ggalt")

library(ggalt)
p= ggplot(dune_pcoa_result, aes(x=PCoA1, y=PCoA2, color=Stem_Maturity, group = Stem_Maturity)) +
   labs(x=paste("PCoA 1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA 2 (", eig_percent[2], "%)", sep=""),
       title=dune_adonis) +
   geom_point(size=2,alpha=0.8) + scale_fill_manual(values=c("CK" = "#FF1493", "Xs" = "#1E90FF"))+
   geom_encircle(aes(fill=Stem_Maturity), alpha = 0.2, expand=0.05,show.legend = F,linetype=3) +
   theme_bw() +theme(panel.grid=element_blank()) + theme(legend.position="bottom")


p
ggsave(paste("sc.pdf", sep=""), p, width = 3.2, height = 3)
ggsave(paste("sc.png", sep=""), p, width = 3.2, height = 3)

