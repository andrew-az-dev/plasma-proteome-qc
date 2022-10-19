# contaminants-qc-script.
# This script implements a QC method to avoid sample-related biases in biomarker studies described in https://www.embopress.org/doi/full/10.15252/emmm.201910427 
# You need a data file to be QC'ed that contains gene names - example included with this script
# The "Quality-marker-panel-for..." files contain protein IDs used to calculate contamination ratios, also included with this script.   
library(ggplot2)

#####################################
#####################################
# Substitute the file name depending on the marker panel to be used
# The following marker panels are available: Quality-marker-panel-for-coagulation-panel-plasma.txt; Quality-marker-panel-for-coagulation-panel-serum.txt; Quality-marker-panel-for-erythrocyte-contaminations.txt; Quality-marker-panel-for-platelet-contaminations.txt
con <- read.table("./contaminant.panels/Quality-marker-panel-for-erythrocyte-contaminations.txt" , stringsAsFactors = FALSE, header = TRUE, sep = "\t", fill =  TRUE, comment.char="?", check.names=T, quote = "")  
# read in example data file
load("./data/example.data.RData")
# 
tmp <- merge(con, dat, by.x = "Gene.names" , by.y = "gene") 
# nrow(dat)

con.ratio <- colSums(tmp[, 9:ncol(tmp)], na.rm = T) / colSums( (dat[ , 1:ncol(dat)-1]), na.rm = T)
con.ratio <- data.frame( Sample = names(con.ratio), Contamination.Ratio = con.ratio)


# list samples that might be contaminated
con.ratio$Sample[ which(con.ratio$Contamination.Ratio > mean(con.ratio$Contamination.Ratio) + 3*sd(con.ratio$Contamination.Ratio))]
# 
# TO DO: implement a statistical test for outlier detection. Right now an arbitrary cut-off of 3xsd is used 


# visualize results
p<-ggplot(con.ratio, aes(x=Sample, y=Contamination.Ratio)) +
  geom_bar(stat="identity")+
  geom_hline(yintercept=mean(con.ratio$Contamination.Ratio) + 3*sd(con.ratio$Contamination.Ratio),
             linetype="dashed", 
             color = "blue", size=1) +
  ggtitle("Erythrocyte contaminations") + 
  ylim(0, mean(con.ratio$Contamination.Ratio) + 5*sd(con.ratio$Contamination.Ratio) ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
