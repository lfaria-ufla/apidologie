library(corrplot)
library(ggpubr)
library(RColorBrewer)

names(dados)
dados <- read.csv("./nodes.apidologie.csv",sep = ";", head = T)

str(dados)

co = dados%>%
  filter(ordem != "P")%>%
  dplyr::select(degree, betweenness, closeness, c, z, core, eig)%>%
  rename( "Degree" = "degree","Betweenness" = "betweenness" , 
          "Closeness" = "closeness", 
          "Core/Periphery" = "core", 
          "Eigenvector" = "eig")%>%
  drop_na()

m = cor(co, method = "spearman")
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
################################################

co = dados%>%
  filter(ordem != "P")%>%
  dplyr::select(degree, betweenness, closeness, c, z, core, eig)%>%
  rename( "Degree" = "degree","Betweenness" = "betweenness" , 
          "Closeness" = "closeness", 
          "Core/Periphery" = "core", 
          "Eigenvector" = "eig")%>%
  drop_na()

mt = cor(co, method = "spearman")
# matrix of the p-value of the correlation
p.mat <- cor.mtest(mt)

library(ggcorrplot)
cor = ggcorrplot(mt, hc.order = TRUE, type = "lower",
           lab = TRUE, colors = c("gold","red", "cornflowerblue"))

cor

library(ggpubr)
tiff("figure_cor_pca.tiff", units="in", width=8, height=8, res=300)
ggarrange(pca_a, 
          ncol = 3, nrow = 3, common.legend = F)
dev.off()



