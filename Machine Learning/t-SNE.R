library(ggplot2)
library(Rtsne)

h=sample(1:nrow(db))[1:10000]
features<-db[h,-ncol(db)]

tsne <- Rtsne(as.matrix(features), check_duplicates = FALSE, pca = TRUE, 
              perplexity=30, theta=0.5, dims=2)

embedding <- as.data.frame(tsne$Y)
embedding$class <- as.factor(db[h,ncol(db)])

ggplot(embedding, aes(x=V1, y=V2, color=class)) +
  geom_point(size=1.25) +
  guides(colour = guide_legend(override.aes = list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE 2D Normal") +
  theme_light(base_size=20) +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks       = element_blank(),
        axis.line        = element_blank(),
        panel.border     = element_blank())
