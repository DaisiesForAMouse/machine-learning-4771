library(ggplot2)

pca20 <- read.csv('./pca20.txt')

ggplot(pca20, aes(x=training_size,y=success_ratio,color=model)) + ylim(0, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca20.png")

pca20_knn_vs_prob <- pca20[pca20[1] == 'prob' |
                           pca20[1] == 'knn|k=1|norm=2' |
                           pca20[1] == 'knn|k=5|norm=2' |
                           pca20[1] == 'knn|k=10|norm=2',]

ggplot(pca20_knn_vs_prob, aes(x=training_size,y=success_ratio,color=model)) + ylim(0, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca20_knn_vs_prob.png")

pca20_1nn_norms <- pca20[pca20[1] == 'knn|k=1|norm=-1' |
                         pca20[1] == 'knn|k=1|norm=1' |
                         pca20[1] == 'knn|k=1|norm=2',]

ggplot(pca20_1nn_norms, aes(x=training_size,y=success_ratio,color=model)) + ylim(0.7, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca20_1nn_norms.png")

pca20_5nn_norms <- pca20[pca20[1] == 'knn|k=5|norm=-1' |
                         pca20[1] == 'knn|k=5|norm=1' |
                         pca20[1] == 'knn|k=5|norm=2',]

ggplot(pca20_5nn_norms, aes(x=training_size,y=success_ratio,color=model)) + ylim(0.7, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca20_5nn_norms.png")

pca20_10nn_norms <- pca20[pca20[1] == 'knn|k=10|norm=-1' |
                          pca20[1] == 'knn|k=10|norm=1' |
                          pca20[1] == 'knn|k=10|norm=2',]

ggplot(pca20_10nn_norms, aes(x=training_size,y=success_ratio,color=model)) + ylim(0.7, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca20_10nn_norms.png")

pca20_knn_norms <- pca20[pca20[1] != 'prob', ]

ggplot(pca20_knn_norms, aes(x=training_size,y=success_ratio,color=model)) + ylim(0.7,1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca20_knn_norms.png")

pca40 <- read.csv('./pca40.txt')

ggplot(pca40, aes(x=training_size,y=success_ratio,color=model)) + ylim(0, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca40.png")

ggsave("pca40_knn_vs_prob.png")

pca40_knn_vs_prob <- pca40[pca40[1] == 'prob' |
                           pca40[1] == 'knn|k=1|norm=2' |
                           pca40[1] == 'knn|k=5|norm=2' |
                           pca40[1] == 'knn|k=10|norm=2',]

ggplot(pca40_knn_vs_prob, aes(x=training_size,y=success_ratio,color=model)) + ylim(0, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca40_knn_vs_prob.png")

pca40_1nn_norms <- pca40[pca40[1] == 'knn|k=1|norm=-1' |
                         pca40[1] == 'knn|k=1|norm=1' |
                         pca40[1] == 'knn|k=1|norm=2',]

ggplot(pca40_1nn_norms, aes(x=training_size,y=success_ratio,color=model)) + ylim(0.7, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca40_1nn_norms.png")

pca40_5nn_norms <- pca40[pca40[1] == 'knn|k=5|norm=-1' |
                         pca40[1] == 'knn|k=5|norm=1' |
                         pca40[1] == 'knn|k=5|norm=2',]

ggplot(pca40_5nn_norms, aes(x=training_size,y=success_ratio,color=model)) + ylim(0.7, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca40_5nn_norms.png")

pca40_10nn_norms <- pca40[pca40[1] == 'knn|k=10|norm=-1' |
                          pca40[1] == 'knn|k=10|norm=1' |
                          pca40[1] == 'knn|k=10|norm=2',]

ggplot(pca40_10nn_norms, aes(x=training_size,y=success_ratio,color=model)) + ylim(0.7, 1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca40_10nn_norms.png")

pca40_knn_norms <- pca40[pca40[1] != 'prob', ]

ggplot(pca40_knn_norms, aes(x=training_size,y=success_ratio,color=model)) + ylim(0.7,1) + geom_point() + stat_summary(fun="mean", geom="line")
ggsave("pca40_knn_norms.png")