library(readxl)
library(dplyr)
library(psych)
library(MVN)
library(ggplot2)
library(factoextra)
library(cluster)

setwd("C:/Users/admin/Dropbox (work)/ml/asu/bio/code")

data <- read_excel("matrix.xls", 1)

names(data) <- make.names(names(data))
attach(data)

num_data <- select(data, -species, -class, -hair.on.the.lemma.strips.or.not)


# описательная статистика
descr_stat <- round(describe(num_data), 2) # psych


# проверка нормальности
uniNorm(num_data, type = "Lillie") # MVN

source("make_histogram.R")
make_histogram(data, 3)


# корреляции пирсона
source("correlations.R")
cor_list <- correlations(num_data)


# метод главных компонент
pc <- prcomp(num_data, center = T, scale. = T)
summary(pc) # variance >1

# pc$rotation
# pc$x


# кластерный анализ
frequency_table <- count_(data, "species", sort = T)

cluster_data <- select(data, -species, -class)

dm <- daisy(cluster_data, metric = "gower") # Dissimilarity Matrix Calculation

tree <- hclust(dm, method = "average") # "average" = метод UPGMA
plot(as.dendrogram(tree))
rect.hclust(tree, 5, border="blue")
result <- cutree(tree, 5)
table(data$species, result)

detach(data)
