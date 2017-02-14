library(readxl)
library(dplyr)
library(psych)
library(MVN)
library(ggplot2)

setwd("C:/biology")

dataset <- read_excel("bio-matrix.xls", 1)

names(dataset) <- make.names(names(dataset))
attach(dataset)

num_data <- select(dataset, -species, -class, -hair.on.the.lemma.strips.or.not)

descr_stat <- round(describe(num_data), 2) # psych

source("correlations.R")
cor_list <- correlations(num_data)

qplot(data = num_data,
      length.of.the.anthecium,
      length.of.the.callus) + stat_smooth(method = "lm")

frequency_table <- count_(dataset, "species", sort = T)

uniNorm(num_data, type = "Lillie", desc = FALSE) # MVN

source("make_histogram.R")
make_histogram(dataset, 3)
