# Title     : Supermarkets Clustering using K-prototypes
# Objective : Using silhouette method find the optimal number of clusters
# Created by: Emilio Corvino
# Created on: 02/05/2020

library("Rtsne")
library("ggplot2")
library("ggplotgui")
library("readxl")
library("clustMixType")
library("ISLR")
library("dplyr")
library("readr")

set.seed(99)
dataset <- read_csv("supermarket_clean.csv")
df <- as.data.frame(dataset)

# Invoices IDs
ids <- df %>% select(Invoice_ID)

# Dataset without IDs and with categoricals variables factorized
df_clean <- df %>%
  mutate(City = factor(City),
         Customer_type = factor(Customer_type),
         Gender = factor(Gender),
         Product_line = factor(Product_line)
  ) %>% select(City, Customer_type, Gender, Product_line, Total, Rating)

frame <- as.data.frame(df_clean)
min_clusters <- 2
max_clusters <- 24

# Calculate optimal number of clusters
validation <- silhouette_kproto(data = frame, k = min_clusters:max_clusters)

# Plot silhouette width (higher is better)
plot(min_clusters:max_clusters, validation$indices, xlab = "Number of clusters", ylab = "Silhouette width", type = "b")
