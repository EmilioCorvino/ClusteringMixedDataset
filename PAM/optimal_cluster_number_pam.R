# Title     : Supermarkets Clustering using partitioning around medoids
# Objective : Using silhouette method find the optimal number of clusters
# Created by: Emilio Corvino
# Created on: 02/05/2020

library("dplyr")
library("readr")
library("cluster")
library("ggplot2")

set.seed(42)
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
  ) %>%
  select(City, Customer_type, Gender, Product_line, Total, Rating)
frame <- as.data.frame(df_clean)

# Calculate gower distance
gower_dist <- daisy(frame, metric = "gower")
summary(gower_dist)

# Calculate silhouette width for many k using PAM
min_clusters <- 2
max_clusters <- 24
silhouette_width <- c()
for (num_clusters in min_clusters:max_clusters) {
  pam_fit <- pam(gower_dist, diss = TRUE, k = num_clusters)
  silhouette_width[num_clusters] <- pam_fit$silinfo$avg.width
}

# Plot silhouette width (higher is better)
plot(1:max_clusters, silhouette_width, xlab = "Number of clusters", ylab = "Silhouette width", type = "b")
