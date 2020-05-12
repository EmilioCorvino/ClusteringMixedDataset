# Title     : Supermarkets Clustering using partitioning around medoids
# Objective : Recognize user profile in some supermarkets clientele
# Created by: Emilio Corvino
# Created on: 02/05/2020

library("Rtsne")
library("dplyr")
library("readr")
library("cluster")
library("ggplot2")
library("fpc")
library("clustMixType")

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
start_time <- Sys.time() # Starting time
gower_dist <- daisy(frame, metric = "gower")
summary(gower_dist)

# Applying partitioning around medoids algorithm. pamk() automatically finds the optimal number of clusters in a range
# using silhouette width. The optimal cluster value is saved in pam_fit$nc.
pam_fit <- pamk(gower_dist, 2:24, diss = TRUE)
end_time <- Sys.time() # Ending time
time_elapsed <- (end_time-start_time)
cat("\n")
time_elapsed
cat("\n\n")

# Summary visualization
cat("Summary of each cluster:\n")
pam_results <- frame %>%
  mutate(Cluster = pam_fit$pamobject$clustering) %>%
  group_by(Cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
cat("\n\n")

# Clusters' medoids
cat("Cluster medoids:\n")
medoids <- frame[pam_fit$pamobject$medoids, ]
medoids
cat("\n\n")

# t-SNE visualization
cat("t-SNE visualization:\n")
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$pamobject$clustering),
         name = df$Invoice_ID)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

# Building object for clprofiles function
pam_profiles <- pam_fit
pam_profiles$cluster <- pam_fit$pamobject$clustering
pam_profiles$size <- pam_fit$pamobject$clusinfo[1:pam_fit$nc, 1]
clprofiles(pam_profiles, frame)
