# Title     : Supermarkets Clustering using K-prototypes
# Objective : Recognize user profile in some supermarkets clientele
# Created by: Emilio Corvino
# Created on: 02/05/2020

library("Rtsne")
library("clustMixType")
library("dplyr")
library("readr")
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

# Calculate optimal number of clusters
min_clusters <- 2
max_clusters <- 24
validation <- silhouette_kproto(data = frame, k = min_clusters:max_clusters)

# Perform the algorithm with the optimal number of clusters
start_time <- Sys.time()
kpres <- kproto(x = frame, k = validation$k_opt)
end_time <- Sys.time() # Ending time
time_elapsed <- (end_time-start_time)
cat("\n")
time_elapsed

# Display results and plot them
cat("\n Displaying summary for k-prototypes clustering:\n")
summary(kpres)

cat("\n Displaying plots for k-prototypes clustering:\n")
clprofiles(kpres, frame)
