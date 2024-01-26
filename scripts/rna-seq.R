
# Download the data provided by your collaborator
# using a for loop to automate this step
for(i in c("counts_raw.csv",
           "counts_transformed.csv",
           "sample_info.csv",
           "test_result.csv")){
  download.file(
    url = paste0("https://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data_raw/", i, "?raw=true"),
    destfile = paste0("data_raw/", i)
  )
}

# Import libraries
library(tidyverse)

# Import data
raw_cts <- read_csv("data_raw/counts_raw.csv")
trans_cts <- read_csv("data_raw/counts_transformed.csv")
sample_info <- read_csv("data_raw/sample_info.csv")
test_results <- read_csv("data_raw/test_result.csv")

# Pivot trans_cts into long format
trans_cts_long <- trans_cts %>%
  pivot_longer(
    names_to = "sample",
    values_to = "cts",
    cols = wt_0_r1:mut_180_r3
  )

# Join trans_cts_long with samples_info
trans_cts_long <- full_join(trans_cts_long, sample_info, by = "sample")

# Plot histograms as frequency polygon
trans_cts_long %>%
  ggplot(aes(x = cts)) +
  geom_freqpoly()

trans_cts_long %>%
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(
    rows = vars(strain),
    cols = vars(minute)
  )


# Challenge: produce similar plot for raw counts
# convert raw_cts to long format
raw_cts_long <- raw_cts %>%
  pivot_longer(
    names_to = "sample",
    values_to = "cts",
    cols = wt_0_r1:mut_180_r3
  ) %>%
  full_join(
    sample_info,
    by = "sample"
  )

raw_cts_long %>%
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly() +
  facet_grid(
    rows = vars(strain),
    cols = vars(minute)
  ) +
  scale_x_log10()

raw_cts_long %>%
  ggplot(
    aes(
      x = log10(cts),
      color = replicate
    )) +
  geom_freqpoly() +
  facet_grid(
    rows = vars(strain),
    cols = vars(minute)
  )

log10(0)
log10(NA)

raw_cts_long %>%
  ggplot(
    aes(
      x = log10(cts + 1),
      color = replicate
    )) +
  geom_freqpoly() +
  facet_grid(
    rows = vars(strain),
    cols = vars(minute)
  )

raw_cts_long %>%
  ggplot(
    aes(
      x = log10(cts + 1),
      color = replicate
    )) +
  geom_freqpoly(binwidth = 0.25) +
  facet_grid(
    rows = vars(strain),
    cols = vars(minute)
  )

# Make a boxplot
raw_cts_long %>%
  ggplot(aes(
    x = factor(minute),
    y = log(cts + 1),
    fill = strain
  )) +
  geom_boxplot() +
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_grid(
    cols = vars(replicate),
    labeller = labeller(replicate = Hmisc::capitalize)
  ) +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")



# Correlations

# Across time points
trans_cts %>%
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) +
  geom_point() +
  geom_abline(color = "brown")

# Across replicates
trans_cts %>%
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point() +
  geom_abline(color = "brown")

# Correlation across all samples in a experiment

trans_cts_corr <- trans_cts %>%
  select(-gene) %>%
  cor(method = "spearman")

library(corrr)
rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1
  ))


summary(raw_cts_long$cts)
summary(trans_cts_long$cts)


raw_cts %>%
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point()

raw_cts %>%
  ggplot(aes(x = wt_0_r1 + 1, y = wt_0_r2 + 1)) +
  geom_point() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

# Mean-variance plot
raw_cts_long %>%
  group_by(gene) %>%
  summarize(mean_cts = mean(cts),
            var_cts = var(cts)) %>%
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(color = "brown") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")
# The plot shows that there is no linear relationship between log-mean and log-variance.


trans_cts_long %>%
  group_by(gene) %>%
  summarize(mean_cts = mean(cts),
            var_cts = var(cts)) %>%
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point()
# Transformed data show that variance does not depend anymore on mean value, which hint to biological effects.

trans_cts_long %>%
  group_by(gene) %>%
  summarize(mean_cts = mean(cts),
            var_cts = var(cts)) %>%
  mutate(above_four = var_cts > 4) %>%
  ggplot(aes(x = mean_cts, y = var_cts, color = above_four)) +
  geom_point()

# Principal Component Analysis
pca_matrix <-trans_cts %>%
  column_to_rownames("gene") %>%
  as.matrix() %>%
  t()

sample_pca <- prcomp(pca_matrix)
summary(sample_pca)

# Matrix slicing
pca_matrix[1:10, 1:5]

# Matrix to Tibble
as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames = "gene")

# Eigenvalues
pc_eigenvalues <- sample_pca$sdev ^ 2

pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),
                         variance = pc_eigenvalues) %>%
  mutate(pct = variance / sum(variance) * 100,
         pct_cum = cumsum(pct))

# Pareto plot (!!)
pc_eigenvalues %>%
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum)) +
  labs(x = "Principal componenet", y = "Fraction of variance explained")

pc_scores <- sample_pca$x %>%
  as_tibble(rownames = "sample")

pc_scores %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

pca_plot <- pc_scores %>%
  full_join(sample_info, by = "sample") %>%
  ggplot(aes(x = PC1, y = PC2, color = factor(minute), shape = strain)) +
  geom_point()

pc_loadings <- sample_pca$rotation %>%
  as_tibble(rownames = "gene")

top10 <- pc_loadings %>%
  select(gene, PC1, PC2) %>%
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>%
  group_by(PC) %>%
  arrange(desc(abs(loading))) %>%
  slice(1:10) %>%
  pull(gene) %>%
  unique()

top_loadings <- pc_loadings %>%
  filter(gene %in% top10)

loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               color = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

install.packages("patchwork")

library(patchwork)

pca_plot | loadings_plot
(pca_plot + pca_plot + pca_plot) / loadings_plot +
  plot_annotation(tag_levels = "A")

library(ggfortify)

autoplot(sample_pca)

pca_plot <- autoplot(sample_pca,
         data = sample_info %>% mutate(minute = as.factor(minute)),
         colour = "minute",
         shape = "strain")

library(broom)

tidy(sample_pca, matrix = "eigenvalues")

tidy(sample_pca, matrix = "loadings")



# Differential expression results
test_results <- read_csv("data_raw/test_result.csv")
test_results
# gene column -> gene name
# baseMean column -> normalized expression level of a gene
# log2FoldChange column -> amount of change between 2 conditions
# lfcSE column -> standard error associated to log2FoldChange value
# stat column -> statistics value computed as log2FoldChange / lfcSE compared to standard normal distribution
# pvalue -> p-value associated with the change
# padj -> p-value corrected for multiple hypothesis testing
# comparison -> comparison group

# MA plot
# Challenge: generate a MA plot (baseMean vs log2FoldChange) organize panels by comparison (time point). Hint: consider log-transform baseMean
test_results %>%
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_wrap(facets = vars(comparison))

ma_plot <- test_results %>%
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>%
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), color = "tomato", size = 1) +
  geom_hline(yintercept = 0, color = "dodgerblue") +
  facet_wrap(facets = vars(comparison))


(ma_plot | pca_plot)


# Visualizing expression trends
# 1. Get candidate gene (aka padj < 0.01)
candidate_genes <- test_results %>%
  filter(padj < 0.01) %>%
  pull(gene) %>% # test_results[,"gene"] aka test_results$gene
  unique()

# 1a. Get the trans_cts table in long format
trans_cts_long <- trans_cts %>%
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample", values_to = "cts") %>%
  full_join(sample_info, by = "sample")

# 2. Filter trans_cts_long for candidate genes and compute mean expression value for each gene in each tp and each genotype
trans_cts_mean <- trans_cts_long %>%
  filter(gene %in% candidate_genes) %>%
  group_by(gene, strain, minute) %>%
  summarize(mean_cts = mean(cts), nrep = n()) %>%
  ungroup()

# 3. Plot trends
trans_cts_mean %>%
  ggplot(aes(x = minute, y = mean_cts)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

# Scaling data to improve visualization
trans_cts_mean <- trans_cts_long %>%
  filter(gene %in% candidate_genes) %>%
  group_by(gene) %>%
  mutate(cts_scaled  = (cts - mean(cts)) / sd(cts)) %>%
  group_by(gene, strain, minute) %>%
  summarize(mean_cts_scaled = mean(cts_scaled),
            nrep = n()) %>%
  ungroup()

trans_cts_mean %>%
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_hline(yintercept = 0, color = "brown", linetype = "dashed") +
  facet_grid(rows = vars(strain)) +
  scale_x_continuous(breaks = unique(trans_cts_mean$minute))

# Clustering
trans_cts <- read_csv("data_raw/counts_transformed.csv")

# 1. Create a matrix of counts
hclust_matrix <- trans_cts %>%
  select(-gene) %>%
  as.matrix()
rownames(hclust_matrix) <- trans_cts$gene
hclust_matrix <- hclust_matrix[candidate_genes,]

hclust_matrix <- hclust_matrix %>%
  t() %>%
  scale() %>%
  t()
