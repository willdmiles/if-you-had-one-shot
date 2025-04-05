library(dplyr)
library(tidyr)
library(ggplot2)

## set working directory here --->>>
## ---      
      
## load market-year level dataset      
market_level_df <- read_dta("Data/market_year_dataset.dta")
market_level_df <- market_level_df %>% filter(!is.na(atc1))

##----------------------------------------------------------------------------------------
## Figure 3: 
## Binned scatter plots of the market-level relationship between experimenter scale, 
## the diversity of approaches, and success
##----------------------------------------------------------------------------------------

# Set the number of bins
num_bins <- 25

# (a) TARGET DIVERSITY
market_level_df %>%
  mutate(av_scale_bin = ntile(av_scale, num_bins)) %>%
  group_by(av_scale_bin) %>%
  summarize(
    av_scale_bin = mean(av_scale),
    shannon_binned = mean(shannon, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = av_scale_bin, y = shannon_binned)) +
  geom_point(shape = 4, size = 1) +
  geom_smooth(method = 'lm', size = 0.5, fill = 'red', alpha = 0.1, col = 'red4', se = FALSE) +
  theme_classic() +
  labs(
    x = "Average Experimenter Scale",
    y = "Target Diversity"
  ) +
  theme(text = element_text(size = 6))

ggsave("Figures/target_diversity_exp_scale.pdf",
       height = 2, 
       width = 2)


# (b) AVERAGE SUCCESS

market_level_df %>%
  mutate(av_scale_bin = ntile(av_scale, num_bins)) %>%  # Divide av_scale into quantile-based bins
  group_by(av_scale_bin) %>%
  summarize(
    av_scale_bin = mean(av_scale),
    phase1_share_binned = mean(phase1_share, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = av_scale_bin, y = phase1_share_binned)) +
  geom_point(shape = 4, size = 1) +
  geom_smooth(method = 'lm', size = 0.5, alpha = 0.1, col = 'red4', se = FALSE) +
  theme_classic() +
  labs(
    x = "Average Experimenter Scale",
    y =  "Share to Phase 1"
  ) +
  theme(text = element_text(size = 8)) 

ggsave("Figures/scale_phase1_share.pdf",
       height = 2, 
       width = 2)

# (c) AT LEAST ONE SUCCESS

market_level_df %>%
  mutate(shannon_bin = ntile(shannon, num_bins)) %>%  # Divide av_scale into quantile-based bins
  group_by(shannon_bin) %>%
  summarize(
    shannon_bin = mean(shannon),
    atleastone_binned = mean(atleastone_phase1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = shannon_bin, y = atleastone_binned)) +
  geom_point(shape = 4, size = 1) +
  geom_smooth(method = 'lm', size = 0.5, fill = 'red', alpha = 0.1, col = 'red4', se = FALSE) +
  theme_classic() +
  labs(
    x = "Target Diversity",
    y = "At Least One Phase 1"
  ) +
  theme(text = element_text(size = 8))

ggsave("Figures/diversity_atleastone_phase1.pdf",
       height = 2, 
       width = 2)


##----------------------------------------------------------------------------------------
## Figure D.1: 
## New Gene-Disease Discoveries Over Time
##----------------------------------------------------------------------------------------

gwas <- readr::read_tsv("Data/gwas_catalog.tsv")

discovery <- gwas %>% 
  select(disease = `DISEASE/TRAIT`, gene_id = SNP_GENE_IDS, date = DATE) %>% 
  separate_rows(gene_id, sep = ', ') %>% 
  group_by(disease, gene_id) %>% 
  filter(date == min(date),
         !is.na(gene_id)) 

discovery %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_line(col = 'darkblue', lwd = 1.5) +
  theme_classic2() +
  labs(x = 'Year', y = 'New Gene-Disease Discoveries') -> discovery_over_time

ggsave("Figures/discovery_over_time.pdf", discovery_over_time, width = 4, height = 4)
