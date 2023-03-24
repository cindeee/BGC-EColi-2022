# libraries ----------------------------

library(tidyverse)
library(readr)
library(readxl)
library(cowplot)


# load data ----------------------------


metadata = read_excel("data/MAIN_metadata.xlsx", 
                      sheet = "metadata")

phylo = 
  metadata %>% 
  filter(Origin %in% c('AUS', 'ECOREF')) %>% 
  filter(Discard == 'No') %>% 
  select(fasta, Broadphenotype, phylogroup) %>% 
  distinct(fasta, .keep_all = TRUE) %>% 
  mutate(genome = str_sub(fasta, 
                          start = 1, end = -7)) %>% 
  select(-fasta)


gene_pa = read_delim("data/gene_presence_absence.Rtab", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)





## transpose the df ------

colnames(gene_pa)

# transposing a matrix is super easy but a dataframe in R is a pain
# first we need to make the table long
gene_pa_long = gene_pa %>% 
  pivot_longer(cols = `100`:SPC_4, 
               names_to = 'genome', 
               values_to = 'presence')

# then we can add the metadata info to the table itself
gene_pa_long = gene_pa_long %>% 
  left_join(phylo)


# now we can make it wider again, but using the gene info instead of genomes
# as columns

gene_pa_wide = gene_pa_long %>% 
  pivot_wider(names_from = Gene, 
              values_from = presence, 
              values_fill = 0)



## exploration -------

gene_pa_long %>% 
  filter(presence != 0) %>% 
  group_by(Gene) %>% 
  count() %>% 
  ggplot(aes(n)) +
  geom_histogram() +
  theme_cowplot()

## WHAT IS THIS PLOT REPRESENTING? 


# Dimensionality reduction ------

## PCA plot ------

pca_fit = gene_pa_wide %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = F) # do PCA on scaled data


pca_fit %>%
  augment(gene_pa_wide) %>% # add original dataset back in
  drop_na(phylogroup) %>% 
  mutate(phylogroup = str_replace_all(phylogroup, 'E or cladeI', 'E')) %>% 
  filter(phylogroup != 'cladeI') %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, 
             color = phylogroup,
             fill = phylogroup)) + 
  stat_ellipse(geom = 'polygon', alpha = 0.2) +
  geom_point(size = 2.5) +
  theme_half_open(12) + 
  background_grid()



pca_fit %>%
  augment(gene_pa_wide) %>% # add original dataset back in
  drop_na(phylogroup) %>% 
  # mutate(phylogroup = str_replace_all(phylogroup, 'E or cladeI', 'E')) %>% 
  filter(phylogroup != 'cladeI') %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, 
             color = Broadphenotype,
             fill = Broadphenotype)) + 
  stat_ellipse(geom = 'polygon', alpha = 0.2) +
  geom_point(size = 2.5) +
  theme_half_open(12) + 
  background_grid()

## A lighter representation would be without the genes that are always present, 
# HOW DO WE GET RID OF THEM? 

pca_fit %>%
  tidy(matrix = "eigenvalues")

pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  head(10) %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:18) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)




# number of genes ---------------------------------------------------------

# plot to see how many genes we have per genome
gene_pa_long %>% 
  filter(presence == 1, phylogroup != 'cladeI') %>% # remove the 0s
  mutate(phylogroup = str_replace_all(phylogroup,'E or cladeI', 'E')) %>% 
  drop_na(phylogroup) %>% 
  group_by(genome, phylogroup) %>% 
  count() %>% 
  ggplot(aes(x = phylogroup, y = n, fill = phylogroup)) +
  geom_boxplot() +
  labs(
    y = 'number of genes',
    x = 'phylogroups'
  ) +
  geom_point(position = position_jitterdodge()) +
  theme_cowplot()

# calculate stats between phylogroups pairwise
# which group has the highest gene count of all? which measure would you use to prove it? 

# which are the genes 

# https://rpkgs.datanovia.com/rstatix/reference/t_test.html


# install.packages('rstatix')
library(rstatix)

gene_pa_long %>% 
  group_by(variables) %>% 
  pairwise_t_test(formula = "n ~ phylogroup", detailed = TRUE)




