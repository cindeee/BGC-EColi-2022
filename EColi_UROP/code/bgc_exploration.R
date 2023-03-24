
# libraries ----

library(tidyverse)
library(readr)
library(readxl)
library(cowplot)

# loading data ---------------------

bgc = read_csv("data/BGCs/antismash_summary.csv")

# test of functions
sum(dim(bgc))

bgc %>%
  dim() %>% 
  sum()

# select
# filter
# mutate
# group by
# summarise

## tidyverse training ---- 

important_types = c('NRPS', 'siderophore')

# counting bgc types in the dataset
bgc_counts = bgc %>% 
  select(genome, cluster, type, contig, start, end) %>% 
  filter(start != 0 ) %>% 
  # filter(type == 'NRPS') %>% 
  mutate(gene_lenght = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  group_by(type) %>% 
  count(sort = TRUE)

# calculating the gene length mean
gene_length = bgc %>% 
  select(genome, cluster, type, contig, start, end) %>% 
  filter(start != 0 ) %>% 
  # filter(type == 'NRPS') %>% 
  mutate(gene_lenght = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  group_by(type) %>% 
  summarise(
    gene_mean = mean(gene_lenght),
    gene_std = sd(gene_lenght)
  ) %>% 
  arrange(desc(gene_mean))




# exploration -------------------------------------------------------------

# calculating the number of genomes and types
length(unique(bgc$genome))

bgc %>% 
  distinct(genome, .keep_all = TRUE) %>% 
  count()

bgc %>% 
  distinct(type)

bgc %>% 
  distinct(genome, type)


# plot the mean gene length

gene_length %>% 
  ggplot(aes(y = gene_mean, x = type, 
             fill = type)) +
  # geom_point(size = 4,
  #            shape = 21,
  #            color = 'black')
  # geom_point(size = 5, color = 'black') +
  geom_bar(stat = 'identity', color = 'black') +
  theme(
    axis.text.x = element_text(
      angle = 90)
  )
  

gene_length %>% 
  ggplot(aes(x = gene_mean, 
             y = fct_reorder(type, gene_mean), 
             fill = type)) +
  # geom_point(size = 4,
  #            shape = 21,
  #            color = 'black')
  # geom_point(size = 5, color = 'black') +
  geom_bar(stat = 'identity', 
           color = '#F56866',
           fill = '#F56866',
           show.legend = FALSE) +
  labs(
    x = 'Gene mean',
    y = 'BGC type',
    caption = 'This plot represents the average gene mean'
  ) +
  # scale_y_discrete(limits = rev) +
  cowplot::theme_cowplot(19)

ggsave('output/figures/barplot_gene_length.png',
       width = 6, height = 6)



# create boxplots

bgc %>% 
  select(genome, cluster, type, contig, start, end) %>% 
  filter(start != 0 ) %>%
  # filter(type == 'NRPS') %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>% 
  ggplot(aes(y = type, x = gene_length, fill = type)) +
  geom_boxplot(show.legend = F) +
  geom_point(alpha = 0.5,
             show.legend = F) +
  theme_cowplot(15) 
  
ggsave('output/figures/boxplot_gene_length.pdf',
       width = 6, height = 6)




# phylogroups -------------------------------------------------------------


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
  

bgc_extended = bgc %>% 
  left_join(phylo) 

# this is a comment 
bgc_extended %>% 
  filter(start != 0 ) %>%
  # filter(type == 'NRPS') %>% 
  mutate(gene_length = end - start) %>% # here we calculate gene length
  mutate(ID = paste(genome, type, sep = '_')) %>% # here we are generating diff IDs
  mutate(type = as.factor(type)) %>% 
  ggplot(aes(y = phylogroup, 
             x = gene_length, 
             fill = phylogroup)) +
  geom_boxplot(show.legend = T) +
  geom_point(alpha = 0.5,
             show.legend = F) +
  theme_cowplot(15)

# -remove the NA from the data
# -remove the cladeI from the data
# -BONUS: convert 'E or cladeI' to 'E'
# -generate a plot with information about how many 
# bgcs are in the genomes by phylogroup and phenotype

phylogroups = unique(bgc_extended$phylogroup)

clean_phylo = phylogroups[phylogroups != 'cladeI']
clean_phylo = clean_phylo[!is.na(clean_phylo)]



bgc_extended %>% 
  # drop_na(phylogroup) %>%
  # filter(phylogroup != 'cladeI') %>% 
  # filter(!is.na(phylogroup)) %>% 
  filter(phylogroup %in% clean_phylo) %>% 
  mutate(phylogroup = case_when(phylogroup == 'E or cladeI' ~ 'E',
                                TRUE ~ phylogroup)) %>% 
  filter(start != 0 ) %>%
  # filter(type == 'NRPS') %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>% 
  ggplot(aes(y = phylogroup, 
             x = gene_length, 
             fill = phylogroup)) +
  geom_boxplot(show.legend = T) +
  geom_point(alpha = 0.5,
             show.legend = F) +
  theme_cowplot(15)



# barplot for types by phylogroups
bgc_extended %>% 
  drop_na(phylogroup) %>% 
  filter(phylogroup != 'cladeI') %>% 
  mutate(phylogroup = case_when(phylogroup == 'E or cladeI' ~ 'E',
                                TRUE ~ phylogroup)) %>% 
  # filter(start != 0 ) %>%
  ggplot(aes(y = type, 
             fill = phylogroup)) +
  geom_bar(position = "fill", 
           show.legend = T) +
  theme_cowplot(15)




bgc_extended %>% 
  drop_na(phylogroup) %>% 
  filter(phylogroup != 'cladeI') %>% 
  mutate(phylogroup = case_when(phylogroup == 'E or cladeI' ~ 'E',
                                TRUE ~ phylogroup)) %>% 
  # filter(start != 0 ) %>%
  ggplot(aes(y = type, fill = Broadphenotype)) +
  geom_bar(show.legend = T) +
  theme_cowplot(15)




phylo %>% 
  ggplot(aes(phylogroup, fill =  phylogroup)) +
  geom_bar() +
  theme_cowplot()




# stats -------------------------------------------------------------------

c_vs_e = bgc_extended %>% 
  filter(start != 0 ) %>%
  # filter(type == 'NRPS') %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(phylogroup = as.factor(phylogroup)) %>% 
  filter(phylogroup %in% c('C', 'E')) %>% 
  select(phylogroup, gene_length)

model = lm(formula = gene_length ~ phylogroup, 
   data = c_vs_e)

sum_model2 = summary(model)



a_vs_b2 = bgc_extended %>% 
  filter(start != 0 ) %>%
  # filter(type == 'NRPS') %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(phylogroup = as.factor(phylogroup)) %>% 
  filter(phylogroup %in% c('A', 'B2')) %>% 
  select(phylogroup, gene_length)

model = lm(formula = gene_length ~ phylogroup, 
           data = a_vs_b2)

sum_model = summary(model)


# take 4 or 5 comparisons, extract the data, and get 
# the stats between the two groups
# BONUS: create a table with the stats and comparisons

library(broom)

stats = tidy(sum_model) %>% 
  mutate(comparison = 'A vs B2', 
         .before = 'term') %>% 
  filter(term != '(Intercept)')
  

stats = tidy(sum_model2) %>% 
  mutate(comparison = 'C vs E', 
         .before = 'term') %>% 
  filter(term != '(Intercept)') %>% 
  bind_rows(stats)



# generate heatmaps -----

bgc_extended %>% 
  ggplot(aes(x = genome, y = type, fill = type)) +
  geom_tile(show.legend = F) +
  theme(axis.text.x = element_blank())


bgc_extended %>% 
  drop_na(phylogroup) %>% 
  group_by(phylogroup) %>% 
  count(type) %>% 
  filter(phylogroup != 'cladeI') %>% 
  ggplot(aes(x = phylogroup, y = type, fill = type)) +
  geom_tile(show.legend = F) +
  theme_cowplot() +
  scale_fill_viridis_d(option = 'turbo') +
  theme(axis.text.x = element_text(angle = 90))
  

library(viridis)

bgc_extended %>% 
  drop_na(phylogroup) %>% 
  group_by(phylogroup) %>% 
  filter(phylogroup != 'cladeI') %>% 
  filter(start != 0) %>% 
  mutate(gene_length = end - start) %>% 
  group_by(phylogroup, type) %>% 
  summarise(Mean = mean(gene_length)) %>% 
  ggplot(aes(x = phylogroup, y = type, fill = Mean)) +
  geom_tile(show.legend = T) +
  scale_fill_viridis_c() +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90))




# for the bacterial phenotype, replace the NA for Unknown 
# plot a heatmap of bgc types (P/A) by bacterial phenotype
# generate a heatmap of of bgc types (n) by bacterial phenotype


bgc_extended %>%
  drop_na(phylogroup) %>%
  group_by(phylogroup, type) %>%
  mutate(gene_length = end - start) %>%
  filter(phylogroup != "cladeI") %>%
  summarise(Mean = mean(gene_length)) %>%
  ggplot(aes(x=phylogroup, y=type, fill=Mean)) +
  geom_tile(show.legend =T) +
  scale_fill_viridis_c() +
  theme_cowplot() +
  theme(axis.text.x= element_text(angle=90))


metadata %>% 
  distinct(ID, .keep_all = T) %>% 
  filter(Discard == 'No') %>% 
  count(Broadphenotype)






bgc_extended %>%
  drop_na(Broadphenotype) %>%
  group_by(Broadphenotype, type) %>%
  summarise(N = n()) %>% 
  # count(Broadphenotype, type, sort=TRUE) %>%
  ggplot(aes(x=Broadphenotype, y=type, fill=N)) +
  geom_tile(show.legend =T) +
  theme_cowplot() +
  theme(axis.text.x= element_text(angle=90))





# PCA ----------------------------------------------------------------------

# read_csv("https://wilkelab.org/classes/SDS348/data_sets/biopsy.csv")

library(broom)

pca_df =
  bgc_extended %>% 
  filter(start != 0) %>% 
  filter(phylogroup != 'cladeI') %>% 
  drop_na(phylogroup) %>% 
  group_by(genome, phylogroup, Broadphenotype, type) %>% 
  count() %>% 
  pivot_wider(names_from = type, 
              values_from = n,
              values_fill = 0) %>% 
  ungroup


pca_fit = pca_df %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = F) # do PCA 


pca_fit %>%
  augment(pca_df %>% 
            select(genome, phylogroup, Broadphenotype)) %>% # add original dataset back in
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, 
             color = phylogroup,
             fill = phylogroup)) + 
  stat_ellipse(geom = 'polygon', alpha = 0.2) +
  geom_point(size = 2.5) +
  theme_half_open(12) + 
  background_grid()


pca_fit %>%
  augment(pca_df) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, 
             color = Broadphenotype,
             fill = Broadphenotype)) + 
  stat_ellipse(geom = 'polygon', alpha = 0.2) +
  geom_point(size = 2.5) +
  theme_half_open(12) + background_grid()


## rotation matrix ####

pca_fit %>%
  tidy(matrix = "rotation")


# define arrow style for plotting
arrow_style = arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 0.5) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal_grid(12)



# variance explained by PCs

pca_fit %>%
  tidy(matrix = "eigenvalues")

pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:18) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)


library(plotly)

pca_plot = pca_fit %>%
  augment(pca_df)

fig <- plot_ly(pca_plot, 
               x = ~.fittedPC1, 
               y = ~.fittedPC2, 
               z = ~.fittedPC3, 
               color = ~phylogroup)

fig





# https://allisonhorst.github.io/palmerpenguins/articles/pca.html





# tSNE --------------------------------------------------------------------

library(Rtsne)



set.seed(142)


tSNE_fit = pca_df %>% 
  column_to_rownames("genome") %>% 
  select(where(is.numeric)) %>%
  # scale() %>% 
  Rtsne(check_duplicates = FALSE,
        perplexity = 20,
        theta = 0.01,
        num_threads = 8)

tSNE_df = tSNE_fit$Y %>% 
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2="V2") %>%
  mutate(genome=pca_df$genome)

tSNE_df = tSNE_df %>%
  inner_join(pca_df %>% 
               select(genome, phylogroup, Broadphenotype), by="genome")


tSNE_df %>%
  ggplot(aes(x = tSNE1, 
             y = tSNE2,
             color = phylogroup))+
  geom_point()+
  theme(legend.position="bottom") +
  theme_minimal(12)

