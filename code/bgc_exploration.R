
# libraries and data ---------------------------------------------------------------

library(tidyverse)
library(readr) # read csv
library(readxl) # read excel
library(ggplot2)
library(stringr)
library(dplyr)
# install.packages("cowplot")
library(cowplot)
library(broom)
# install.packages('viridis')
library(viridis)

bgc = read_csv("data/BGCs/antismash_summary.csv")

# Basic operations ------------------------------------------------------------

# AIM: learn %>%, select, filter, mutate, groupby, summarise

# add up dimensions
sum(dim(bgc)) # concatenate f(x).messy when operating several f(x).

# pipe operator is much clearer.
bgc %>% 
  dim() %>%  
  sum()  

# select columns and filter out specific value 
bgc %>% 
  select(genome, type, cluster, contig, start, end) %>% 
  #filter in col 'type'
  filter(type %in% c('NRPS', 'siderophore'), start != 0)


# mutate: generate new columns. assign it to bgc_counts
# you can modify column by assigning same col name 
bgc_counts = bgc %>% 
  select(genome, type, cluster, contig,start,end) %>% 
  filter(start!=0) %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  # group_by
  group_by(type) %>% 
  count(sort = TRUE)

# group_by : group BGC type
bgc %>% 
  select(genome, type, cluster, contig,start,end) %>% 
  filter(start!=0) %>% 
  mutate(length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  group_by(type) %>% 
  # summarise: 
  summarise(
    gene_mean = mean(length), gene_std = sd(length)) %>% 
  arrange(desc(gene_mean)) # descending / ascending
  
# group_by type of BGC
# summarise create new df.
gene_length = bgc %>% 
  select(genome, cluster, type, contig, start, end) %>% 
  filter(start != 0 ) %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  group_by(type) %>% 
  # calculate gene mean and sd of different BGC types
  # n = n() : count nrow by group. 
  summarise(
    gene_mean = mean(gene_length),
    gene_std = sd(gene_length), n = n()
  ) %>% 
  arrange(desc(gene_mean)) # order


# Exploration  ------------------------------------------------------------

# AIM: find any relationships between 
## 1. bgc genome, type ====

length(bgc$genome) # does not = genome
length(unique(bgc$genome))# 747 distinct genomes.

# distinct: select unique rows from df
# count = unique values
# .keep_all retain other columns 
bgc %>% 
  distinct(genome, .keep_all = TRUE) %>% 
  count() 

# geom_point. aes = asthetics
# 'fill' = filling the data point 
gene_length %>% 
  ggplot(aes(x = type, y =gene_mean )) + 
  geom_point(shape = 8, colour = "black", fill = "white", 
             size = 1, stroke = 1) + 
  # BGC types in alphabetical order
  scale_x_discrete(limits = rev) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  

# geom_bar: barplot of gene mean by BGC type
# change axis to make data more readable. 
gene_length %>% 
  ggplot(aes(x = gene_mean, y =type, fill = type)) + 
  geom_bar(stat='identity', show.legend = F) + 
  labs(y = 'gene mean', x = 'BGC types') + # labs: relabel axis
  theme_cowplot(10) + # cowplot gives cool colours
  scale_y_discrete(limits = rev) # alphabetical order 
# save plot as pdf / png.
ggsave('output/figures/barplot_gene_length.pdf', width=5, height=6)

#  boxplot of gene length by BGC type
# order of plotting matters, they overlay. eg. boxplot -> points
bgc %>% 
  select(genome, type, cluster, contig,start,end) %>% 
  filter(start!=0) %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>% 
  ggplot(aes(y = type, x = gene_length, fill = type)) +
  geom_boxplot(show.legend = F) +
  geom_point(alpha = 0.5, show.legend = F) +
  theme_cowplot(12) 
ggsave('output/figures/boxplot_bgc_gene.pdf', width=5, height=6)
# stat::filter VS dplyr::filter. 
# :: can specify package to avoid conflict. 

## 2. metadata ============================================

# load metadata
metadata = read_excel("data/MAIN_metadata.xlsx")

## AIM: join data with bgc
# does not work. no row contents and colnames match
bgc %>% 
  left_join(metadata) 

# column names 
# fasta column has '.fasta' 
colnames(metadata)
view(metadata['fasta'])

# 1. remove '.fasta', rename column as genome 
# 2. join with bgc.

# modify metadata as phylo for joining  
phylo = metadata %>%  
  # select specific origins for analysis
  filter(Origin %in% c('AUS', 'ECOREF')) %>% 
  # choose undiscarded data
  filter(Discard == 'No') %>% 
  select(fasta, Broadphenotype, phylogroup) %>% 
  distinct(fasta, .keep_all = TRUE) %>% 
  # make new col 'genome' from 'fasta'.
  mutate(genome = str_sub(fasta, start = 1, end = -7)) %>% 
  # del 'fasta' col
  select(-fasta) 

# let's join dataframe as bgc_extended 
bgc_extended = bgc %>% 
  left_join(phylo)


# Visualisation  ----------------------------------------------------------

##1. data processing ====
colnames(bgc_extended)
unique(bgc_extended['phylogroup'])

# boxplot: phylogroups and gene length
bgc_extended %>% 
  # process it again 
  filter(start != 0) %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>%
  #filter out NA in phylogroup
  filter(!is.na(phylogroup)) %>%
  # filter cladeI
  filter(!phylogroup %in% c('NA', 'cladeI')) %>% 
  # convert 'E or cladeI' to 'E'
  mutate(phylogroup = replace( phylogroup, phylogroup=='E or cladeI', 'E')) %>% 
  # ggplot : phylogroup and gene length 
  ggplot(aes(y = phylogroup, 
             x = gene_length, 
             fill = phylogroup)) +
  geom_boxplot(show.legend = T) +
  geom_point(alpha = 0.3,
             show.legend = F) +
  theme_cowplot(15) + 
  scale_y_discrete(limits = rev)

ggsave('output/figures/barplot_phylo_gene.pdf', width=5, height=6)


# barplot: BGC proportion by phylogroup
bgc_extended %>% 
  filter(start != 0 ) %>%
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  # filter NA and E, replace 'E or CaldeI'
  filter(!is.na(phylogroup)) %>%
  filter(!phylogroup %in% c('NA', 'cladeI')) %>%  # != 'cladeI'
  mutate(phylogroup = replace( phylogroup, phylogroup=='E or cladeI', 'E')) %>% 
  # ggplot
  ggplot(aes(y = type, 
             fill = phylogroup)) +
  geom_bar(position = "fill", 
           show.legend = T) +
  theme_cowplot(15)

ggsave('output/figures/barplot_bgc_phylo.pdf', width=5, height=6)
 
## boxplot : BGC by phenotype
# types
unique(bgc_extended$Broadphenotype) # there are NA. unknown.

bgc_extended %>% 
  filter(start != 0 ) %>%
  # filter(type == 'NRPS') %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(!is.na(phylogroup)) %>%
  filter(!phylogroup %in% c('NA', 'cladeI')) %>%  # != 'cladeI'
  mutate(phylogroup = replace( phylogroup, phylogroup=='E or cladeI', 'E')) %>% 
  # filter NA in phenotype
  filter(!is.na(type)) %>% # or drop_na(phylogroup)
  ggplot(aes( y = Broadphenotype,fill = phylogroup)) +
  geom_bar(position = 'fill', show.legend = T)

ggsave('output/figures/boxplot_BGC_pheno.pdf', width=5, height=6)

###  remarks
# 1. using case_when for conditional reasoning 
bgc_extended %>% 
  mutate(phylogroup = case_when(phylogroup=='E or cladeI'~ 'E', # tiddle for changing
                                TRUE~phylogroup), # must, or else other values change to NA
         .before = type)

# 2. useful computational operations / without pipe
seq(4,6,0.5)
clean_phylo = phylogroups[phylogroups != 'cladeI']
clean_phylo = clean_phylo[!is.na(clean_phylo)]


## 2. statistical analysis -------------------------------------------------------------------

# AIM: statistical significance of BGC gene lengths in different phylogroups? compare >= 5. 

## 1. a vs b2
a_vs_b2 = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(phylogroup %in% c('A','B2')) %>% 
  select(phylogroup, gene_length)
# linear model 
model = lm(gene_length ~ phylogroup, data = a_vs_b2) 
sum_model = summary(model)
# tidy() : summarises statistical findings
stats = tidy(sum_model) %>% 
  # rearrange column before col 'term'
  mutate(comparison = 'A vs B2', 
         .before = 'term') %>%
  filter(term != '(Intercept)') 

# accessing coefficients
sum_model$coefficients

# 2. c VS E 
c_vs_e = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(phylogroup %in% c('C','E')) %>% 
  select(phylogroup, gene_length)

model2 = lm(gene_length ~ phylogroup, data = c_vs_e)
sum_model2 = summary(model2)

# tidy sum_model2 as stats2 
stats2 = tidy(sum_model2) %>% 
  mutate(comparison = 'C vs E', 
         .before = 'term') %>% 
  filter(term != '(Intercept)') %>% 
  bind_rows(stats) # bind stats to stats2 
stats2

# 3. B1 VS B2 
b1_vs_b2 = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(phylogroup %in% c('B1','B2')) %>% 
  select(phylogroup, gene_length)

model3 = lm(gene_length ~ phylogroup, data = b1_vs_b2)
sum_model3 = summary(model3)

# tidy sum_model
stats3 = tidy(sum_model3) %>% 
  mutate(comparison = 'B1 VS B2', 
         .before = 'term') %>% 
  filter(term != '(Intercept)') %>% 
  bind_rows(stats2) # bind stats to stats2 
stats3


# 4. D VS F 
d_vs_f = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(phylogroup %in% c('D','F')) %>% 
  select(phylogroup, gene_length)

model4 = lm(gene_length ~ phylogroup, data = d_vs_f)
sum_model4 = summary(model4)

# tidy sum_model
stats4 = tidy(sum_model4) %>% 
  mutate(comparison = 'D VS F', 
         .before = 'term') %>% 
  filter(term != '(Intercept)') %>% 
  bind_rows(stats3) # bind stats to stats2 
stats4

# 5. A VS C 
a_vs_c = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(phylogroup %in% c('A','C')) %>% 
  select(phylogroup, gene_length)

model5 = lm(gene_length ~ phylogroup, data = a_vs_c)
sum_model5 = summary(model5)

# tidy sum_model
stats5 = tidy(sum_model5) %>% 
  mutate(comparison = 'A VS C', 
         .before = 'term') %>% 
  filter(term != '(Intercept)') %>% 
  bind_rows(stats4) # bind stats to stats2 
stats5

save(stats5, file = 'output/tables/stat_length_phylo.Rda')

# phenotypes versus phenotypes
vs_phenotype = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(!is.na(phylogroup)) %>%
  filter(!phylogroup %in% c('NA', 'cladeI')) %>%  # != 'cladeI'
  mutate(phylogroup = replace( phylogroup, phylogroup=='E or cladeI', 'E')) %>% 
  drop_na(Broadphenotype) %>% 
  select(Broadphenotype, gene_length)

model_phenotype = lm(gene_length ~ Broadphenotype,
                     data = vs_phenotype)
summary(model_phenotype)


## 3. heatmap ====
# colour package viridis: 
# "magma" =  "A". "inferno" = "B". "plasma" = "C".
# "viridis" = "D". "cividis" = "E". "rocket" ="F".
# "mako" ="G". "turbo" = "H"

# heatmap shows categorical relationships in 2 axis to observe patterns.
colnames(bgc_extended) # as ref

# geom_tiles : heatmap of genome and bgc type. 
# VS other plots: it shows abundance.
bgc_extended %>% 
  ggplot(aes(x = genome, y = type, fill = type)) +
  geom_tile(show.legend = F) +
  theme(axis.text.x = element_blank())

# heatmap of phylogroup, bgc type , gene length
bgc_extended %>% 
  drop_na(phylogroup) %>% 
  mutate(type = as.factor(type)) %>% 
  group_by(phylogroup, type) %>% # gorup by both factors
  filter(phylogroup != 'cladeI') %>% 
  mutate(gene_length = end - start) %>% 
  summarise(Mean = mean(gene_length)) %>% # grouped df
  ggplot(aes(x = phylogroup, y = type, fill = Mean)) +
  geom_tile(show.legend = T) +
  theme_cowplot() +
  scale_fill_viridis() + 
  theme(axis.text.x = element_text(angle = 90))

ggsave('output/figures/heatmap_bgc_phylo_gene.pdf', height = 6, width = 6)

# heatmap of bacterial phenotype, bgc type (n)
bgc_extended %>% 
  # replace NA to unknown
  mutate(Broadphenotype = na_if(Broadphenotype, "Unknown")) %>% 
  mutate(type = as.factor(type)) %>% 
  group_by(Broadphenotype, type) %>% 
  #count(Broadphenotype, type, sort = T) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = Broadphenotype, y = type, fill = n)) +
  geom_tile(show.legend = T) +
  theme_cowplot() +
  scale_fill_viridis(option = 'plasma') +
  theme(axis.text.x = element_text(angle = 90))

ggsave('output/figures/heatmap_bgc_pheno_gene.pdf', height = 6, width = 6)




