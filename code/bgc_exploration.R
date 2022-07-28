
# libraries ---------------------------------------------------------------

library(tidyverse)
library(readr) # read csv
library(readxl) # read excel
library(ggplot2)
library(stringr)
# install.packages("cowplot")
library(cowplot)
install.packages('vtable')
library(broom)

install.packages("palmerpenguins")

# load data ------------------------------------------------------------

bgc = read_csv("data/BGCs/antismash_summary.csv")

## 06072022 ====

sum(dim(bgc)) # concacenate f(x). not good

# pipe operator 
bgc %>% 
  dim() %>%  
  sum()  

# important f(x): select, filter, mutate, groupby, summarise

# select, filter(select(bgc, genome:contig), type == 'NRPS') 
bgc %>% 
  select(genome, type, cluster, contig, start, end) %>% 
  #filter in col 'type'.revise boolean
  filter(type %in% c('NRPS', 'siderophore'), start != 0)


# gen columns 
bgc %>% 
  select(genome, type, cluster, contig,start,end) %>% 
  mutate(lengthy = end - start) %>% 
  # paste
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  # modify original col 
  mutate (start = start / 2) %>% 
  group_by(genome) 

#  MUTATE for new col 
bgc %>% 
  select(genome, type, cluster, contig,start,end) %>% 
  filter(start!=0) %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  # group_by
  group_by(type) %>% 
  count(sort = TRUE)

# summary operation  
bgc %>% 
  select(genome, type, cluster, contig,start,end) %>% 
  filter(start!=0) %>% 
  mutate(lengthyy = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  group_by(type) %>% 
  # summarise(n =n()) 
  summarise(
    gene_mean = mean(lengthy),
    gene_std = sd(lengthy)
  ) %>% 
  arrange(desc(gene_mean)) # descending / ascending
  

# gene len mean as new object 
gene_length = bgc %>% 
  select(genome, cluster, type, contig, start, end) %>% 
  filter(start != 0 ) %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  group_by(type) %>% 
  summarise(
    gene_mean = mean(gene_lenght),
    gene_std = sd(gene_lenght)
  ) %>% 
  arrange(desc(gene_mean))

# own exploration
for (i in 1:ncol(bgc)) {
  print(count(unique(bgc[,i])))
}
  

## 08072022 ====

length(bgc$genome) # does not = genome
length(unique(bgc$genome))

bgc %>% 
  distinct(genome,.keep_all = TRUE) 
# counting
bgc %>% 
  distinct(type) %>% 
  count()

# ggplot2
gene_length %>% 
  #plot points 
  ggplot(aes(x = type, y =gene_mean )) + 
  geom_point(shape = 8, colour = "black", fill = "white", 
             size = 1, stroke = 1) + 
  #reverse order in x axis
  scale_x_discrete(limits = rev) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
# 1st plot
gene_length %>% 
  #plot points 
  ggplot(aes(x = type, y =gene_mean )) + 
  geom_point(shape = 8, colour = "black", fill = "white", 
             size = 1, stroke = 1) + 
  #reverse order in x axis
  scale_x_discrete(limits = rev) + 
  theme(axis.text.x = element_text(angle = 45)) 

#2nd plot
gene_length %>% 
  #plot points 
  ggplot(aes(x = gene_mean, y =type)) + 
  geom_bar(stat='identity', fill = type ) + 
  labs(y = 'gene mean', x = 'peptidess')
  #reverse order in x axis
  scale_y_discrete(limits = rev)




#aixs.text.x
ggsave('barplot.pdf', width=5, height=6) # save plot as pdf
#theme_cowplot(19) # font
# can put pathway

# stat::filter VS dplyr::filter. :: can specify package 
bgc %>% 
  select(genome, type, cluster, contig,start,end) %>% 
  filter(start!=0) %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  ggplot(aes(y = type, x = gene_length))

# fill = type, in aes ggplot





## 11072022 ============================================

# load metadata
metadata = read_excel("data/MAIN_metadata.xlsx")

## AIM: join data with bgc
bgc %>% 
  left_join(metadata) # does not work rows does not match 
  filter(Origin %in% )
  # mutate(fasta = str_sub(fasta, start=1, end =-7)) %>% 

# create spc col in phylo in metadata
phylo = metadata %>%  
  # trust dani
  filter(Origin %in% c('AUS', 'ECOREF')) %>% 
  # choose undiscarded data
  filter(Discard == 'No') %>% 
  select(fasta, Broadphenotype, phylogroup) %>% 
  distinct(fasta, .keep_all = TRUE) %>% 
  # make new col'genome' from 'fasta'.
  mutate(genome = str_sub(fasta, start = 1, end = -7)) %>% 
  # del 'fasta' col
  select(-fasta) 

# let's join df
bgc_extended = bgc %>% 
  left_join(phylo)
  
# homework 
bgc_extended %>% 
  # start from 0 is not in range. 
  filter(start != 0) %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  # convert to factor
  mutate(type = as.factor(type)) %>%
  #filter out NA 
  filter(!is.na(phylogroup)) %>%
  # filter cladeI
  filter(!phylogroup %in% c('NA', 'cladeI')) %>% 
  #convert 'E or cladeI' to 'E'
  mutate(phylogroup = replace( phylogroup, phylogroup=='E or cladeI', 'E')) %>% 
  # ggplot
  ggplot(aes(y = phylogroup, 
             x = gene_length, 
             fill = phylogroup)) +
  geom_boxplot(show.legend = T) +
  geom_point(alpha = 0.3,
             show.legend = F) +
  theme_cowplot(15) + 
  scale_y_discrete(limits = rev)

ggsave('barplot.pdf', width=5, height=6)


## BGC by phylogroup. proportion
palette = c('#C0E2C9', "#88B7B5", '#C7BC9D',"#847996","#310A31")
bgc_extended %>% 
  filter(start != 0 ) %>%
  # filter(type == 'NRPS') %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  # filter NA and E...
  filter(!is.na(phylogroup)) %>%
  filter(!phylogroup %in% c('NA', 'cladeI')) %>%  # != 'cladeI'
  mutate(phylogroup = replace( phylogroup, phylogroup=='E or cladeI', 'E')) %>% 
  # ggplot
  ggplot(aes(y = type, 
             fill = phylogroup)) +
  geom_bar(position = "fill", 
           show.legend = T) +
  theme_cowplot(15)


ggsave('barplot2.pdf', width=5, height=6)
 
## boxplot of BGC by phenotype
# types
unique(bgc_extended$Broadphenotype) # NA. unknown.

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
  # ggplot
  ggplot(aes( y = Broadphenotype, 
             fill = phylogroup)) +
  geom_bar(position = 'fill', show.legend = T)

ggsave('barplot3.pdf', width=5, height=6)


## 14072022 ====

# solutions last time. in pipe
mutate(phylogroup = case_when(phylogroup=='E or cladeI'~ 'E',
                              TRUE~phylogroup), # must, or else it changes to NA
       .before = type)


#with proportion 
bgc_extended %>% 
  filter(!is.na(phylogroup)) %>%
  filter(!phylogroup %in% c('NA', 'cladeI')) %>%  # != 'cladeI'
  mutate(phylogroup = replace( phylogroup, phylogroup=='E or cladeI', 'E')) %>% 
  # ignore
  group_by(phylogorup,type) %>% 
  mutate(n=n/n()) %>% 
  


# useful computational operations for values 
seq(4,6,0.5)



# stats -------------------------------------------------------------------
## 14072022 ====

c_vs_e = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  # paste
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(phylogroup %in% c('C','E')) %>% 
  select(phylogroup, gene_length)

model = lm(gene_length ~ phylogroup, data = c_vs_e)
summary(model)$pvalue

## a vs b2
a_vs_b2 = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>% 
  filter(phylogroup %in% c('A','B2')) %>% 
  select(phylogroup, gene_length)

model = lm(gene_length ~ phylogroup, data = a_vs_b2) 
sum_model = summary(model)
#accessing coeff
sum_model$coefficients

# homework: comparisons commensal vs pathogenic strians
# by phenotype
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


# gene_len, phylogroups
a_vs_b2 = bgc_extended %>% 
  filter(start != 0 ) %>%
  # filter(type == 'NRPS') %>% 
  mutate(gene_length = end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(phylogroup = as.factor(phylogroup)) %>% 
  filter(phylogroup %in% c('B2', 'E')) %>% 
  select(phylogroup, gene_length)

model = lm(formula = gene_length ~ phylogroup, 
           data = a_vs_b2)

sum_model = summary(model)
sum_model

#me

bgc_extended_tab = bgc_extended %>% 
  filter(start!=0) %>% 
  mutate(gene_length= end - start) %>% 
  mutate(ID = paste(genome, type, sep = '_')) %>% 
  mutate(type = as.factor(type)) %>% 
  drop_na(phylogroup) %>%
  filter(!phylogroup %in% c('NA', 'cladeI')) %>% 
  mutate(phylogroup = replace( phylogroup, phylogroup=='E or cladeI', 'E')) %>% 
  select(phylogroup, gene_length) 




## 18072022====
tidy(table)


## 260722 ====
# heatmap 

bgc_extended %>% 
  ggplot(aes(x = genome, y = type, fill = type)) +
  geom_tile() + 
  theme(axis.text.x = element_blank())

