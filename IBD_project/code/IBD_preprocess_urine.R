# load library ----
library(readxl)
library(matrixStats)
library(openxlsx)
library(dplyr)
library(tidyverse)

setwd('C:/Users/cindy/OneDrive - Imperial College London/Desktop/IBD_project/data/urine')
getwd()

rm(list=ls(all= T))


u_merged = read_excel('merged_urine.xlsm', sheet = 'Inception cohort')

colnames(u_merged)

#column names and cleaning
colClean = function(x){ colnames(x) <- gsub("\\s|\\(|\\)|\\'|,|:|\\||\\/", ".", colnames(x)); x } 
u_merged = colClean(u_merged)
u_merged = u_merged%>% 
  mutate(Disease.type = str_replace_all(Disease.type, 'healthy control', 'Healthy control')) %>%  
  mutate(Disease.type = str_replace_all(Disease.type, 'disease control', 'Disease control')) %>% 
  mutate(Disease.subtype...4 = case_when(Disease.type == 'UC' ~ 'IBD',
                                         Disease.type == 'CD' ~ 'IBD',
                                         Disease.type == 'Disease control'~ 'Control',
                                         Disease.type == 'Healthy control'~ 'Control')) %>% 
  mutate(Disease.type = as.factor(Disease.type)) %>%
  mutate(Disease.subtype...4 = as.factor(Disease.subtype...4))
  
#for padj
u_merged_org = u_merged

dim(u_merged)
u_merged_z = u_merged[,19:213]
u_merged[,19:213] = (u_merged_z-rowMeans(u_merged_z))/(rowSds(as.matrix(u_merged_z)))[row(u_merged_z)]

u_merged = u_merged %>% 
  arrange(desc(Disease.type), Sample.check)

colnames(u_merged)
u_merged[,c(13,15,16)] = lapply(u_merged[,c(13,15,16)], factor)

# diet as 1 column
u_merged = u_merged %>%
  mutate(Diet = case_when( Meat == 1 & Vegetarian ==0 ~ 'Meat',
                           Meat == 0 & Vegetarian ==1 ~ 'Vegetarian',
                           Meat == 0 & Vegetarian ==0 ~ 'Vegan')) %>% 
  select(-Meat) %>% 
  select(-Vegetarian) %>%
  select(-DOB) %>%  
  select(-Date.of.sample) %>%  
  mutate(Sex.M.1..or.F.0. = case_when( Sex.M.1..or.F.0. == 1 ~ 'Male',
                                       Sex.M.1..or.F.0. == 0 ~ 'Female'))


# data exploration for inception
CrossTable(u_merged$Disease.type, u_merged$Responder)

table(u_merged$Diet)

write.xlsx(u_merged, 'meta_ui_z.xlsx')


## 2. multiple t tests 

# HC_UC
u_HC_UC = u_merged_org %>%
  filter(Disease.type %in% c('UC', 'Healthy control')) %>%  
  group_by(Disease.type) %>% 
  select(-c(1,2,4:18, Disease.subtype...4))

HC_UC_pu = lapply(u_HC_UC[-1], function(x) wilcox.test(x ~ u_HC_UC$Disease.type)$p.value)
adj.pval=p.adjust(HC_UC_pu, method='BH')
adj.pval = sort(unlist(HC_UC_pu), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)

# HC_CD
u_HC_CD = u_merged_org %>% 
  filter(Disease.type %in% c('CD', 'Healthy control')) %>%  
  group_by(Disease.type) %>% 
  select(-c(1,2,4:18,Disease.subtype...4))

HC_CD_pu = lapply(u_HC_CD[-1], function(x) wilcox.test(x ~ u_HC_CD$Disease.type)$p.value)
adj.pval=p.adjust(HC_CD_pu, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)

# HC_DC
u_HC_DC = u_merged_org %>% 
  filter(Disease.type %in% c('Disease control', 'Healthy control')) %>%  
  group_by(Disease.type) %>% 
  select(-c(1,2,4:18,Disease.subtype...4 ))

HC_DC_pu = lapply(u_HC_DC[-1], function(x) wilcox.test(x ~ u_HC_DC$Disease.type)$p.value)
adj.pval=p.adjust(HC_DC_pu, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)

#HC_IBD 
u_HC_IBD = u_merged_org %>% 
  filter(Disease.type %in% c('Healthy control', 'UC', 'CD')) %>%  
  group_by(Disease.subtype...4) %>% 
  select(-c(1:18))

HC_IBD_pu = lapply(u_HC_IBD[-ncol(u_HC_IBD)], function(x) wilcox.test(x ~ u_HC_IBD$Disease.subtype...4)$p.value)
adj.pval=p.adjust(HC_IBD_pu, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)

# caucasian (IBD patients only)
u_cau = u_merged_org %>% 
  filter(Disease.type %in% c('UC','CD')) %>%  
  group_by(Ethnicity) %>% 
  select(-c(1:9,11:18, Disease.subtype...4))

cau_pu = lapply(u_cau[-1], function(x) wilcox.test(x ~ u_cau$Ethnicity)$p.value)
adj.pval=p.adjust(cau_pu, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)

# sex 
u_fm = u_merged_org %>% 
  mutate(Sex.M.1..or.F.0. = case_when( Sex.M.1..or.F.0. == 1  ~ 'Male',
                                       Sex.M.1..or.F.0. == 0  ~ 'Female')) %>% 
  mutate(Sex.M.1..or.F.0.= as.factor(Sex.M.1..or.F.0.)) %>% 
  group_by(Sex.M.1..or.F.0.) %>% 
  select(-c(1:12,13:18,Disease.subtype...4 ))

u_fm_pu = lapply(u_fm[-1], function(x) wilcox.test(x ~ u_fm$Sex.M.1..or.F.0.)$p.value)
adj.pval=p.adjust(u_fm_pu, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)