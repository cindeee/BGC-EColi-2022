
# load library ----
library(readxl)
library(matrixStats)
library(openxlsx)
library(dplyr)
library(tidyverse)
#install.packages('gmodels')
library(gmodels)
library(tidyr)
library(pander)
library(clipr)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")


rm(list=ls(all= T))
setwd('C:/Users/cindy/OneDrive - Imperial College London/Desktop/IBD_project/data/faecal')
getwd()


# faecal dataset ----
getSheetNames('v2.0_FW Reversepositive_.xlsm')
getSheetNames('v2.0_FW_ReverseNEG_AnnotatedData.xlsm')
getSheetNames('V2.0FINAL_STR_HPOS_Annotated_combinedData.xlsm')

rposi = read_excel('v2.0_FW Reversepositive_.xlsm', sheet = 'Inception cohort')
rnegi = read_excel('v2.0_FW_ReverseNEG_AnnotatedData.xlsm', sheet = 'Inception')
posi = read_excel('V2.0FINAL_STR_HPOS_Annotated_combinedData.xlsm', sheet = 'Inception cohort')


# 1. untargeted ----

#column names and cleaning
colnames(rposi)
colnames(rnegi)

colClean = function(x){ colnames(x) <- gsub("\\s|\\(|\\)|\\'|,|:|\\||\\/", ".", colnames(x)); x } 
rposi = colClean(rposi)
rnegi = colClean(rnegi)
posi = colClean(posi)

colnames(rposi)
colnames(rnegi)
colnames(posi)

# remove columns and join
posi = posi %>% 
  rename(Sample.check=Check.sample.ID)

intersect(intersect(names(rposi), names(rnegi)), names(posi))

clean_mi1 = rposi[ -c(1,2,7:10,18) ]
clean_mi2 = rnegi[-c(1,2,4:18)]
clean_mi3 = posi[ -c(1,2,4:18)]

joined = clean_mi1 %>%
  left_join(clean_mi2, by=c('Sample.check'='Sample.check', 'Disease.type'='Disease.type')) %>% 
  left_join(clean_mi3, by=c('Sample.check'='Sample.check', 'Disease.type'='Disease.type'))

#run code for padj
joined_org = joined

dim(joined)
joinedn = joined[,13:134]
joined_z = (joinedn-rowMeans(joinedn))/(rowSds(as.matrix(joinedn)))[row(joinedn)]


# arrange rows
joined[,13:134] = joined_z
joined = joined %>% 
  arrange(desc(Disease.type), Sample.check)

# change diet into 1 column
joined = joined %>%
  mutate(Diet = case_when( Meat == 1 & Vegetarian ==0 ~ 'Meat',
                           Meat == 0 & Vegetarian ==1 ~ 'Vegetarian',
                           Meat == 0 & Vegetarian ==0 ~ 'Vegan')) %>% 
  select(-Meat) %>% 
  select(-Vegetarian) %>% 
  mutate(Sex.M.1..or.F.0. = case_when( Sex.M.1..or.F.0. == 1 ~ 'Male',
                                       Sex.M.1..or.F.0. == 0 ~ 'Female'))
                                        
# change data type
colnames(joined)
joined[,7:9] = lapply(joined[,7:9], factor)

joined[,6] = lapply(joined[,6], numeric)
sapply(joined, class)




write.xlsx(joined, 'meta_fi_z1.xlsx')

## b. data exploration ----
colnames(posi)
typeof(posi[,5])
CrossTable(posi$Disease.type, posi$Responder)

table(joined$Diet)

## c. univariate analysis ----
# metabolites VIP > 1.30
# trial multiple univariate t test 

HC_UC = c('Riboflavin vit B2',
         'N-alpha-Acetyl-L-lysine',
         'Taurine',
         'N1.N12-Diacetylspermine',
         'Suberic.acid',
         'Deoxycholic.acid',
         'Hypoxanthine',
         '3-Methylglutaric.acid',
         'Adipic.acid',
         'Tryptophan',
         '4-Hydroxybenzoic.acid',
         '2-Methylglutaric.acid',
         '1-Methyladenosine',
         'Phenylalanine',
         'Succinic.acid',
         'N-acetylglutamic.acid',
         'Glutaric.acid',
         'Glycodeoxycholic.acid',
         'Azelaic.Acid'
         )

#HC_UC
f_HC_UC = joined_org %>% 
  filter(Disease.type %in% c('UC', 'Healthy control')) %>%  
  group_by(Disease.type) %>% 
  select(-c(2:12))

# p value with adjustment 
pval = lapply(f_HC_UC[-1], function(x) wilcox.test(x ~ f_HC_UC$Disease.type)$p.value)

adj.pval=p.adjust(pval, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)

write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)



# HC_CD
f_HC_CD = joined_org %>% 
  filter(Disease.type %in% c('CD', 'Healthy control')) %>%  
  group_by(Disease.type) %>% 
  select(-c(2:12))


HC_CD_pf = lapply(f_HC_CD[-1], function(x) wilcox.test(x ~ f_HC_CD$Disease.type)$p.value)

adj.pval=p.adjust(HC_CD_pf, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)

    #test
x_sorted['Thymine']

wilcox.test(f_HC_CD['Thymine'] ~ f_HC_CD$Disease.type)

boxplot(Taurine ~ Disease.type, data = f_HC_CD)

boxplot(Thymine ~ Disease.type, data = f_HC_CD)




# HC_DC
f_HC_DC = joined_org %>% 
  filter(Disease.type %in% c('Disease control', 'Healthy control')) %>%  
  group_by(Disease.type) %>% 
  select(-c(2:12))


HC_DC_pf = lapply(f_HC_DC[-1], function(x) wilcox.test(x ~ f_HC_DC$Disease.type)$p.value)
adj.pval=p.adjust(HC_DC_pf, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)

# HC VS IBD
f_HC_IBD = joined_org %>% 
  filter(Disease.type %in% c('Healthy control', 'UC', 'CD')) %>%  
  group_by(Disease.subtype...4) %>% 
  select(-c(1, 3:12))

HC_IBD_pf = lapply(f_HC_IBD[-1], function(x) wilcox.test(x ~ f_HC_IBD$Disease.subtype...4)$p.value)
adj.pval=p.adjust(HC_IBD_pf, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)

# HC VS IBDS
f_HC_IBDS = joined_org %>% 
  
  mutate(Disease.subtype...4 = ifelse(Disease.type == 'Disease control','IBD',as.character(Disease.subtype...4))) %>%   
  mutate(Disease.subtype...4 = as.factor(Disease.subtype...4)) %>% 
  group_by(Disease.subtype...4) %>% 
  select(-c(1, 3:12))

HC_IBDS_pf = lapply(f_HC_IBDS[-1], function(x) wilcox.test(x ~ f_HC_IBDS$Disease.subtype...4)$p.value)
adj.pval=p.adjust(HC_IBDS_pf, method='BH')
adj.pval = sort(unlist(adj.pval), decreasing = FALSE)
format(adj.pval,scientific = FALSE)
write.table(adj.pval, "clipboard", sep="\t", row.names=TRUE, col.names=FALSE)



# 2. targeted , anova ----

getSheetNames('v2.0_FW_SCFA1.xlsx')

scfa = read_excel('v2.0_FW_SCFA1.xlsx', sheet = 'IC_final')
colClean = function(x){ colnames(x) <- gsub("\\s|\\(|\\)|\\'|,|:|\\||\\/|\\-", ".", colnames(x)); x } 
scfa = colClean(scfa)

colnames(scfa)
nrow(scfa)

# MISSING f027

#lactate
scfa_ba = aov(Lactate.µmol.L ~ Disease.type, data = scfa)
summary(scfa_ba)
TukeyHSD(scfa_ba)

scfa_ba2 = aov(Acetate.µmol.L ~ Disease.type, data = scfa)
summary(scfa_ba2)
TukeyHSD(scfa_ba2)

scfa_ba3 = aov(Propionate.µmol.L ~ Disease.type, data = scfa)
summary(scfa_ba3)
TukeyHSD(scfa_ba3)

# sig
scfa_ba4 = aov(scfa$`2.Hydroxybutyrate.µmol.L` ~ Disease.type, data = scfa)
summary(scfa_ba4)
TukeyHSD(scfa_ba4)

boxplot(`2.Hydroxybutyrate.µmol.L` ~ Disease.type, data = scfa)

scfa_ba5 = aov(Isobutyrate.µmol.L ~ Disease.type, data = scfa)
summary(scfa_ba5)
TukeyHSD(scfa_ba5)

scfa_ba6 = aov(Butyrate.µmol.L ~ Disease.type, data = scfa)
summary(scfa_ba6)
TukeyHSD(scfa_ba6)
boxplot(Butyrate.µmol.L ~ Disease.type, data = scfa)


scfa_ba7 = aov(`2.Methylbutyrate.µmol.L` ~ Disease.type, data = scfa)
summary(scfa_ba7)
TukeyHSD(scfa_ba7)

scfa_ba8 = aov(`Isovalerate.µmol.L` ~ Disease.type, data = scfa)
summary(scfa_ba8)
TukeyHSD(scfa_ba8)

scfa_ba9 = aov(`Valerate.µmol.L` ~ Disease.type, data = scfa)
summary(scfa_ba9)
TukeyHSD(scfa_ba9)

scfa_ba10 = aov(`Hexanoate.µmol.L` ~ Disease.type, data = scfa)
summary(scfa_ba10)
TukeyHSD(scfa_ba10)

#sig
scfa_ba11 = aov(`Murocholic.Acid..10-1000nM.` ~ Disease.type, data = scfa)
summary(scfa_ba11)
TukeyHSD(scfa_ba11)

boxplot(`Murocholic.Acid..10.1000nM.` ~ Disease.type, data = scfa)

scfa_ba12 = aov(`5.beta.Cholanic.Acid.3.alpha..6.alpha.diol.7.one..2.5.1000.` ~ Disease.type, data = scfa)
summary(scfa_ba12)
TukeyHSD(scfa_ba12)

scfa_ba13 = aov(`Hyocholic.Acid..1.500NM.` ~ Disease.type, data = scfa)
summary(scfa_ba13)
TukeyHSD(scfa_ba13)

scfa_ba14 = aov(`Glycochenodeoxycholic.Acid..2.5.2500nM.` ~ Disease.type, data = scfa)
summary(scfa_ba14)
TukeyHSD(scfa_ba14)

scfa_ba15 = aov(`Glycocholic.Acid..2.5.2500.` ~ Disease.type, data = scfa)
summary(scfa_ba15)
TukeyHSD(scfa_ba15)

scfa_ba16 = aov(`Taurocholic.Acid..2.5.2500.` ~ Disease.type, data = scfa)
summary(scfa_ba16)
TukeyHSD(scfa_ba16)

scfa %>% 
  group_by(Disease.type) %>% 
  summarise(ze(mean = `Taurocholic.Acid..2.5.2500.`)) %>% 
  pander
#sig
scfa_ba17 = aov(`Glycolithocholic.Acid.3.Sulfate..2.5.2500.` ~ Disease.type, data = scfa)
summary(scfa_ba17)
TukeyHSD(scfa_ba17)
write.xlsx(clean_bai, 'targeted_inception.xlsx')

boxplot(`Glycolithocholic.Acid.3.Sulfate..2.5.2500.` ~ Disease.type, data = scfa)




## 3. CHi quared test ----

