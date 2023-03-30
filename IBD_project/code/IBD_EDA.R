### EDA ---- 

median(s_merged$Age)
range(s_merged$Age)

s_merged %>%  
  filter(Disease.type == 'Disease control') %>% 
  pull(Age) %>% 
  median()

s_merged %>%  
  filter(Disease.type == 'Disease control') %>% 
  pull(Age) %>% 
  range()


s_merged %>% 
  filter(Disease.type == 'DC') %>% 
  mutate(DAI_value = as.numeric(DAI_value)) %>% 
  pull(DAI_value) %>% 
  median()

s_merged %>% 
  filter(Disease.type == 'DC') %>% 
  mutate(DAI_value = as.numeric(DAI_value)) %>%
  pull(DAI_value) %>% 
  range()

s_merged %>% 
  filter(Disease.type == 'DC') %>% 
  mutate(DAI_value = as.numeric(DAI_value)) %>% 
  pull(DAI_value) %>% 
  range()

#diet changed to 2 types only
s_merged %>% 
  mutate(Diet = as.factor(Diet)) %>% 
  filter(Diet == 'Meat') %>% 
  nrow()

s_merged %>% 
  filter(Disease.type == 'UC') %>% 
  mutate(Diet = as.factor(Diet)) %>% 
  filter(Diet == 'Meat') %>% 
  nrow()

s_merged %>% 
  filter(Disease.type == 'CD') %>% 
  mutate(Diet = as.factor(Diet)) %>% 
  filter(Diet == 'Meat') %>% 
  nrow()

s_merged %>% 
  filter(Disease.type == 'Disease control') %>% 
  mutate(Diet = as.factor(Diet)) %>% 
  filter(Diet == 'Meat') %>% 
  nrow()

s_merged %>% 
  filter(Disease.type == 'Healthy control') %>% 
  mutate(Diet = as.factor(Diet)) %>% 
  filter(Diet == 'Meat') %>% 
  nrow()

s_merged %>% 
  filter(Disease.type == 'Disease control') %>% 
  count(Disease.subtype)

s_merged %>% 
  filter(Disease.type == 'UC') %>% 
  count(Responder)
