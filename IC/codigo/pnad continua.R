library(PNADcIBGE)
library(tidyverse)
library(lubridate)
library(survey)
library(stringr)
library(haven)
library(readxl)
library(stats)
library(scales)
library(magrittr)


savepdf <- function(plot, filename = "plot.pdf", width = 8, height = 6) {
  ggsave(filename, plot = plot, device = "pdf", width = width, height = height)
  invisible(plot)
}

directory <- "D:/Bases de Dados"
file_path <- file.path(directory, "pnadc_combinada_2012_2023.csv")
#DO NOT RUN - used to get household survey data through API!!!!!!!
#pnadc_dados <- data.frame(c())
#for (x in seq(2012, 2023, 1)){
  #for(y in c(1,2,3,4)){
    #z <- get_pnadc(year = x, quarter = y, vars = c('Ano', 'Trimestre', 'UF', 'V2007','V1028',
                                              #'V2009', 'V2010', 'V40403',
                                              #'V4010', 'V4013','V4028','V4029', 'V4012',
                                              #'V4039', 'V403312', 'V403322', 'V4021','V4022', 
                                              #'V1022', 'VD3004', 'VD4009'), 
              #deflator = F, design = F, labels = F) %>% 
      #select(Ano, Trimestre, UF, V2007, V1028, V2009, V2010,
             #V4010, V4013, V4012, V4028, V4029, V4039, V403312, V403322,
             #V4021, V4022, ID_DOMICILIO, V1022, VD3004, VD4009, V40403)
    #pnadc_dados <- rbind(z, pnadc_dados)
    #write.csv(pnadc_dados, file = file_path, row.names = FALSE)
  #}
#}


#pnad covid
{
pnad_cov <- read.csv("C:/Users/Chacha/Desktop/FGV/IC/data/PNAD_COVID_102020.csv") 
replace_C007C <- seq(1,36,1)
replace_values_C007C <- c(9, 9, 4, 4, 5, 5, 5, 5, 9, 5, 7, 6, 9, 9, 8, 9, 9, 7, 7, 7, 5, 8, 8, 2, 2, 2, 3, 5, 5, 5, 5, 2, 1, 2, 3, 99)

#we have three distinct types of wfh
pnad_cov <- pnad_cov %>%
  mutate(C007C = replace_values_C007C[match(pnad_cov$C007C, replace_C007C)],
         C013 = case_when(
           C003 == 1 ~ 1,
           C013 == 1 ~ 1,
           C013 == 2 ~ 0
         ),
         C013_working = case_when(
           C003 == 1 ~ 1,
           C013 == 1 ~ 1,
           C013 == 2 ~ 0,
           C009 > 10 ~ 0
         ),
         C013_occupation = case_when(
           C003 == 1 ~ 1,
           C013 == 1 ~ 1,
           C013 == 2 ~ 0,
           !is.na(C007C) ~ 0
         ))

rm(replace_C007C, replace_values_C007C)


pnad_cov <- pnad_cov[complete.cases(pnad_cov$C013),]


#adjusting weights so to make the datasets compatible
setor_ocup_wfh_rate <- svyby(~C013, ~ C007C + C007D,
                               design = svydesign(ids = ~1, weights = ~V1032, data = pnad_cov[complete.cases(pnad_cov$C013),]), FUN = svymean)

setor_ocup_wfh_rate_occ <- svyby(~C013_occupation, ~ C007C + C007D,
                             design = svydesign(ids = ~1, weights = ~V1032, data = pnad_cov[complete.cases(pnad_cov$C013_occupation),]), FUN = svymean)
setor_ocup_wfh_rate_working <- svyby(~C013_working, ~ C007C + C007D,
                             design = svydesign(ids = ~1, weights = ~V1032, data = pnad_cov[complete.cases(pnad_cov$C013_working),]), FUN = svymean)
setor_ocup_wfh_rate <- merge(setor_ocup_wfh_rate, setor_ocup_wfh_rate_occ, by = c('C007C', 'C007D'))
setor_ocup_wfh_rate <- merge(setor_ocup_wfh_rate, setor_ocup_wfh_rate_working, by = c('C007C', 'C007D'), all.x = T)
  
setor_ocup_wfh_rate <- setor_ocup_wfh_rate %>% 
    rename(setor = C007D,
           ocupacao = C007C) 
rm(setor_ocup_wfh_rate_occ, setor_ocup_wfh_rate_working)
}

#pnad continua - data gathered from API
{

pnadc_dados <- read.csv('D:/Bases de Dados/pnadc_combinada_2012_2023.csv')


pnadc_dados$mes <- ifelse(pnadc_dados$Trimestre == '1', 02,
                          ifelse(pnadc_dados$Trimestre == '2', 05,
                                ifelse(pnadc_dados$Trimestre == '3', 07, 09)))
#we have to adjust the sectors/occupations in order to make both datasets compatible
{
pnadc_dados <- pnadc_dados %>% 
  mutate(ocupacao = substr(V4010,1,1),
         ano_tri = yq(paste(Ano, Trimestre, sep = '-Q')),
         wage = (V403312) / V4039) %>%
  #filter(!is.na(V4010)) %>% 
  mutate(V4013 = case_when(
    V4013 <= 03002 & V4013 != 0 ~ 01,
    V4013 >= 5000 & V4013 <= 9000 ~ 02,
    V4013 >= 10000 & V4013 <= 33002 ~ 03,
    V4013 >= 35010 & V4013 <= 39000 ~ 04,
    V4013 >= 41000 & V4013 <= 43000 ~ 05,
    V4013 >= 45000 & V4013 <= 45040 ~ 07,
    V4013 >= 48000 & V4013 <= 48100 ~ 06,
    V4013 >= 49000 & V4013 <= 51000 ~ 08,
    V4013 >= 50000 & V4013 <= 53002 ~ 10,
    V4013 >= 55000 & V4013 <= 56020 ~ 12,
    V4013 >= 58000 & V4013 <= 63000 ~ 13,
    V4013 >= 64000 & V4013 <= 66002 ~ 14,
    V4013 == 68000 ~ 15,
    V4013 >= 69000 & V4013 <= 75000 ~ 16,
    V4013 >= 77010 & V4013 <= 82009 ~ 17,
    V4013 >= 84000 & V4013 <= 84020 ~ 18,
    V4013 >= 85011 & V4013 <= 85029 ~ 19,
    V4013 >= 86001 & V4013 <= 88000 ~ 20,
    V4013 >= 90000 & V4013 <= 93020 ~ 22, 
    V4013 >= 94010 & V4013 <= 94099 ~ 21,
    V4013 >= 95010 & V4013 <= 96090 ~ 23,
    V4013 == 97000 ~ 24,
    V4013 == 0 ~25,
    TRUE ~ V4013 
  )) %>% 
  rename(setor = V4013) %>%
  group_by(ID_DOMICILIO) %>%
  mutate(young_child = ifelse(any(V2009 < 6), 1, 0)) %>%
  ungroup()
}
#now we can merge data and use the wfh rate in the PNAD Contínua
pnadc_wfh_rate <- merge(pnadc_dados, setor_ocup_wfh_rate, by = c('setor', 'ocupacao'), all.x = T)
pnadc_wfh_rate <- pnadc_wfh_rate %>% 
  rename(age = V2009,
         educ = VD3004) %>% 
  mutate(
    pos_pandemia = case_when(
      Ano <= 2019 ~ 0,
      Ano == 2020 & Trimestre <= 2 ~ 0,
      Ano == 2020 & Trimestre >= 3 ~ 1,
      Ano >= 2021 ~ 1),
    black = ifelse(V2010 == 2 | V2010 == 4, 1, 0),
    rural = ifelse(V1022 == 2, 1, 0),
    formal = case_when(
      VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 ~ 1,
      VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 8 | 
        VD4009 == 9 | VD4009 == 10 ~ 0,
      TRUE ~ NA
    ),
    hours_st = (V4039 - mean(V4039))/sd(V4039),
    wfh_alternative = case_when(
           V4022 == 4 | V4022 == 5 ~ 1,
           V4021 == 1 ~ 0,
           V4022 != 4 & V4022 != 5 & V4022 != NA ~ 0,
           TRUE ~ NA
         ), 
    Ano = factor(Ano),
    mulher = ifelse(V2007 == 2,1,0)) %>% 
  filter(age >= 18 & age <=55)

pnadc_wfh_rate <- pnadc_wfh_rate %>% 
  mutate(wfh_05 = ifelse(C013 > 0.5,1,0),
         wfh_03 = ifelse(C013 > 0.3,1,0),
         wfh_working_03 = ifelse(C013_working > 0.3,1,0),
         wfh_occupation_03 = ifelse(C013_occupation > 0.3,1,0))

#Adjusting wages - this way - 2024 R$
IPCA <- read_excel("C:/Users/Chacha/Desktop/FGV/IC/data/IPCA mensal.xlsx") %>% 
  mutate(data = ym(Data)) %>% 
  arrange(desc(Data)) %>% 
  mutate(IPCA_ajustado = (1 + IPCA_ajustado)^(1/12) - 1,
         IPCA_acc_2024 = cumprod(1 + IPCA_ajustado)) %>% 
  select(data, IPCA_acc_2024) %>% 
  rename(ano_tri = data,
         ipca_acc_2024 = IPCA_acc_2024)

pnadc_wfh_rate <- pnadc_wfh_rate %>% 
  merge(IPCA, by = c("ano_tri"), all.x = T, all.y = T) %>% 
  mutate(real_wage = wage*ipca_acc_2024) 

directory <- "D:/Bases de Dados"
file_path <- file.path(directory, "pnadc_wfh_rate_2012_2023.csv")
pnadc_wfh_rate <- pnadc_wfh_rate[complete.cases(pnadc_wfh_rate$V1028),]
write.csv(pnadc_wfh_rate, file = file_path)
}

#-------now that we treated the datasets, we can get on with our analysis-------

pnadc_wfh_rate <- read.csv('D:/Bases de Dados/pnadc_wfh_rate_2012_2023.csv')
pnadc_wfh_rate <- pnadc_wfh_rate %>% 
  mutate(educ = as.factor(educ),
         race = as.factor(V2010),
         trabalhando = case_when(
           !is.na(V4010) ~ 1,
           TRUE ~ 0
         ))
#note that for all regressions we need to add the weights from the household survey

svy_all <- svydesign(ids = ~1, weights = ~V1028, data = pnadc_wfh_rate)

pnadc_wfh_rate_wage <- pnadc_wfh_rate %>% 
  drop_na(real_wage, C013) %>% 
  mutate(hours_st = (V4039 - mean(V4039))/sd(V4039)) %>% 
  filter(real_wage > quantile(real_wage, 0.05) & real_wage < quantile(real_wage, 0.95))

svy_wage <- svydesign(ids = ~1, weights = ~V1028, data = pnadc_wfh_rate_wage)


#--------------------descriptive statistics ----------

wfh_rate_ocup <- svyby(~C013, by = ~ocupacao, FUN = svymean, design = svydesign(ids = ~1, weights = ~V1028, data = pnadc_wfh_rate %>% 
                                                drop_na(C013, ocupacao))) %>% 
  as.data.frame()
stargazer::stargazer(wfh_rate_ocup[,c(1,2)], summary = F, rownames = NULL)

wfh_rate_mulher <- svyby(~C013, by = ~mulher, FUN = svymean, design = svydesign(ids = ~1, weights = ~V1028, data = pnadc_wfh_rate %>% 
                                                                                  drop_na(C013))) %>% 
  as.data.frame()

wfh <- svymean(~C013, design = svy_wage)

raca_descritiva <- svymean(~race, design = svy_all) %>% 
  as.data.frame() %>%
  rownames_to_column()%>% 
  select(rowname, mean) %>% 
  rename(race = rowname)

filha_peq_descritiva <- svymean(~young_child, design = svy_all) %>% 
  as.data.frame()

educ_descritiva <- svymean(~educ, design = svy_all) %>% 
  as.data.frame() %>% 
  rownames_to_column %>% 
  select(rowname, mean) %>% 
  rename(educ = rowname) 

educ_descritiva <- educ_descritiva %>% 
  filter(educ == 'educ7') %>% 
  rename(rowname = educ)

age_descritiva <- svymean(~age, design = svy_all) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  select(rowname, mean) 

mulher_descritiva <- svymean(~mulher, design = svy_all) %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  select(rowname, mean) 
  
wage_descritiva <- svymean(~real_wage, design = svy_wage) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  select(rowname, mean) 

descritivas <- rbind(age_descritiva, mulher_descritiva) %>% 
  rbind(wage_descritiva) %>% 
  rbind(educ_descritiva)

stargazer::stargazer(descritivas, summary = F, rownames = F, type = 'latex', style = 'qje')


#-------------------------------------------------------

pnadc_wfh_05 <- svyby(~wfh_05, ~ano_tri + ~V2007, design = svy_wage, FUN = svymean)
pnadc_wfh_05 <- pnadc_wfh_05 %>% 
  data.frame(row.names= NULL) %>% 
  mutate(V2007 = ifelse(V2007 == 2, "F", "M"))
pnadc_wfh_05 <- pnadc_wfh_05 %>% 
  mutate(ano_tri = as.Date(ano_tri))

ggplot(aes(x = ano_tri, y = wfh_05, group = V2007, color = V2007), data = pnadc_wfh_05)+
  geom_point()+
  theme_bw()+
  labs(color = "Gender")+
  scale_color_manual(values = c("darkslateblue", "firebrick2"),
                     labels = c("Female", "Male"))+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 19),
        legend.position = 'top',
        legend.direction = "horizontal",
        legend.spacing.x = unit(.2, 'cm'),
        legend.key.size = unit(1.3,'cm'),
        legend.text.align = 0,
        legend.margin = margin(.2))+
  scale_y_continuous(name = "Proportion", labels = percent)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               expand = c(0.02,0.02), name = "Year")

pnadc_wfh_05 <- pnadc_wfh_05 %>% 
  mutate(past2020 = ifelse(ano_tri > as.Date("2019-12-31"),1 , 0),
         mulher = ifelse(V2007 == "F",1,0))
lm(wfh_05 ~ mulher*ano_tri + mulher + ano_tri, data = pnadc_wfh_05 %>% 
     filter(ano_tri > as.Date("2019-12-31"))) %>%
  summary()
#-----------------------participation in sectors with high rate of WFH---------

coef_se_wfh_05 <- data.frame(matrix(nrow = 0, ncol = 0)) 

for (x in seq(2012,2023,1)){
  for(y in c(0,1)){
    k <- glm(wfh_05 ~ mulher + black + age + factor(educ) + rural, 
             data = pnadc_wfh_rate %>% 
               filter(young_child == y) %>% 
               filter(Ano == x),
             weights = V1028) 
    w <- data.frame(coef = coef(k), se = sqrt(diag(vcov(k))), year = x, young_child = y)
    coef_se_wfh_05 <- rbind(coef_se_wfh_05, w)
  }
}
rm(k,w,x,y)

for (x in seq(2012, 2023, 1)){
  k <- glm(wfh_05 ~ mulher + black + age + factor(educ) + rural, 
           data = pnadc_wfh_rate %>% 
             filter(Ano == x),
           weights = V1028) 
  w <- data.frame(coef = coef(k), se = sqrt(diag(vcov(k))), year = x, young_child = 'amostra_completa')
  coef_se_wfh_05 <- rbind(coef_se_wfh_05, w)
}
rm(k,w,x,y)

coef_se_trab <- data.frame(matrix(nrow = 0, ncol = 0))
for (x in seq(2012, 2023, 1)){
  for (y in seq(0,1,1)){
    k <- glm(trabalhando ~ mulher + black + age + factor(educ) + rural, 
             data = pnadc_wfh_rate %>% 
               filter(young_child == y) %>% 
               filter(Ano == x),
             weights = V1028) 
    w <- data.frame(coef = coef(k), se = sqrt(diag(vcov(k))), year = x, young_child = y)
    coef_se_trab <- rbind(coef_se_trab, w)
    
  }
}

for (x in seq(2012, 2023, 1)){
    k <- glm(trabalhando ~ mulher + black + age + factor(educ) + rural, 
             data = pnadc_wfh_rate %>% 
               filter(Ano == x),
             weights = V1028) 
    w <- data.frame(coef = coef(k), se = sqrt(diag(vcov(k))), year = x, young_child = 'amostra_completa')
    coef_se_trab <- rbind(coef_se_trab, w)
}
rm(k,w,x,y)

coef_se_wfh_05 <- coef_se_wfh_05 %>% 
  mutate(independent_var = rownames(.)) %>% 
  data.frame(row.names = NULL) %>% 
  mutate(lower_bound = coef - 1.96 * se,
         upper_bound = coef + 1.96 * se)

coef_se_trab<- coef_se_trab %>% 
  mutate(independent_var = rownames(.)) %>% 
  data.frame(row.names = NULL) %>% 
  mutate(lower_bound = coef - 1.96 * se,
         upper_bound = coef + 1.96 * se)

coef_se_trab <- coef_se_trab %>% 
  mutate(independent_var = gsub("[0-9]", "", independent_var))
coef_se_wfh_05 <- coef_se_wfh_05 %>% 
  mutate(independent_var = gsub("[0-9]", "", independent_var))

coef_se_trab <- coef_se_trab %>% 
  mutate(young_child = as.factor(young_child))
coef_se_wfh_05 <- coef_se_wfh_05 %>% 
  mutate(young_child = as.factor(young_child))

ggplot(data = coef_se_wfh_05 %>% 
         filter(independent_var == "mulher"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(color = 'Young Child at the Household')+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))

ggplot(data = coef_se_wfh_05 %>% 
         filter(independent_var == "mulher") %>% 
         filter(young_child == 1 | young_child == 0),
       aes(x = year, y = coef, color = young_child))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(color = 'Young Child at the Household')+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))+
  scale_color_manual(values = c('blue', 'firebrick'),
                     labels = c('No', 'Yes'))+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = 'top',
        legend.direction = "horizontal",
        legend.spacing.x = unit(.2, 'cm'),
        legend.key.size = unit(1.3,'cm'),
        legend.text.align = 0)

ggplot(data = coef_se_trab %>% 
         filter(independent_var == "mulher"),
       aes(x = year, y = coef, color = young_child))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(color = 'Young Child at the Household')+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012,2023, by = 1))+
  scale_color_manual(values = c('blue', 'firebrick', 'forestgreen'),
                     labels = c('No', 'Yes', 'Full Sample'))+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18),
        legend.position = 'top',
        legend.direction = "horizontal",
        legend.spacing.x = unit(.2, 'cm'),
        legend.key.size = unit(1.3,'cm'),
        legend.text.align = 0)


#---------------------------wage models ----------------------

#novo modelo salário---------
#design = svy_wage - 
#pos_pandemia na interação

wage_reg <- glm(log(real_wage) ~ C013 + mulher + pos_pandemia + C013*mulher + C013*pos_pandemia + mulher*pos_pandemia + C013*mulher*pos_pandemia +
     young_child + hours_st + black + age + I(age^2) + formal + factor(educ) + rural, weights = V1028, data = pnadc_wfh_rate_wage %>% 
       filter(Ano != 2020 & Ano != 2021))

wage_cria_0 <- glm(log(real_wage) ~ C013 + mulher + pos_pandemia + C013*mulher + C013*pos_pandemia + mulher*pos_pandemia + C013*mulher*pos_pandemia +
                     young_child + hours_st + black + age + I(age^2) + factor(educ) + rural, weights = V1028, data = pnadc_wfh_rate_wage %>% 
                     filter(Ano != 2020 & Ano != 2021) %>% 
                     filter(young_child == 0))

wage_cria_1 <- glm(log(real_wage) ~ C013 + mulher + pos_pandemia + C013*mulher + C013*pos_pandemia + mulher*pos_pandemia + C013*mulher*pos_pandemia +
                     young_child + hours_st + black + age + I(age^2) + factor(educ) + rural, weights = V1028, data = pnadc_wfh_rate_wage %>% 
                     filter(Ano != 2020 & Ano != 2021) %>% 
                     filter(young_child == 1))

stargazer::stargazer(wage_reg, wage_cria_0, wage_cria_1, keep = c('C013', 'mulher', 'C013:mulher', 'pos_pandemia', 'mulher:pos_pandemia', 'C013:pos_pandemia', 'C013:mulher:pos_pandemia'), 
                     type = 'latex')
#criança_pequena na interação
wage_child_reg <- svyglm(log_real_wage ~ C013 + mulher + young_child + C013*mulher + C013*young_child + mulher*young_child + C013*mulher*young_child + 
                           hours_st + formal + black + age + age^2 + factor(educ) + rural + factor(Ano), design = z)
summary(wage_child_reg)

#-----------

coef_se_wage_full_sample <- data.frame(matrix(nrow = 0, ncol = 0)) 

for (x in seq(2012,2023,1)){
  z <- subset(svy_wage, subset = Ano == x)
  w <- svyglm(log(real_wage) ~ C013 + mulher + young_child + mulher*young_child + C013*young_child + C013*mulher + mulher*C013*young_child +
                hours_st + formal + black + age + age^2 + factor(educ) + rural, 
              design = z)
  y <- data.frame(coef = coef(w), se = sqrt(diag(vcov(w))), year = x)
  coef_se_wage_full_sample <- rbind(coef_se_wage_full_sample, y)
  rm(k, y, z, w)
}

coef_se_wage_full_sample <- coef_se_wage_full_sample %>% 
  mutate(independent_var = rownames(.)) %>% 
  data.frame(row.names = NULL) %>% 
  mutate(lower_bound = coef - 1.96 * se,
         upper_bound = coef + 1.96 * se)


coef_se_wage_full_sample$independent_var <- ifelse(coef_se_wage_full_sample$year >2012 & coef_se_wage_full_sample$year <=2021, str_sub(coef_se_wage_full_sample$independent_var, end=-2),
                                                   ifelse(coef_se_wage_full_sample$year > 2021, str_sub(coef_se_wage_full_sample$independent_var, end=-3), coef_se_wage_full_sample$independent_var))


ggplot(data = coef_se_wage_full_sample %>% 
         filter(independent_var == "C013:mulher"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "WFH*female coefficient on wage")+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))

ggplot(data = coef_se_wage_full_sample %>% 
         filter(independent_var == "mulher"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "Female coefficient on wage")+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))

ggplot(data = coef_se_wage_full_sample %>% 
         filter(independent_var == "C013"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "WFH coefficient on wage")+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))

ggplot(data = coef_se_wage_full_sample %>% 
         filter(independent_var == "young_child"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "Young child coefficient on wage")+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))

ggplot(data = coef_se_wage_full_sample %>% 
         filter(independent_var == "mulher:young_child"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "Female*Young_child coefficient on wage")+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))

ggplot(data = coef_se_wage_full_sample %>% 
         filter(independent_var == "C013:young_child"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "WFH coefficient on wage")+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))

ggplot(data = coef_se_wage_full_sample %>% 
         filter(independent_var == "C013:mulher:young_child"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "WFH*female*young_child coefficient on wage")+
  xlab("Year")+
  ylab("")+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2012, 2023, by = 1))

#------------------------------ high education sample --------------------
pnad_alta_esc <- pnadc_wfh_rate %>% 
  filter(educ == 6 | educ == 7)

svy_pnadc_alta <- svydesign(ids = ~1, weights = ~V1028, data = pnad_alta_esc)

coef_se_wage_high_educ <- data.frame(matrix(nrow = 0, ncol = 0)) 


for (x in seq(2012,2023,1)){
  z <- subset(svy_pnadc_alta, subset = Ano == x)
  assign(paste0('wage_reg_', x), 
         svyglm(ln(real_wage) ~ C013 + mulher + young_child + C013*mulher + mulher*C013*young_child + 
                  hours_st + formal + black + + age + age^2 + factor(educ) + rural, design = z))
  
  k <- get(paste0('wage_reg_', x))
  y <- data.frame(coef = coef(k), se = sqrt(diag(vcov(k))), year = x)
  coef_se_wage_high_educ <- rbind(coef_se_wage_high_educ, y)
  rm(k, y)
}

coef_se_wage_high_educ <- coef_se_wage_high_educ %>% 
  mutate(independent_var = rownames(.)) %>% 
  data.frame(row.names = NULL) %>% 
  mutate(lower_bound = coef - 1.96 * se,
         upper_bound = coef + 1.96 * se)

coef_se_wage_high_educ$independent_var <- ifelse(coef_se_wage_high_educ$year != 2012, 
                                       str_sub(coef_se_wage_high_educ$independent_var, end=-2), 
                                       coef_se_wage_high_educ$independent_var)
ggplot(data = coef_se_wage_high_educ %>% 
         filter(independent_var == "C013"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "WFH rate coefficient on wage (high education sample)")+
  xlab("Ano")+
  ylab("")+
  scale_y_continuous()

ggplot(data = coef_se_wage_high_educ %>% 
         filter(independent_var == "C013:mulher"),
       aes(x = year, y = coef))+
  geom_point()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1)+
  theme_bw()+
  labs(title = "WFH*female coefficient on wage (high education sample)")+
  xlab("Ano")+
  ylab("")+
  scale_y_continuous()

#---------------------usa vs brasil wfh rate--------------------
wfh_rate_usa <- read_dta("C:/Users/Chacha/Desktop/FGV/IC/data/wfh_rate_usa.dta") %>% 
  rename(ocupacao = occupation,
         setor = work_industry,
         wfh_rate_usa = mean_wfh)
wfh_usa_br <- wfh_rate_usa %>% 
  merge(setor_ocup_wfh_rate, by = c('ocupacao', 'setor'), all.x = T, all.y = T) %>% 
  rename(wfh_rate_br = C013) %>% 
  na.omit() %>% 
  mutate(ocupacao = as.factor(ocupacao))

ggplot(data = wfh_usa_br, aes(x = wfh_rate_usa, y = wfh_rate_br))+ 
  geom_point(colour = 'darkslateblue')+
  theme_bw()+
  xlab('WFH Rate in the US')+
  ylab('WFH Rate in Brazil')+
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = 0.65)+
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.02, 0))+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

lm(wfh_rate_br ~ wfh_rate_usa, data = wfh_usa_br) %>% 
  stargazer::stargazer(type = 'text')

for (x in c(2020,2021,2022,2024)){
  z <- wfh_usa_br %>% 
    filter(as.numeric(year) == x)
  assign(paste0('reg', x), lm(wfh_rate_br ~ wfh_rate_usa, data = z))
}
z <- wfh_usa_br %>% 

stargazer:: stargazer(glm(wfh_rate_br ~ wfh_rate_usa, data = wfh_usa_br, family = binomial(link = "probit")), type = 'latex',
                      title = "WFH rate in Brazil explained by WFH rate in the US by industry and occupation",
                      align = T)
