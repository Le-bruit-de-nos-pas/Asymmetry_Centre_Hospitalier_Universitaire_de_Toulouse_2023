library(readxl)
library(tidyverse)
library(data.table)

# Import files ---------------------------------------------------------------------

sheets_list <- excel_sheets(path = "Raw_Database/Asymmetry_DeepBrainStimulation.xlsx")

#  [1] "DEMOGRAPHIE "             "FACTEURSDERISQUE "        "ATCD_MED_CHIR"           
#  [4] "SOCIAL "                  "PDQ39-CGIS-SCOPA"         "PGI"                     
#  [7] "UPDRS II"                 "UPDRSIII_TOTAUX"          "UPDRSIII_COMPLET_V0_V1"  
# [10] "UPDRSI_II_IV"             "Hoehn&Yarh-S&E"           "EVA_FNM_V0_V1"           
# [13] "HAM-D"                    "HAM-A"                    "TCI_TCSP_V0"             
# [16] "Hallu_Miami"              "MoCA V0"                  "MoCA V1"                 
# [19] "Clox"                     "Boston_Fluence"           "PEROP_COMPLPEROP"        
# [22] "FREQUENCE_V0"             "FREQUENCE_V1"             "EVENEMENTSINDESIRABLES"  
# [25] "CONSO_SPE"                "PSYCHOTROPES"             "AUTRE_PARKINSON"         
# [28] "MEDICAMENTS dans Rapport" "DATES_DE_VISITES "


UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

# Items 3.3–3.8 and 3.15–3.17

df_names <- names(UPDRSIII_COMPLET_V0_V1)




# -------------------------------------------------------------------------------------
# Pre OP OFF -------------------------------------------------------------------------------------

OFF_before <- data.frame(df_names) %>%
  filter(grepl("^OFF_", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names) %>%
  filter(!grepl("1$", df_names))

toString(as.list(OFF_before))

match <- c("OFF_3.15_Left", "OFF_3.15_Right_", "OFF_3.16_Left", "OFF_3.16_Right", "OFF_3.17_Inf_Left_", 
  "OFF_3.17_Inf_Right", "OFF_3.17_Sup_Left_", "OFF_3.17_Sup_Right", "OFF_3.3_Inf_Left", "OFF_3.3_Inf_Right", 
  "OFF_3.3_S_Left", "OFF_3.3_S_Right", "OFF_3.4_Left_", "OFF_3.4_Right_", "OFF_3.5_Left_", "OFF_3.5_Right_", 
  "OFF_3.6_Left_", "OFF_3.6_Right_", "OFF_3.7_Left", "OFF_3.7_Right_", "OFF_3.8_Left", "OFF_3.8_Right_")

match <- append("SUBJID", match)
           

which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFF_before <- UPDRSIII_COMPLET_V0_V1[which_names]
OFF_before <- OFF_before[-1,]

names(OFF_before)

# 33 patients had no data in the OFF pre-OP whatsoever

OFF_before <- data.frame(OFF_before %>% gather(Var, Value, OFF_3.3_S_Right:OFF_3.17_Inf_Left_) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(OFF_before)
 

OFF_before <- data.frame(OFF_before) %>% mutate_each(as.numeric, OFF_3.3_S_Right:OFF_3.17_Inf_Left_)



for(i in 2:23){
  cat(i)
  print(round(mean(OFF_before[,i], na.rm = T),5))
}

# 2[1] 1.63466
# 3[1] 1.66334
# 4[1] 1.40773
# 5[1] 1.43641
# 6[1] 1.9414
# 7[1] 2.09613
# 8[1] 1.70948
# 9[1] 1.7965
# 10[1] 1.73317
# 11[1] 1.85144
# 12[1] 1.96746
# 13[1] 2.16416
# 14[1] 1.48065
# 15[1] 1.64375
# 16[1] 0.56696
# 17[1] 0.54261
# 18[1] 0.29036
# 19[1] 0.3584
# 20[1] 0.70213
# 21[1] 0.67459
# 22[1] 0.43304
# 23[1] 0.40426


dim_desc(OFF_before) # "[802 x 23]"  -> 18446
sum(is.na(OFF_before))# 39  0.00211428   0.2%
drop_na(OFF_before) # 7 pats

Imputed <- imputePCA(OFF_before[,-1],ncp=2, scale = T)

OFF_before <- OFF_before %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(OFF_before[,i], na.rm = T),5))
}

# 2[1] 1.63466
# 3[1] 1.66334
# 4[1] 1.40773
# 5[1] 1.43641
# 6[1] 1.9414
# 7[1] 2.09742
# 8[1] 1.70948
# 9[1] 1.79788
# 10[1] 1.73317
# 11[1] 1.85319
# 12[1] 1.96939
# 13[1] 2.16682
# 14[1] 1.483
# 15[1] 1.64772
# 16[1] 0.56686
# 17[1] 0.54515
# 18[1] 0.29042
# 19[1] 0.36039
# 20[1] 0.70203
# 21[1] 0.67479
# 22[1] 0.43304
# 23[1] 0.40461

sum(is.na(OFF_before))

sum(OFF_before<0)

OFF_before <- data.frame(OFF_before) %>% drop_na() %>% gather(Var, Value, OFF_3.3_S_Right:OFF_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>%  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(OFF_before) %>% drop_na() %>% gather(Var, Value, OFF_3.3_S_Right:OFF_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

OFF_before$Diff <- OFF_before$Right - OFF_before$Left
mean(OFF_before$Diff)

OFF_before %>% ungroup() %>%
  ggplot(aes(abs(Diff))) +
  geom_density() +
  theme_minimal()




# -------------------------------------------------------------------------------------

# Post OP ON OFF -------------------------------------------------------------------------------------


ONOFF_After <- data.frame(df_names) %>%
  filter(grepl("^ONOFF", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names) 

toString(as.list(ONOFF_After))

match <- c("ONOFF_3.15_Left", "ONOFF_3.15_Right_", "ONOFF_3.16_Left", "ONOFF_3.16_Right", "ONOFF_3.17_Inf_Left_", 
           "ONOFF_3.17_Inf_Right", "ONOFF_3.17_Sup_Left_", "ONOFF_3.17_Sup_Right", "ONOFF_3.3_Inf_Left", 
           "ONOFF_3.3_Inf_Right", "ONOFF_3.3_S_Left", "ONOFF_3.3_S_Right", "ONOFF_3.4_Left_", "ONOFF_3.4_Right_",
           "ONOFF_3.5_Left_", "ONOFF_3.5_Right_", "ONOFF_3.6_Left_", "ONOFF_3.6_Right_", "ONOFF_3.7_Left", 
           "ONOFF_3.7_Right_", "ONOFF_3.8_Left", "ONOFF_3.8_Right_")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONOFF_After <- ONOFF_After[-1,]

names(ONOFF_After)

# 291 patients had no data in the ON OFF post-OP whatsoever

ONOFF_After <- data.frame(ONOFF_After %>% gather(Var, Value, ONOFF_3.3_S_Right:ONOFF_3.17_Inf_Left_) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONOFF_After)
 
ONOFF_After <- data.frame(ONOFF_After) %>% mutate_each(as.numeric, ONOFF_3.3_S_Right:ONOFF_3.17_Inf_Left_)




for(i in 2:23){
  cat(i)
  print(round(mean(ONOFF_After[,i], na.rm = T),5))
}

# 2[1] 0.7119
# 3[1] 0.73606
# 4[1] 0.50649
# 5[1] 0.55576
# 6[1] 1.12616
# 7[1] 1.34387
# 8[1] 0.85529
# 9[1] 1.06506
# 10[1] 0.85158
# 11[1] 1.08178
# 12[1] 1.10037
# 13[1] 1.42565
# 14[1] 0.63755
# 15[1] 0.84544
# 16[1] 0.19666
# 17[1] 0.22078
# 18[1] 0.10575
# 19[1] 0.13544
# 20[1] 0.30427
# 21[1] 0.27273
# 22[1] 0.15584
# 23[1] 0.12987


dim_desc(ONOFF_After) # "[545 x 23]"  -> 12535
sum(is.na(ONOFF_After))# 143  0.01140806   0.1%
drop_na(ONOFF_After) # 11 pats

Imputed <- imputePCA(ONOFF_After[,-1],ncp=2, scale = T)

ONOFF_After <- ONOFF_After %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(ONOFF_After[,i], na.rm = T),5))
}

# 2[1] 0.71128
# 3[1] 0.73623
# 4[1] 0.50649
# 5[1] 0.55512
# 6[1] 1.12616
# 7[1] 1.34414
# 8[1] 0.85529
# 9[1] 1.0653
# 10[1] 0.85158
# 11[1] 1.08204
# 12[1] 1.09947
# 13[1] 1.42455
# 14[1] 0.63826
# 15[1] 0.8452
# 16[1] 0.19666
# 17[1] 0.22078
# 18[1] 0.10575
# 19[1] 0.13544
# 20[1] 0.30427
# 21[1] 0.27273
# 22[1] 0.15584
# 23[1] 0.12987


sum(is.na(ONOFF_After))

ONOFF_After <- data.frame(ONOFF_After) %>% drop_na() %>% gather(Var, Value, ONOFF_3.3_S_Right:ONOFF_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(ONOFF_After) %>% drop_na() %>% gather(Var, Value, ONOFF_3.3_S_Right:ONOFF_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

sum(ONOFF_After<0)

ONOFF_After$Diff <- ONOFF_After$Right - ONOFF_After$Left
mean(ONOFF_After$Diff)

# -------------------------------------------------------------------------------------

# Post OP OFF ON -------------------------------------------------------------------------------------


OFFON_After <- data.frame(df_names) %>%
  filter(grepl("^OFFON", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names) 

toString(as.list(OFFON_After))

match <- c("OFFON_3.15_Left", "OFFON_3.15_Right_", "OFFON_3.16_Left", "OFFON_3.16_Right", 
           "OFFON_3.17_Inf_Left_", "OFFON_3.17_Inf_Right", "OFFON_3.17_Sup_Left_",
           "OFFON_3.17_Sup_Right", "OFFON_3.3_Inf_Left", "OFFON_3.3_Inf_Right",
           "OFFON_3.3_S_Left", "OFFON_3.3_S_Right", "OFFON_3.4_Left_", "OFFON_3.4_Right_", 
           "OFFON_3.5_Left_", "OFFON_3.5_Right_", "OFFON_3.6_Left_", 
           "OFFON_3.6_Right_", "OFFON_3.7_Left", "OFFON_3.7_Right_", "OFFON_3.8_Left", "OFFON_3.8_Right_")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFFON_After <- UPDRSIII_COMPLET_V0_V1[which_names]
OFFON_After <- OFFON_After[-1,]

names(OFFON_After)

# 296 patients had no data in the ON OFF post-OP whatsoever

OFFON_After <- data.frame(OFFON_After %>% gather(Var, Value, OFFON_3.3_S_Right:OFFON_3.17_Inf_Left_) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(OFFON_After)
 
OFFON_After <- data.frame(OFFON_After) %>% mutate_each(as.numeric, OFFON_3.3_S_Right:OFFON_3.17_Inf_Left_)




for(i in 2:23){
  cat(i)
  print(round(mean(OFFON_After[,i], na.rm = T),5))
}

# 2[1] 0.62476
# 3[1] 0.65267
# 4[1] 0.46857
# 5[1] 0.50095
# 6[1] 1.0057
# 7[1] 1.20532
# 8[1] 0.79507
# 9[1] 1
# 10[1] 0.7685
# 11[1] 1.01331
# 12[1] 0.93536
# 13[1] 1.29981
# 14[1] 0.54753
# 15[1] 0.73384
# 16[1] 0.15939
# 17[1] 0.24478
# 18[1] 0.10436
# 19[1] 0.18786
# 20[1] 0.22391
# 21[1] 0.27324
# 22[1] 0.08729
# 23[1] 0.08729


dim_desc(OFFON_After) # "[539 x 23]"  -> 12397
sum(is.na(OFFON_After))# 280  0.02258611   0.2%
drop_na(OFFON_After) # 17 pats

Imputed <- imputePCA(OFFON_After[,-1],ncp=2, scale = T)

OFFON_After <- OFFON_After %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(OFFON_After[,i], na.rm = T),5))
}

# 2[1] 0.62515
# 3[1] 0.65271
# 4[1] 0.46895
# 5[1] 0.5013
# 6[1] 1.00633
# 7[1] 1.20477
# 8[1] 0.79507
# 9[1] 0.99945
# 10[1] 0.7685
# 11[1] 1.01272
# 12[1] 0.93522
# 13[1] 1.29981
# 14[1] 0.54648
# 15[1] 0.73264
# 16[1] 0.15939
# 17[1] 0.24478
# 18[1] 0.10436
# 19[1] 0.18786
# 20[1] 0.22391
# 21[1] 0.27324
# 22[1] 0.08729
# 23[1] 0.08729


sum(is.na(OFFON_After))


sum(OFFON_After<0)
OFFON_After[OFFON_After<0] <- 0
sum(OFFON_After<0)

OFFON_After <- data.frame(OFFON_After) %>% drop_na() %>% gather(Var, Value, OFFON_3.3_S_Right:OFFON_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(OFFON_After) %>% drop_na() %>% gather(Var, Value, OFFON_3.3_S_Right:OFFON_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 


OFFON_After$Diff <- OFFON_After$Right - OFFON_After$Left
mean(OFFON_After$Diff)


# -----------------------------------------------------------
# Post OP ON ON -------------------------------------------------------------------------------------


ONON_After <- data.frame(df_names) %>%
  filter(row_number()>272) %>%
  filter(grepl("^ON", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names)  %>%
  filter(!grepl("OFF", df_names)) 


toString(as.list(ONON_After))

match <- c("ON_3.15_Left6", "ON_3.15_Right_6", "ON_3.16_Left6", "ON_3.16_Right6", 
           "ON_3.17_Inf_Left_6", "ON_3.17_Inf_Right6", "ON_3.17_Sup_Left_6", 
           "ON_3.17_Sup_Right6", "ON_3.3_Inf_Left", "ON_3.3_Inf_Right", "ON_3.3_S_Left", 
           "ON_3.3_S_Right", "ON_3.4_Left_", "ON_3.4_Right_", "ON_3.5_Left_", "ON_3.5_Right_", 
           "ON_3.6_Left_", "ON_3.6_Right_", "ON_3.7_Left", "ON_3.7_Right_", "ON_3.8_Left6", "ON_3.8_Right_6")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONON_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONON_After <- ONON_After[-1,]

names(ONON_After)

# 292 patients had no data in the ON OFF post-OP whatsoever

ONON_After <- data.frame(ONON_After %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After)
 
ONON_After <- data.frame(ONON_After) %>% mutate_each(as.numeric, ON_3.3_S_Right:ON_3.17_Inf_Left_6)




for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After[,i], na.rm = T),5))
}

# 2[1] 0.24067
# 3[1] 0.26168
# 4[1] 0.18131
# 5[1] 0.20374
# 6[1] 0.56636
# 7[1] 0.7397
# 8[1] 0.41948
# 9[1] 0.57974
# 10[1] 0.38202
# 11[1] 0.59662
# 12[1] 0.66917
# 13[1] 0.98124
# 14[1] 0.28143
# 15[1] 0.42589
# 16[1] 0.06542
# 17[1] 0.06916
# 18[1] 0.0206
# 19[1] 0.05431
# 20[1] 0.05597
# 21[1] 0.05037
# 22[1] 0.01679
# 23[1] 0.00933


dim_desc(ONON_After) # "[543 x 23]"  -> 12489
sum(is.na(ONON_After))# 189  0.01513332   0.15%
drop_na(ONON_After) # 14 pats

Imputed <- imputePCA(ONON_After[,-1],ncp=2, scale = T)

ONON_After <- ONON_After %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After[,i], na.rm = T),5))
}

# 2[1] 0.24067
# 3[1] 0.2615
# 4[1] 0.18127
# 5[1] 0.20371
# 6[1] 0.56612
# 7[1] 0.73892
# 8[1] 0.41861
# 9[1] 0.57831
# 10[1] 0.38138
# 11[1] 0.59533
# 12[1] 0.66766
# 13[1] 0.98013
# 14[1] 0.28011
# 15[1] 0.42424
# 16[1] 0.06532
# 17[1] 0.06905
# 18[1] 0.02062
# 19[1] 0.05432
# 20[1] 0.05597
# 21[1] 0.05037
# 22[1] 0.01679
# 23[1] 0.00933

sum(is.na(ONON_After))


sum(ONON_After<0)
ONON_After[ONON_After<0] <- 0
sum(ONON_After<0)

ONON_After <- data.frame(ONON_After) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(ONON_After) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 


ONON_After$Diff <- ONON_After$Right - ONON_After$Left
mean(ONON_After$Diff)


# -----------------------------------------------------------
# Post OP OFF OFF -------------------------------------------------------------------------------------


OFFOFF_After <- data.frame(df_names) %>%
  filter(row_number()>272) %>%
  filter(grepl("^OFF", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names)  %>%
  filter(!grepl("ON", df_names)) 


toString(as.list(OFFOFF_After))

match <- c("OFF_3.15_Left1", "OFF_3.15_Right_1", "OFF_3.16_Left1", "OFF_3.16_Right1", "OFF_3.17_Inf_Left_1",
           "OFF_3.17_Inf_Right1", "OFF_3.17_Sup_Left_1", "OFF_3.17_Sup_Right1", "OFF_3.3_Inf_Left1",
           "OFF_3.3_Inf_Right1", "OFF_3.3_S_Left1", "OFF_3.3_S_Right1", "OFF_3.4_Left_1", "OFF_3.4_Right_1", 
           "OFF_3.5_Left_1", "OFF_3.5_Right_1", "OFF_3.6_Left_1", "OFF_3.6_Right_1", "OFF_3.7_Left1", 
           "OFF_3.7_Right_1", "OFF_3.8_Left1", "OFF_3.8_Right_1")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFFOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
OFFOFF_After <- OFFOFF_After[-1,]

names(OFFOFF_After)

# 292 patients had no data in the OFF OFF post-OP whatsoever

OFFOFF_After <- data.frame(OFFOFF_After %>% gather(Var, Value, OFF_3.3_S_Right1:OFF_3.17_Inf_Left_1) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(OFFOFF_After)
 
OFFOFF_After <- data.frame(OFFOFF_After) %>% mutate_each(as.numeric, OFF_3.3_S_Right1:OFF_3.17_Inf_Left_1)




for(i in 2:23){
  cat(i)
  print(round(mean(OFFOFF_After[,i], na.rm = T),5))
}

# 2[1] 1.62127
# 3[1] 1.71963
# 4[1] 1.27799
# 5[1] 1.32523
# 6[1] 2.09328
# 7[1] 2.28598
# 8[1] 1.87873
# 9[1] 2.02991
# 10[1] 1.89366
# 11[1] 2.17196
# 12[1] 1.8729
# 13[1] 2.18352
# 14[1] 1.47477
# 15[1] 1.67416
# 16[1] 0.60075
# 17[1] 0.68097
# 18[1] 0.32463
# 19[1] 0.4403
# 20[1] 0.89739
# 21[1] 0.88619
# 22[1] 0.46269
# 23[1] 0.43843


dim_desc(OFFOFF_After) # "[543 x 23]"  -> 12489
sum(is.na(OFFOFF_After))# 165  0.01321163   
drop_na(OFFOFF_After) # 12 pats

Imputed <- imputePCA(OFFOFF_After[,-1],ncp=2, scale = T)

OFFOFF_After <- OFFOFF_After %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(OFFOFF_After[,i], na.rm = T),5))
}

# 2[1] 1.62127
# 3[1] 1.72049
# 4[1] 1.27798
# 5[1] 1.32517
# 6[1] 2.09328
# 7[1] 2.2872
# 8[1] 1.87873
# 9[1] 2.03105
# 10[1] 1.89365
# 11[1] 2.17333
# 12[1] 1.87247
# 13[1] 2.18541
# 14[1] 1.47492
# 15[1] 1.67432
# 16[1] 0.60075
# 17[1] 0.68097
# 18[1] 0.32463
# 19[1] 0.4403
# 20[1] 0.89739
# 21[1] 0.88619
# 22[1] 0.46269
# 23[1] 0.43843

sum(is.na(OFFOFF_After))


sum(OFFOFF_After<0)

OFFOFF_After <- data.frame(OFFOFF_After) %>% drop_na() %>% gather(Var, Value, OFF_3.3_S_Right1:OFF_3.17_Inf_Left_1) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(OFFOFF_After) %>% drop_na() %>% gather(Var, Value, OFF_3.3_S_Right1:OFF_3.17_Inf_Left_1) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 


OFFOFF_After$Diff <- OFFOFF_After$Right - OFFOFF_After$Left
mean(OFFOFF_After$Diff)


# -----------------------------------------------------------
# Asymmetries pooled together ------------------------------------------------------------------


Asymmetry_Pre_vs_Post <- OFF_before %>% select(SUBJID, Diff) %>% rename("Diff_Pre_OP"="Diff") %>%
  full_join(ONOFF_After %>% select(SUBJID, Diff) %>% rename("Diff_Post_OP_ONOFF"="Diff") ) %>%
  full_join(OFFON_After %>% select(SUBJID, Diff) %>% rename("Diff_Post_OP_OFFON"="Diff") ) %>%
    full_join(ONON_After %>% select(SUBJID, Diff) %>% rename("Diff_Post_OP_ONON"="Diff") ) %>%
      full_join(OFFOFF_After %>% select(SUBJID, Diff) %>% rename("Diff_Post_OP_OFFOFF"="Diff") ) %>%
  drop_na() %>% mutate(Diff_Pre_OP=abs(Diff_Pre_OP),  Diff_Post_OP_ONOFF=abs(Diff_Post_OP_ONOFF),  
                       Diff_Post_OP_OFFON=abs(Diff_Post_OP_OFFON), Diff_Post_OP_ONON=abs(Diff_Post_OP_ONON),
                       Diff_Post_OP_OFFOFF=abs(Diff_Post_OP_OFFOFF))

sum(Asymmetry_Pre_vs_Post<0)

fwrite(Asymmetry_Pre_vs_Post, "Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")

Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")

mean(Asymmetry_Pre_vs_Post$Diff_Pre_OP)  # 5.330728
mean(Asymmetry_Pre_vs_Post$Diff_Post_OP_ONOFF) # 3.865068   ->   Mean Asymmetry in the Post OP is lower, significantly lower
mean(Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFON) # 3.322602   ->   Mean Asymmetry in the Post OP is lower, significantly lower
mean(Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON) # 2.291811   ->   Mean Asymmetry in the Post OP is lower, significantly lower
mean(Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF) # 5.36392   ->   Mean Asymmetry in the Post OP is lower, significantly lower

t.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_ONOFF, Asymmetry_Pre_vs_Post$Diff_Pre_OP, paired = TRUE)


# 	Paired t-test
# 
# data:  Asymmetry_Pre_vs_Post$Diff_Post_OP_ONOFF and Asymmetry_Pre_vs_Post$Diff_Pre_OP
# t = -7.137, df = 536, p-value = 3.115e-12
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -1.869071 -1.062251
# sample estimates:
# mean difference 
#       -1.465661 



Asymmetry_Pre_vs_Post %>% 
  mutate(Diff_Pre_OP=ifelse(Diff_Pre_OP>=5, ">5", "no"))  %>% 
  mutate(Diff_Post_OP_ONON   =ifelse(Diff_Post_OP_ONON   >=5, ">5", "no")) %>%
  group_by(Diff_Pre_OP, Diff_Post_OP_ONON   ) %>% count() %>% ungroup()


#  Diff_Pre_OP Diff_Post_OP_ONOFF     n
# 1 >5          >5                    68
# 2 >5          no                   159
# 3 no          >5                    76
# 4 no          no                   234



temp <- as.matrix(
  Asymmetry_Pre_vs_Post %>% 
  mutate(Diff_Pre_OP=ifelse(Diff_Pre_OP>5, ">5", "no"))  %>% 
  mutate(Diff_Post_OP_ONOFF=ifelse(Diff_Post_OP_ONOFF>5, ">5", "no")) %>%
  group_by(Diff_Pre_OP, Diff_Post_OP_ONOFF) %>% count() %>% ungroup() %>%
    gather(Timepoint, AsymGroup, Diff_Pre_OP:Diff_Post_OP_ONOFF) %>%
    group_by(Timepoint, AsymGroup) %>% summarise(n=sum(n)) %>%
    spread(key=Timepoint, value=n))

matrix(as.numeric(c(temp[1,2], temp[2,2], temp[1,3], temp[2,3])), nrow=2) 

fisher.test( matrix(as.numeric(c(temp[1,2], temp[2,2], temp[1,3], temp[2,3])), nrow=2)  )

# In the Post-OP ONOFF, half as likely to be asymmetric 

# 	Fisher's Exact Test for Count Data
# 
# data:  matrix(as.numeric(c(temp[1, 2], temp[2, 2], temp[1, 3], temp[2, 3])), nrow = 2)
# p-value = 1.308e-07
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  0.3838321 0.6518396
# sample estimates:
# odds ratio 
#  0.5007091 


Asymmetry_Pre_vs_Post %>% select(-Diff_Post_OP_OFFON) %>%
  gather(`Session (Pre/Post OP)`, `Abs R-to-L Asymmetry`, Diff_Pre_OP:Diff_Post_OP_ONOFF) %>%
  mutate(`Session (Pre/Post OP)`=ifelse(`Session (Pre/Post OP)`=="Diff_Post_OP_ONOFF", "Post OP ONOFF", "Pre OP")) %>%
  ggplot(aes(`Abs R-to-L Asymmetry`, colour=`Session (Pre/Post OP)`, fill=`Session (Pre/Post OP)`)) +
  geom_density(alpha=0.7) + 
  theme_minimal() +
  ggsci::scale_fill_uchicago() +
  ggsci::scale_colour_uchicago() +
  xlab("\n `Abs R-to-L Asymmetry`") + ylab("Patient Kernel Density Estimate (KDE) \n")


Asymmetry_Pre_vs_Post %>% select(-Diff_Post_OP_ONOFF) %>%
  gather(`Session (Pre/Post OP)`, `Abs R-to-L Asymmetry`, Diff_Pre_OP:Diff_Post_OP_OFFON) %>%
  mutate(`Session (Pre/Post OP)`=ifelse(`Session (Pre/Post OP)`=="Diff_Post_OP_OFFON", "Post OP OFFON", "Pre OP")) %>%
  ggplot(aes(`Abs R-to-L Asymmetry`, colour=`Session (Pre/Post OP)`, fill=`Session (Pre/Post OP)`)) +
  geom_density(alpha=0.7) + 
  theme_minimal() +
  ggsci::scale_fill_uchicago() +
  ggsci::scale_colour_uchicago() +
  xlab("\n `Abs R-to-L Asymmetry`") + ylab("Patient Kernel Density Estimate (KDE) \n")

  
# -------------------------------------------------------------------------------------

# Compare Asymmetries Friedman/Prop test --------------------------------------

Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")

Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% gather(Evaluation, Diff, Diff_Pre_OP:Diff_Post_OP_OFFOFF)

unique(Asymmetry_Pre_vs_Post$Evaluation)

Asymmetry_Pre_vs_Post %>%
  mutate(Evaluation=ifelse(Evaluation=="Diff_Pre_OP", "OFF_[Pre-OP]",
                           ifelse(Evaluation=="Diff_Post_OP_OFFOFF", "OFF_OFF_[Post-OP]",
                                  ifelse(Evaluation=="Diff_Post_OP_ONOFF", "ON-DBS_OFF-Med_[Post-OP]",
                                         ifelse(Evaluation=="Diff_Post_OP_OFFON", "OFF-DBS_ON-Med_[Post-OP]", "ON_ON_[Post-OP]"))))) %>%
  mutate(Evaluation=factor(Evaluation, levels=c("OFF_[Pre-OP]" , 
                                                "OFF_OFF_[Post-OP]", 
                                                "ON-DBS_OFF-Med_[Post-OP]",
                                                "OFF-DBS_ON-Med_[Post-OP]" , 
                                                "ON_ON_[Post-OP]"))) %>%
  ggplot(aes(Evaluation, Diff, colour=Evaluation, fill=Evaluation )) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.3, notch = TRUE, notchwidth = 0.3, varwidth = T, show.legend = FALSE) +
  geom_jitter(width=0.2, height = 0.6, alpha=0.6, show.legend = FALSE) +
  theme_minimal() +
  xlab("\n") + ylab("Absolute R-to-L Difference \n (i.e. Asymmetry) \n") +
  scale_fill_brewer(palette="PuBu") +
  scale_colour_brewer(palette="PuBu") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




friedman.test(y=Asymmetry_Pre_vs_Post$Diff, groups=Asymmetry_Pre_vs_Post$Evaluation, blocks=Asymmetry_Pre_vs_Post$SUBJID)


# 	Friedman rank sum test
# 
# data:  Asymmetry_Pre_vs_Post$Diff, Asymmetry_Pre_vs_Post$Evaluation and Asymmetry_Pre_vs_Post$SUBJID
# Friedman chi-squared = 323.35, df = 4, p-value < 2.2e-16



pairwise.wilcox.test(Asymmetry_Pre_vs_Post$Diff, Asymmetry_Pre_vs_Post$Evaluation, p.adj = "bonferroni", paired=T)


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  Asymmetry_Pre_vs_Post$Diff and Asymmetry_Pre_vs_Post$Evaluation 
# 
#                    Diff_Post_OP_OFFOFF Diff_Post_OP_OFFON Diff_Post_OP_ONOFF Diff_Post_OP_ONON
# Diff_Post_OP_OFFON < 2e-16             -                  -                  -                
# Diff_Post_OP_ONOFF < 2e-16             0.001              -                  -                
# Diff_Post_OP_ONON  < 2e-16             3.6e-14            < 2e-16            -                
# Diff_Pre_OP        1.000               < 2e-16            2.9e-11            < 2e-16          
# 
# P value adjustment method: bonferroni 





temp <- ConoverTest(Asymmetry_Pre_vs_Post$Diff, Asymmetry_Pre_vs_Post$Evaluation, method ="bonferroni",  out.list = F)


#  Conover's test of multiple comparisons : bonferroni  
# 
#                                        mean.rank.diff    pval    
# Diff_Post_OP_OFFON-Diff_Post_OP_OFFOFF     -399.60335 < 2e-16 ***
# Diff_Post_OP_ONOFF-Diff_Post_OP_OFFOFF     -267.22812 2.1e-08 ***
# Diff_Post_OP_ONON-Diff_Post_OP_OFFOFF      -653.02700 < 2e-16 ***
# Diff_Pre_OP-Diff_Post_OP_OFFOFF              17.26071  1.0000    
# Diff_Post_OP_ONOFF-Diff_Post_OP_OFFON       132.37523  0.0293 *  
# Diff_Post_OP_ONON-Diff_Post_OP_OFFON       -253.42365 1.3e-07 ***
# Diff_Pre_OP-Diff_Post_OP_OFFON              416.86406 < 2e-16 ***
# Diff_Post_OP_ONON-Diff_Post_OP_ONOFF       -385.79888 < 2e-16 ***
# Diff_Pre_OP-Diff_Post_OP_ONOFF              284.48883 1.8e-09 ***
# Diff_Pre_OP-Diff_Post_OP_ONON               670.28771 < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 
 
#                     Diff_Post_OP_OFFOFF Diff_Post_OP_OFFON Diff_Post_OP_ONOFF Diff_Post_OP_ONON
# Diff_Post_OP_OFFON < 2e-16             -                  -                  -                
# Diff_Post_OP_ONOFF 2.1e-08             0.029              -                  -                
# Diff_Post_OP_ONON  < 2e-16             1.3e-07            < 2e-16            -                
# Diff_Pre_OP        1.000               < 2e-16            1.8e-09            < 2e-16          



Conover_Test <- fread("Figures/Conover_Test.csv")

Conover_Test %>%
  arrange(G1, G2) %>%
  mutate(G1=factor(G1, levels=c("OFF-DBS_ON-Med_[Post-OP]", "ON-DBS_OFF-Med_[Post-OP]", "ON_ON_[Post-OP]", "OFF_[Pre-OP]"))) %>%
  mutate(G2=factor(G2, levels=c("OFF_OFF_[Post-OP]", "OFF-DBS_ON-Med_[Post-OP]", "ON-DBS_OFF-Med_[Post-OP]", "ON_ON_[Post-OP]"))) %>%
      mutate(G2=as.factor(G2)) %>%
  mutate(RankDiff=as.numeric(RankDiff)) %>%
  mutate(pvalue=as.numeric(pvalue)) %>%
    ggplot(aes(x=G1 , y=G2, fill = RankDiff)) + 
  geom_tile(color = "white", size = 0.1) + 
  geom_text(aes(label=pvalue, fontface = "bold")) +
  scale_fill_distiller(palette = "RdBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("\n From (baseline)") +ylab("To (compared against)\n") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


All <- Asymmetry_Pre_vs_Post %>%
  mutate(Diff=ifelse(Diff>=5, ">5", "no"))  %>% 
  group_by(Evaluation) %>% count() %>% ungroup() 

Pos <- Asymmetry_Pre_vs_Post %>%
  mutate(Diff=ifelse(Diff>=5, ">5", "no"))  %>% 
  filter(Diff==">5") %>% 
  group_by(Evaluation) %>% count() %>% ungroup() 
names(Pos)[2] <- "Asym"


Props <- All %>% left_join(Pos)

#   Evaluation              n  Asym
#   <chr>               <int> <int>
# 1 Diff_Post_OP_OFFOFF   537   268
# 2 Diff_Post_OP_OFFON    537   138
# 3 Diff_Post_OP_ONOFF    537   187
# 4 Diff_Post_OP_ONON     537    66
# 5 Diff_Pre_OP           537   275

pairwise.prop.test(x=Props$Asym,n=Props$n,p.adjust.method="bonferroni",alternative="two.sided", correct = FALSE)

	Pairwise comparisons using Pairwise comparison of proportions 

# data:  Props$Asym out of Props$n 
# 
#   1       2       3       4      
# 2 2.8e-15 -       -       -      
# 3 5.7e-06 0.011   -       -      
# 4 < 2e-16 2.1e-07 < 2e-16 -      
# 5 1.000   < 2e-16 5.8e-07 < 2e-16
# 
# P value adjustment method: bonferroni 

Prop_Test <- fread("Figures/Prop_Test.csv")

	
Prop_Test %>%
  arrange(G1, G2) %>%
  mutate(G1=factor(G1, levels=c("OFF-DBS_ON-Med_[Post-OP]", "ON-DBS_OFF-Med_[Post-OP]", "ON_ON_[Post-OP]", "OFF_[Pre-OP]"))) %>%
  mutate(G2=factor(G2, levels=c("OFF_OFF_[Post-OP]", "OFF-DBS_ON-Med_[Post-OP]", "ON-DBS_OFF-Med_[Post-OP]", "ON_ON_[Post-OP]"))) %>%
      mutate(G2=as.factor(G2)) %>%
  mutate(RankDiff=as.numeric(Prop_Diff)) %>%
  mutate(pvalue=as.numeric(pvalue)) %>%
    ggplot(aes(x=G1 , y=G2, fill = Prop_Diff)) + 
  geom_tile(color = "white", size = 0.1) + 
  geom_text(aes(label=pvalue, fontface = "bold")) +
  scale_fill_distiller(palette = "RdBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("\n From (baseline)") +ylab("To (compared against)\n") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

	

# --------------------------------------------------
# Extract (demographics, medication, stimulation, disease-related, etc etc) ----------

sheets_list <- excel_sheets(path = "Raw_Database/Asymmetry_DeepBrainStimulation.xlsx")

#  [1] "DEMOGRAPHIE "             "FACTEURSDERISQUE "        "ATCD_MED_CHIR"           
#  [4] "SOCIAL "                  "PDQ39-CGIS-SCOPA"         "PGI"                     
#  [7] "UPDRS II"                 "UPDRSIII_TOTAUX"          "UPDRSIII_COMPLET_V0_V1"  
# [10] "UPDRSI_II_IV"             "Hoehn&Yarh-S&E"           "EVA_FNM_V0_V1"           
# [13] "HAM-D"                    "HAM-A"                    "TCI_TCSP_V0"             
# [16] "Hallu_Miami"              "MoCA V0"                  "MoCA V1"                 
# [19] "Clox"                     "Boston_Fluence"           "PEROP_COMPLPEROP"        
# [22] "FREQUENCE_V0"             "FREQUENCE_V1"             "EVENEMENTSINDESIRABLES"  
# [25] "CONSO_SPE"                "PSYCHOTROPES"             "AUTRE_PARKINSON"         
# [28] "MEDICAMENTS dans Rapport" "DATES_DE_VISITES "


DEMOGRAPHIE <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "DEMOGRAPHIE ", skip=0, col_types = "text", trim_ws = TRUE)
PDQ39_CGIS_SCOPA <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "PDQ39-CGIS-SCOPA", skip=0, col_types = "text", trim_ws = TRUE)
UPDRSI_II_IV <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSI_II_IV", skip=0, col_types = "text", trim_ws = TRUE)
UPDRSI_II <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRS II", skip=0, col_types = "text", trim_ws = TRUE)
FREQUENCE_V0 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "FREQUENCE_V0", skip=0, col_types = "text", trim_ws = TRUE)

names(PDQ39_CGIS_SCOPA)
PDQ39_CGIS_SCOPA <- PDQ39_CGIS_SCOPA %>% select(-c("...48", "CGIS" , "CGIS_NR", "SEVER_MALADIE" , "...52" ))
PDQ39_CGIS_SCOPA <- PDQ39_CGIS_SCOPA %>% select(-c("NUM_CENTRE", "NUM_PAT", "INIT_PAT", "SITUATION_MARITALE", "REF_SITU_FAM"))

PDQ39 <- PDQ39_CGIS_SCOPA[,c(1,2, 3:42)]
SCOPA <- PDQ39_CGIS_SCOPA[,c(1,2, 43:53)]
PDQ39 <- PDQ39[-1,]
SCOPA <- SCOPA[-1,]


PDQ39 <- PDQ39 %>% group_by(SUBJID) %>% mutate(VISIT=row_number()-1) %>% ungroup()
SCOPA <- SCOPA %>% group_by(SUBJID) %>% mutate(VISIT=row_number()-1) %>% ungroup()

data.frame(PDQ39 %>% gather(item, value, PDQ39_1:PDQ39_SCORE) %>%
  select(value) %>% distinct())

# 1                           Rarement  - 1
# 2                               <NA>
# 3                            Parfois - 2
# 4   Toujours ou ne peut jamais faire  -4 
# 5                            Souvent  - 3
# 6                             Jamais  - 0


PDQ39 <- PDQ39 %>% gather(item, value, PDQ39_1:PDQ39_SCORE) %>%
  mutate(value=ifelse(value=="Rarement", 1,
                      ifelse(value=="Parfois", 2,
                             ifelse(value=="Toujours ou ne peut jamais faire", 4,
                                    ifelse(value=="Souvent", 3,
                                           ifelse(value=="Jamais", 0, value))))))


PDQ39 %>% filter(item!="PDQ39_SCORE") %>%
  mutate(value=as.numeric(value)) %>%
  group_by(SUBJID, VISIT) %>% summarise(n=sum(value, na.rm=T)) %>%
  left_join(
PDQ39 %>% filter(item=="PDQ39_SCORE") %>%
  select(SUBJID, VISIT, value) %>%
  mutate(value=as.numeric(value))
) %>%
  filter(n!=value) # all good 


fwrite(PDQ39, "Processed_data/PDQ39.txt")


names(UPDRSI_II)
UPDRSI_II <- data.frame(UPDRSI_II %>% drop_na())

UPDRSI_II <- UPDRSI_II %>% group_by(SUBJID) %>% mutate(VISIT=row_number()-1) %>% ungroup()

UPDRSI_II <- UPDRSI_II[-1,]

data.frame(UPDRSI_II %>% gather(item, value, MDS2_1ON:MDS2_13OFF) %>%
  select(value) %>% distinct())

# 1  Minime - 1
# 2   Léger  - 2
# 3  Normal - 0
# 4  Sévère - 4
# 5  Modéré - 3

UPDRSI_II[UPDRSI_II=="Normal"] <- "0"
UPDRSI_II[UPDRSI_II=="Minime"] <- "1"
UPDRSI_II[UPDRSI_II=="Léger"] <- "2"
UPDRSI_II[UPDRSI_II=="Modéré"] <- "3"
UPDRSI_II[UPDRSI_II=="Sévère"] <- "4"

UPDRSI_II <- UPDRSI_II[,-c(16,30)]

UPDRSI_II <- UPDRSI_II %>% gather(item, value, MDS2_1ON:MDS2_13OFF) %>%
  mutate(value=as.numeric(value))

UPDRSI_II <- UPDRSI_II %>% arrange(SUBJID, VISIT, item) %>%
  spread(key=item, value=value) %>% arrange(SUBJID)


dim_desc(UPDRSI_II) # ""[580 x 28]"  -> 16240
sum(is.na(UPDRSI_II))# 951 
drop_na(UPDRSI_II) # 70 evals

Imputed <- imputePCA(UPDRSI_II[,-c(1,2)],ncp=2, scale = T)

UPDRSI_II <- UPDRSI_II %>% select(SUBJID, VISIT) %>% bind_cols(Imputed$completeObs)

names(UPDRSI_II)
sum(UPDRSI_II<0)

UPDRSI_II %>% gather(item, value, MDS_2_4ON:MDS2_9ON) %>%
  mutate(state=ifelse(grepl("ON", item), "ON", "OFF")) %>%
  group_by(SUBJID, VISIT, state) %>% 
  mutate(value=sum(value)) %>%
  ungroup() %>%
  group_by(VISIT, state) %>% summarise(n=mean(value))


fwrite(UPDRSI_II, "Processed_data/UPDRSI_II.txt")

# ------------------------------------------------------
# UPDRS II -------------------------------------------------

UPDRSI_II <- fread("Processed_data/UPDRSI_II.txt")

UPDRSI_II <- UPDRSI_II %>% gather(item, value, MDS_2_4ON:MDS2_9ON) %>%
   mutate(state=ifelse(grepl("ON", item), "ON", "OFF")) %>%
   group_by(SUBJID, VISIT, state) %>% 
   summarise(value=sum(value)) %>%
   ungroup() %>%
  mutate(VISIT=ifelse(VISIT==0, "Pre_OP", "Post_OP")) 


# Removing patients who did not have the 4 conditions - 226 left

UPDRSI_II <- UPDRSI_II %>% filter(VISIT=="Pre_OP"&state=="OFF") %>%
  select(SUBJID, value) %>% rename("Pre_OP_[OFF]"="value") %>%
  full_join(
    UPDRSI_II %>% filter(VISIT=="Pre_OP"&state=="ON") %>%
  select(SUBJID, value) %>% rename("Pre_OP_[ON]"="value") 
  )  %>%
  full_join(
    UPDRSI_II %>% filter(VISIT=="Post_OP"&state=="OFF") %>%
  select(SUBJID, value) %>% rename("Post_OP_[OFF]"="value") 
  )  %>%
  full_join(
    UPDRSI_II %>% filter(VISIT=="Post_OP"&state=="ON") %>%
  select(SUBJID, value) %>% rename("Post_OP_[ON]"="value") 
  ) %>% drop_na()

mean(UPDRSI_II$`Pre_OP_[OFF]`) #19.40816
mean(UPDRSI_II$`Pre_OP_[ON]`) #6.513844
mean(UPDRSI_II$`Post_OP_[OFF]`) #15.9354
mean(UPDRSI_II$`Post_OP_[ON]`) #7.456636

UPDRSI_II %>% gather(item, value, `Pre_OP_[OFF]`:`Post_OP_[ON]`) %>%
   mutate(item=factor(item, levels=c("Pre_OP_[OFF]" , "Pre_OP_[ON]", 
                                                "Post_OP_[OFF]", "Post_OP_[ON]"))) %>%
  ggplot(aes(item, value, colour=item, fill=item )) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.3, notch = TRUE, notchwidth = 0.3, varwidth = T, show.legend = FALSE) +
  geom_jitter(width=0.2, height = 0.6, alpha=0.6, show.legend = FALSE) +
  theme_minimal() +
  xlab("\n") + ylab("UPDRS II \n") +
  scale_fill_manual(values=c("#E9EDC9", "#B5838D", "#0081A7", "#0F4C5C")) +
  scale_colour_manual(values=c("#E9EDC9", "#B5838D", "#0081A7" , "#0F4C5C")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


UPDRSI_II_temp <- UPDRSI_II %>% gather(item, value, `Pre_OP_[OFF]`:`Post_OP_[ON]`)

friedman.test(y=UPDRSI_II_temp$value, groups=UPDRSI_II_temp$item, blocks=UPDRSI_II_temp$SUBJID)

# 	Friedman rank sum test
# 
# data:  UPDRSI_II_temp$value, UPDRSI_II_temp$item and UPDRSI_II_temp$SUBJID
# Friedman chi-squared = 446.07, df = 3, p-value < 2.2e-16


temp <- ConoverTest(UPDRSI_II_temp$value, UPDRSI_II_temp$item, method ="bonferroni",  out.list = F)


#  Conover's test of multiple comparisons : bonferroni  
# 
#              Post_OP_[OFF] Post_OP_[ON] Pre_OP_[OFF]
# Post_OP_[ON] < 2e-16       -            -           
# Pre_OP_[OFF] 5.5e-06       < 2e-16      -           
# Pre_OP_[ON]  < 2e-16       0.18         < 2e-16    


Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")
Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% select(1,5,6)

mean(Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON-Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF)

Asymmetry_Pre_vs_Post$Diff <- Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON-Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF

UPDRSI_II <- UPDRSI_II %>% select(1, 4,5)

mean(UPDRSI_II$`Post_OP_[ON]`-UPDRSI_II$`Post_OP_[OFF]`)

UPDRSI_II$Diff2 <- UPDRSI_II$`Post_OP_[ON]`-UPDRSI_II$`Post_OP_[OFF]`

mean(UPDRSI_II$`Post_OP_[OFF]`)

temp <- Asymmetry_Pre_vs_Post %>% select(1,4) %>%
  inner_join(UPDRSI_II %>% select(1,4)) 

cor.test(temp$Diff, temp$Diff2)

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff and temp$Diff2
# t = -1.6358, df = 222, p-value = 0.1033
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.23682872  0.02226751
# sample estimates:
#        cor 
# -0.1091339 
  

UPDRSI_II %>% 
  inner_join(Asymmetry_Pre_vs_Post) %>%
  ggplot(aes(Diff_Post_OP_ONON, `Post_OP_[ON]`)) +
  geom_jitter(width = 0.5, height = 0.3, alpha=1,  shape=4, colour="deepskyblue4") +
  xlim(-2,22) + ylim(-2,44) +
  theme_minimal() +
  xlab("\n Abs R-to-L Asymmetry \n Post-OP [ON-ON] ") + 
  ylab("UPDRS II \n Post-OP [ON-ON] \n")
  


temp <- UPDRSI_II %>% select(1,2) %>%
  inner_join(Asymmetry_Pre_vs_Post %>% select(1,3))

#cor.test(temp$`Post_OP_[OFF]`, temp$Diff_Post_OP_OFFOFF)



temp <- temp %>% mutate(Diff_Post_OP_OFFOFF=ifelse(Diff_Post_OP_OFFOFF>=5,">5", "no")) 

wilcox.test(temp$`Post_OP_[OFF]`[temp$Diff_Post_OP_OFFOFF==">5"], temp$`Post_OP_[OFF]`[temp$Diff_Post_OP_OFFOFF!=">5"])

temp %>% mutate(Diff_Post_OP_OFFOFF=ifelse(Diff_Post_OP_OFFOFF>=5,">5", "no")) %>%
  group_by(Diff_Post_OP_OFFOFF) %>% summarise(n=mean(`Post_OP_[OFF]`))

# 1 >5                   16.9
# 2 no                   14.7
# 
# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$`Post_OP_[OFF]`[temp$Diff_Post_OP_OFFOFF == ">5"] and temp$`Post_OP_[OFF]`[temp$Diff_Post_OP_OFFOFF != ">5"]
# W = 5098.5, p-value = 0.017
# alternative hypothesis: true location shift is not equal to 0


temp <- UPDRSI_II %>% select(1,3) %>%
  inner_join(Asymmetry_Pre_vs_Post %>% select(1,2))

temp <- temp %>% mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5,">5", "no")) 

wilcox.test(temp$`Post_OP_[ON]`[temp$Diff_Post_OP_ONON==">5"], temp$`Post_OP_[ON]`[temp$Diff_Post_OP_ONON!=">5"])

temp %>% mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5,">5", "no")) %>%
  group_by(Diff_Post_OP_ONON) %>% summarise(n=mean(`Post_OP_[ON]`))

#  Diff_Post_OP_ONON     n
#   <chr>             <dbl>
# 1 >5                 7.34
# 2 no                 7.21

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$`Post_OP_[ON]`[temp$Diff_Post_OP_ONON == ">5"] and temp$`Post_OP_[ON]`[temp$Diff_Post_OP_ONON != ">5"]
# W = 2801, p-value = 0.8599
# alternative hypothesis: true location shift is not equal to 0

# -------------------
# PDQ39 --------------------------------------------------

PDQ39 <- fread("Processed_data/PDQ39.txt")

PDQ39 <- PDQ39 %>% arrange(SUBJID, VISIT, item) %>%
  spread(key=item, value=value) %>% arrange(SUBJID)

PDQ39 %>% group_by(VISIT) %>% summarise(n=mean(as.numeric(PDQ39_SCORE)))

PDQ39 <- PDQ39 %>% filter(PDQ39_SCORE!=0)
PDQ39


Imputed <- imputePCA(PDQ39[,-c(1,2)],ncp=2, scale = T)

PDQ39 <- PDQ39 %>% select(SUBJID, VISIT) %>% bind_cols(Imputed$completeObs)

names(PDQ39)

sum(PDQ39<0)

PDQ39[PDQ39<0] <- 0

PDQ39 %>% group_by(VISIT) %>% summarise(n=mean(as.numeric(PDQ39_SCORE)))


PDQ39 <- PDQ39 %>% select(-PDQ39_SCORE) %>%
  gather(item, value, PDQ39_1 :PDQ39_9) %>%
   group_by(SUBJID, VISIT) %>% 
   summarise(value=sum(value)) %>%
   ungroup() %>%
  mutate(VISIT=ifelse(VISIT==0, "Pre_OP", "Post_OP")) 

PDQ39 <- PDQ39 %>% group_by(SUBJID) %>% count() %>%
  filter(n>1) %>% select(SUBJID) %>%
  left_join(PDQ39)

PDQ39 %>% group_by(VISIT) %>% summarise(n=mean(as.numeric(value)))

# 1 Post_OP  44.4
# 2 Pre_OP   51.9

temp <- PDQ39 %>% filter(VISIT=="Pre_OP") %>% arrange(SUBJID) %>% rename("PDQ39_Pre"="value") %>% select(-VISIT) %>%
  left_join(PDQ39 %>% filter(VISIT=="Post_OP") %>% arrange(SUBJID) %>% rename("PDQ39_Post"="value") %>% select(-VISIT))

wilcox.test(temp$PDQ39_Pre, temp$PDQ39_Post, paired=TRUE, conf.int=TRUE)



# 	Wilcoxon signed rank test with continuity correction
# 
# data:  temp$PDQ39_Pre and temp$PDQ39_Post
# V = 99948, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  5.999986 9.011151
# sample estimates:
# (pseudo)median 
#        7.50004 
       

PDQ39 %>%
     mutate(VISIT=factor(VISIT, levels=c("Pre_OP" , "Post_OP"))) %>%
  ggplot(aes(VISIT, value, colour=VISIT, fill=VISIT )) +
  geom_violin(alpha=0.5, show.legend = FALSE) +
  geom_boxplot(alpha=0.5, notch = TRUE, notchwidth = 0.3, varwidth = T, show.legend = FALSE) +
  geom_jitter(width=0.2, height = 0.6, alpha=0.6, show.legend = FALSE) +
  theme_minimal() +
  xlab("\n") + ylab("PDQ39\n") +
  scale_fill_manual(values=c("#B5838D", "#0081A7")) +
  scale_colour_manual(values=c("#B5838D", "#0081A7")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")
Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% select(1,5,6)

Asymmetry_Pre_vs_Post <-  temp %>% inner_join(Asymmetry_Pre_vs_Post)

cor.test(Asymmetry_Pre_vs_Post$PDQ39_Post, Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF)

cor.test(Asymmetry_Pre_vs_Post$PDQ39_Post, Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON)

# 	Pearson's product-moment correlation
# 
# data:  Asymmetry_Pre_vs_Post$PDQ39_Post and Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON
# t = 1.6769, df = 518, p-value = 0.09417
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.0125875  0.1584633
# sample estimates:
#        cor 
# 0.07347828 

summary(lm(PDQ39_Post~Diff_Post_OP_ONON, data=Asymmetry_Pre_vs_Post))

Asymmetry_Pre_vs_Post %>%
  ggplot(aes(Diff_Post_OP_ONON , `PDQ39_Post`)) +
  geom_jitter(width = 0.5, height = 0.3, alpha=1,  shape=4, colour="deepskyblue4") +
 # xlim(-2,22) +   ylim(-2,44) +
  theme_minimal() +
  xlab("\n Abs R-to-L Asymmetry \n Post-OP [ON-ON] ") + 
  ylab("PDQ39 \n Post-OP\n")
  
# ---------------------------

# PDQ39 sub-scores --------------------------------
PDQ39 <- fread("Processed_data/PDQ39.txt")

PDQ39 <- PDQ39 %>% arrange(SUBJID, VISIT, item) %>%
  spread(key=item, value=value) %>% arrange(SUBJID)

PDQ39 %>% group_by(VISIT) %>% summarise(n=mean(as.numeric(PDQ39_SCORE)))

PDQ39 <- PDQ39 %>% filter(PDQ39_SCORE!=0)

Imputed <- imputePCA(PDQ39[,-c(1,2)],ncp=2, scale = T)

PDQ39 <- PDQ39 %>% select(SUBJID, VISIT) %>% bind_cols(Imputed$completeObs)

names(PDQ39)

sum(PDQ39<0)

PDQ39[PDQ39<0] <- 0

PDQ39 %>% group_by(VISIT) %>% summarise(n=mean(as.numeric(PDQ39_SCORE)))

PDQ39 <- PDQ39 %>% select(-PDQ39_SCORE) 

PDQ39 <- PDQ39 %>% group_by(SUBJID) %>% count() %>% filter(n>1) %>% 
  select(SUBJID) %>% ungroup() %>% left_join(PDQ39)

names(PDQ39)

sum(is.na(PDQ39))

PDQ39$Mobility <- PDQ39$PDQ39_1 + PDQ39$PDQ39_2 + PDQ39$PDQ39_3 + PDQ39$PDQ39_4 + PDQ39$PDQ39_5 + 
  PDQ39$PDQ39_6 + PDQ39$PDQ39_7 + PDQ39$PDQ39_8 + PDQ39$PDQ39_9 + PDQ39$PDQ39_10 

PDQ39$DailyLiving <- PDQ39$PDQ39_11 + PDQ39$PDQ39_12 + PDQ39$PDQ39_13 + PDQ39$PDQ39_14 + PDQ39$PDQ39_15 + PDQ39$PDQ39_16 

PDQ39$Emotional <- PDQ39$PDQ39_17 + PDQ39$PDQ39_18 + PDQ39$PDQ39_19 + PDQ39$PDQ39_20 + PDQ39$PDQ39_21 + PDQ39$PDQ39_22 

PDQ39$Stigma <- PDQ39$PDQ39_23 + PDQ39$PDQ39_24 + PDQ39$PDQ39_25 + PDQ39$PDQ39_26 

PDQ39$SocialSupport <- PDQ39$PDQ39_27 + PDQ39$PDQ39_28 + PDQ39$PDQ39_29 

PDQ39$Cognition <- PDQ39$PDQ39_30 + PDQ39$PDQ39_31 + PDQ39$PDQ39_32 + PDQ39$PDQ39_33 

PDQ39$Communication <- PDQ39$PDQ39_34 + PDQ39$PDQ39_35 + PDQ39$PDQ39_36 

PDQ39$BodilyDiscomfort <- PDQ39$PDQ39_37 + PDQ39$PDQ39_38 + PDQ39$PDQ39_39 

PDQ39 <- PDQ39 %>% select(SUBJID, VISIT, Mobility, DailyLiving, Emotional, Stigma, SocialSupport, Cognition, Communication, BodilyDiscomfort)

PDQ39 %>% group_by(VISIT) %>% summarise(Mobility=mean(Mobility), DailyLiving=mean(DailyLiving), 
                                        Emotional=mean(Emotional), Stigma=mean(Stigma), SocialSupport=mean(SocialSupport),
                                        Cognition=mean(Cognition), Communication=mean(Communication), BodilyDiscomfort=mean(BodilyDiscomfort))

#   VISIT Mobility DailyLiving Emotional Stigma SocialSupport Cognition Communication BodilyDiscomfort
#   <int>    <dbl>       <dbl>     <dbl>  <dbl>         <dbl>     <dbl>         <dbl>            <dbl>
# 1     0     14.9        8.93      7.93   5.44          1.55      4.53          3.05             5.52
# 2     1     13.2        6.95      7.45   3.76          1.70      3.93          3.40             4.01


wilcox.test(PDQ39$Mobility[PDQ39$VISIT==0], PDQ39$Mobility[PDQ39$VISIT==1], paired=TRUE, conf.int=TRUE)

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  PDQ39$Mobility[PDQ39$VISIT == 0] and PDQ39$Mobility[PDQ39$VISIT == 1]
# V = 77688, p-value = 2.547e-05
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  0.9999603 2.4999874
# sample estimates:
# (pseudo)median 
#        1.50008 

wilcox.test(PDQ39$DailyLiving[PDQ39$VISIT==0], PDQ39$DailyLiving[PDQ39$VISIT==1], paired=TRUE, conf.int=TRUE)

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  PDQ39$DailyLiving[PDQ39$VISIT == 0] and PDQ39$DailyLiving[PDQ39$VISIT == 1]
# V = 85204, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  1.999972 2.500022
# sample estimates:
# (pseudo)median 
#       2.000002 

wilcox.test(PDQ39$Emotional[PDQ39$VISIT==0], PDQ39$Emotional[PDQ39$VISIT==1], paired=TRUE, conf.int=TRUE)

# 	Wilcoxon signed rank test with continuity
# 	correction
# 
# data:  PDQ39$Emotional[PDQ39$VISIT == 0] and PDQ39$Emotional[PDQ39$VISIT == 1]
# V = 67179, p-value = 0.007377
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  3.425068e-05 9.999760e-01
# sample estimates:
# (pseudo)median 
#      0.5000216 

wilcox.test(PDQ39$Stigma[PDQ39$VISIT==0], PDQ39$Stigma[PDQ39$VISIT==1], paired=TRUE, conf.int=TRUE)

# 	Wilcoxon signed rank test with continuity
# 	correction
# 
# data:  PDQ39$Stigma[PDQ39$VISIT == 0] and PDQ39$Stigma[PDQ39$VISIT == 1]
# V = 79481, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  1.500043 2.499946
# sample estimates:
# (pseudo)median 
#       2.000067 

wilcox.test(PDQ39$SocialSupport[PDQ39$VISIT==0], PDQ39$SocialSupport[PDQ39$VISIT==1], paired=TRUE, conf.int=TRUE)

# 	Wilcoxon signed rank test with continuity
# 	correction
# 
# data:  PDQ39$SocialSupport[PDQ39$VISIT == 0] and PDQ39$SocialSupport[PDQ39$VISIT == 1]
# V = 32228, p-value = 0.1488
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  -4.787852e-01  2.108107e-05
# sample estimates:
# (pseudo)median 
#     -0.0771893 

wilcox.test(PDQ39$Cognition[PDQ39$VISIT==0], PDQ39$Cognition[PDQ39$VISIT==1], paired=TRUE, conf.int=TRUE)

# 	Wilcoxon signed rank test with continuity
# 	correction
# 
# data:  PDQ39$Cognition[PDQ39$VISIT == 0] and PDQ39$Cognition[PDQ39$VISIT == 1]
# V = 62858, p-value = 1.469e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  0.4999403 1.0000319
# sample estimates:
# (pseudo)median 
#      0.5000702 

wilcox.test(PDQ39$Communication[PDQ39$VISIT==0], PDQ39$Communication[PDQ39$VISIT==1], paired=TRUE, conf.int=TRUE)

# 	Wilcoxon signed rank test with continuity
# 	correction
# 
# data:  PDQ39$Communication[PDQ39$VISIT == 0] and PDQ39$Communication[PDQ39$VISIT == 1]
# V = 36470, p-value = 0.0007912
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  -5.000073e-01 -5.512825e-05
# sample estimates:
# (pseudo)median 
#     -0.4999262 

wilcox.test(PDQ39$BodilyDiscomfort[PDQ39$VISIT==0], PDQ39$BodilyDiscomfort[PDQ39$VISIT==1], paired=TRUE, conf.int=TRUE)

# 	Wilcoxon signed rank test with continuity
# 	correction
# 
# data:  PDQ39$BodilyDiscomfort[PDQ39$VISIT == 0] and PDQ39$BodilyDiscomfort[PDQ39$VISIT == 1]
# V = 84868, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  1.500005 2.000035
# sample estimates:
# (pseudo)median 
#       1.999982 
      


PDQ39 %>%
  gather(feature, value, Mobility:BodilyDiscomfort) %>%
  mutate(VISIT=ifelse(VISIT==0,"Pre_OP", "Post_OP")) %>%
     mutate(VISIT=factor(VISIT, levels=c("Pre_OP" , "Post_OP"))) %>%
  ggplot(aes(feature  , value, colour=VISIT, fill=VISIT )) +
  geom_boxplot( alpha=0.7, notch = TRUE, notchwidth = 0.3, show.legend = T, outlier.alpha = 0) +
  theme_minimal() +
  xlab("\n") + ylab("Score \n") +
  scale_fill_manual(values=c("#B5838D", "#0081A7")) +
  scale_colour_manual(values=c("#B5838D", "#0081A7")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


      

Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")
Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% select(1,5)

temp <-  PDQ39 %>% inner_join(Asymmetry_Pre_vs_Post)

cor.test(temp$Diff_Post_OP_ONON, temp$Mobility, method="pearson")

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff_Post_OP_ONON and temp$Mobility
# t = 2.2907, df = 1038, p-value = 0.02218
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.01017724 0.13114538
# sample estimates:
#        cor 
# 0.07092208 
	
cor.test(temp$Diff_Post_OP_ONON, temp$DailyLiving, method="pearson")

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff_Post_OP_ONON and temp$DailyLiving
# t = 2.8741, df = 1038, p-value = 0.004135
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.02821742 0.14883850
# sample estimates:
#        cor 
# 0.08885372 

	
cor.test(temp$Diff_Post_OP_ONON, temp$Emotional, method="pearson")

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff_Post_OP_ONON and temp$Emotional
# t = 0.61637, df = 1038, p-value = 0.5378
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.04170941  0.07982369
# sample estimates:
#       cor 
# 0.0191278 

	
cor.test(temp$Diff_Post_OP_ONON, temp$Stigma, method="pearson")

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff_Post_OP_ONON and temp$Stigma
# t = 1.4268, df = 1038, p-value = 0.1539
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.01659107  0.10474925
# sample estimates:
#        cor 
# 0.04424226 

	
cor.test(temp$Diff_Post_OP_ONON, temp$SocialSupport, method="pearson")

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff_Post_OP_ONON and temp$SocialSupport
# t = 0.40603, df = 1038, p-value = 0.6848
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.04822412  0.07333406
# sample estimates:
#        cor 
# 0.01260153 

cor.test(temp$Diff_Post_OP_ONON, temp$Cognition, method="pearson")

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff_Post_OP_ONON and temp$Cognition
# t = 0.29777, df = 1038, p-value = 0.7659
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.05157588  0.06999119
# sample estimates:
#         cor 
# 0.009241806 
	
cor.test(temp$Diff_Post_OP_ONON, temp$Communication, method="pearson")

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff_Post_OP_ONON and temp$Communication
# t = 0.83436, df = 1038, p-value = 0.4043
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.03495522  0.08654101
# sample estimates:
#        cor 
# 0.02588849 

	
cor.test(temp$Diff_Post_OP_ONON, temp$BodilyDiscomfort, method="pearson")

# 	Pearson's product-moment correlation
# 
# data:  temp$Diff_Post_OP_ONON and temp$BodilyDiscomfort
# t = 0.61888, df = 1038, p-value = 0.5361
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.04163173  0.07990101
# sample estimates:
#        cor 
# 0.01920559 

temp <- temp %>% filter(VISIT == 1) %>% 
  mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5, ">5", "no"))  %>% 
  select(-c(VISIT, SUBJID))


temp %>% group_by(Diff_Post_OP_ONON) %>% summarise(Mobility=mean(Mobility), DailyLiving=mean(DailyLiving), 
                                        Emotional=mean(Emotional), Stigma=mean(Stigma), SocialSupport=mean(SocialSupport),
                                        Cognition=mean(Cognition), Communication=mean(Communication), BodilyDiscomfort=mean(BodilyDiscomfort))

#   Diff_Post_OP_ONON Mobility DailyLiving Emotional Stigma SocialSupport Cognition Communication BodilyDiscomfort
#   <chr>                <dbl>       <dbl>     <dbl>  <dbl>         <dbl>     <dbl>         <dbl>            <dbl>
# 1 >5                    15.8        8.24      7.66   3.91          1.92      4.22          3.96             4.42
# 2 no                    12.8        6.78      7.43   3.74          1.69      3.92          3.30             3.98


wilcox.test(temp$Mobility[temp$Diff_Post_OP_ONON==">5"], temp$Mobility[temp$Diff_Post_OP_ONON=="no"], paired=F, conf.int=TRUE)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$Mobility[temp$Diff_Post_OP_ONON == ">5"] and temp$Mobility[temp$Diff_Post_OP_ONON == "no"]
# W = 17234, p-value = 0.01109
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  0.9999268 5.0000172
# sample estimates:
# difference in location 
#               3.000008 

wilcox.test(temp$DailyLiving[temp$Diff_Post_OP_ONON==">5"], temp$DailyLiving[temp$Diff_Post_OP_ONON=="no"], paired=F, conf.int=TRUE)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$DailyLiving[temp$Diff_Post_OP_ONON == ">5"] and temp$DailyLiving[temp$Diff_Post_OP_ONON == "no"]
# W = 17131, p-value = 0.01424
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  9.490689e-06 2.999979e+00
# sample estimates:
# difference in location 
#               1.999987 

wilcox.test(temp$Emotional[temp$Diff_Post_OP_ONON==">5"], temp$Emotional[temp$Diff_Post_OP_ONON=="no"], paired=F, conf.int=TRUE)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$Emotional[temp$Diff_Post_OP_ONON == ">5"] and temp$Emotional[temp$Diff_Post_OP_ONON == "no"]
# W = 15041, p-value = 0.5632
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  -0.9999616  1.9999294
# sample estimates:
# difference in location 
#           2.032368e-05 

wilcox.test(temp$Stigma[temp$Diff_Post_OP_ONON==">5"], temp$Stigma[temp$Diff_Post_OP_ONON=="no"], paired=F, conf.int=TRUE)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$Stigma[temp$Diff_Post_OP_ONON == ">5"] and temp$Stigma[temp$Diff_Post_OP_ONON == "no"]
# W = 14402, p-value = 0.9957
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  -0.9999293  0.9999968
# sample estimates:
# difference in location 
#           2.774932e-05 

wilcox.test(temp$SocialSupport[temp$Diff_Post_OP_ONON==">5"], temp$SocialSupport[temp$Diff_Post_OP_ONON=="no"], paired=F, conf.int=TRUE)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$SocialSupport[temp$Diff_Post_OP_ONON == ">5"] and temp$SocialSupport[temp$Diff_Post_OP_ONON == "no"]
# W = 15991, p-value = 0.1405
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  -5.697289e-05  6.520025e-01
# sample estimates:
# difference in location 
#           3.966289e-05 

wilcox.test(temp$Cognition[temp$Diff_Post_OP_ONON==">5"], temp$Cognition[temp$Diff_Post_OP_ONON=="no"], paired=F, conf.int=TRUE)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$Cognition[temp$Diff_Post_OP_ONON == ">5"] and temp$Cognition[temp$Diff_Post_OP_ONON == "no"]
# W = 15164, p-value = 0.4892
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  -6.622079e-05  1.000017e+00
# sample estimates:
# difference in location 
#            2.42429e-05 

wilcox.test(temp$Communication[temp$Diff_Post_OP_ONON==">5"], temp$Communication[temp$Diff_Post_OP_ONON=="no"], paired=F, conf.int=TRUE)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$Communication[temp$Diff_Post_OP_ONON == ">5"] and temp$Communication[temp$Diff_Post_OP_ONON == "no"]
# W = 16834, p-value = 0.028
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  4.601674e-06 1.000048e+00
# sample estimates:
# difference in location 
#              0.9999802 

wilcox.test(temp$BodilyDiscomfort[temp$Diff_Post_OP_ONON==">5"], temp$BodilyDiscomfort[temp$Diff_Post_OP_ONON=="no"], paired=F, conf.int=TRUE)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$BodilyDiscomfort[temp$Diff_Post_OP_ONON == ">5"] and temp$BodilyDiscomfort[temp$Diff_Post_OP_ONON == "no"]
# W = 16042, p-value = 0.1381
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#  -9.914427e-06  1.000016e+00
# sample estimates:
# difference in location 
#              0.4375066 

temp %>%
  rename("Asymmetric_Post-OP_[ON/ON]"="Diff_Post_OP_ONON") %>%
  gather(feature, value, Mobility:BodilyDiscomfort) %>%
  mutate(`Asymmetric_Post-OP_[ON/ON]`=ifelse(`Asymmetric_Post-OP_[ON/ON]`=="no", "<5", "≥5")) %>%
     mutate(`Asymmetric_Post-OP_[ON/ON]` =factor(`Asymmetric_Post-OP_[ON/ON]` , levels=c("<5" , "≥5"))) %>%
  ggplot(aes(feature  , value, colour=`Asymmetric_Post-OP_[ON/ON]`, fill=`Asymmetric_Post-OP_[ON/ON]` )) +
  geom_boxplot(alpha=0.7, notch = TRUE, notchwidth = 0.3, show.legend = T, outlier.alpha = 0) +
  theme_minimal() +
  xlab("\n") + ylab("Score \n") +
  scale_fill_manual(values=c("#B5838D", "#0081A7")) +
  scale_colour_manual(values=c("#B5838D", "#0081A7")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# -------------------------------
# Axial Score -----------------------------------------------------

UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
df_names <- names(UPDRSIII_COMPLET_V0_V1)


ONON_After <- data.frame(df_names) %>%
  filter(row_number()>272) %>%
  filter(grepl("^ON", df_names)) %>%
    filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)) %>%  arrange(df_names)  %>%
  filter(!grepl("OFF", df_names)) 


toString(as.list(ONON_After))

match <- c("ON_3.10_6", "ON_3.11_6", "ON_3.12_6", "ON_3.9_6")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONON_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONON_After <- ONON_After[-1,]

ONON_After <- ONON_After %>% mutate(ON_3.9_6 =as.numeric(ON_3.9_6 ),
                                    ON_3.10_6 =as.numeric(ON_3.10_6 ),
                                    ON_3.11_6 =as.numeric(ON_3.11_6 ),
                                    ON_3.12_6=as.numeric(ON_3.12_6)
                                    ) %>% drop_na()





OFFOFF_After <- data.frame(df_names) %>%
  filter(row_number()>272) %>%
  filter(grepl("^OFF", df_names)) %>%
    filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)) %>%  arrange(df_names)  %>%
  filter(!grepl("ON", df_names)) 


toString(as.list(OFFOFF_After))

match <- c("OFF_3.10_1", "OFF_3.11_1", "OFF_3.12_1", "OFF_3.9_1")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFFOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
OFFOFF_After <- OFFOFF_After[-1,]


OFFOFF_After <- OFFOFF_After %>% mutate(OFF_3.9_1  =as.numeric(OFF_3.9_1  ),
                                    OFF_3.10_1  =as.numeric(OFF_3.10_1  ),
                                    OFF_3.11_1  =as.numeric(OFF_3.11_1  ),
                                    OFF_3.12_1=as.numeric(OFF_3.12_1)
                                    ) %>% drop_na()




ONOFF_After <- data.frame(df_names) %>%
  filter(grepl("^ONOFF", df_names)) %>%
    filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)
           ) %>%
  arrange(df_names) 

toString(as.list(ONOFF_After))

match <- c("ONOFF_3.10_", "ONOFF_3.11_", "ONOFF_3.12_", "ONOFF_3.9_")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONOFF_After <- ONOFF_After[-1,]

  
ONOFF_After <- ONOFF_After %>% mutate(ONOFF_3.10_  =as.numeric(ONOFF_3.10_  ),
                                    ONOFF_3.11_  =as.numeric(ONOFF_3.11_  ),
                                    ONOFF_3.12_  =as.numeric(ONOFF_3.12_  ),
                                    ONOFF_3.9_=as.numeric(ONOFF_3.9_)
                                    ) %>% drop_na()




OFFON_After <- data.frame(df_names) %>%
  filter(grepl("^OFFON", df_names)) %>%
    filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)
           ) %>%
  arrange(df_names) 

toString(as.list(OFFON_After))

match <- c("OFFON_3.10_", "OFFON_3.11_", "OFFON_3.12_", "OFFON_3.9_")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFFON_After <- UPDRSIII_COMPLET_V0_V1[which_names]
OFFON_After <- OFFON_After[-1,]


OFFON_After <- OFFON_After %>% mutate(OFFON_3.10_  =as.numeric(OFFON_3.10_  ),
                                    OFFON_3.11_  =as.numeric(OFFON_3.11_  ),
                                    OFFON_3.12_  =as.numeric(OFFON_3.12_  ),
                                    OFFON_3.9_=as.numeric(OFFON_3.9_)
                                    ) %>% drop_na()





ONON_After$AxialScoreON <- ONON_After$ON_3.9_6 + ONON_After$ON_3.10_6 + ONON_After$ON_3.11_6 + ONON_After$ON_3.12_6
OFFOFF_After$AxialScoreOFF <- OFFOFF_After$OFF_3.9_1 + OFFOFF_After$OFF_3.10_1 + OFFOFF_After$OFF_3.11_1 + OFFOFF_After$OFF_3.12_1 
ONOFF_After$AxialScoreONOFF <- ONOFF_After$ONOFF_3.9_  + ONOFF_After$ONOFF_3.10_  + ONOFF_After$ONOFF_3.11_  + ONOFF_After$ONOFF_3.12_ 
OFFON_After$AxialScoreOFFON <- OFFON_After$OFFON_3.9_  + OFFON_After$OFFON_3.10_  + OFFON_After$OFFON_3.11_  + OFFON_After$OFFON_3.12_ 


Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")

Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% 
  inner_join(OFFOFF_After %>% select(SUBJID, AxialScoreOFF)) %>%
    inner_join(ONON_After %>% select(SUBJID, AxialScoreON)) %>%
      inner_join(ONOFF_After %>% select(SUBJID, AxialScoreONOFF)) %>%
    inner_join(OFFON_After %>% select(SUBJID, AxialScoreOFFON))


cor.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON, Asymmetry_Pre_vs_Post$AxialScoreON)
cor.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF, Asymmetry_Pre_vs_Post$AxialScoreOFF)
cor.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFON, Asymmetry_Pre_vs_Post$AxialScoreOFFON)
cor.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_ONOFF, Asymmetry_Pre_vs_Post$AxialScoreONOFF)



Asymmetry_Pre_vs_Post %>%
  mutate(Diff_Post_OP_OFFOFF=ifelse(Diff_Post_OP_OFFOFF>=5, ">5", "<5")) %>%
  group_by(Diff_Post_OP_OFFOFF) %>%
  summarise(n=mean(AxialScoreOFF))

Asymmetry_Pre_vs_Post %>%
  mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5, ">5", "<5")) %>%
  group_by(Diff_Post_OP_ONON) %>%
  summarise(n=mean(AxialScoreON))


Asymmetry_Pre_vs_Post %>%
  mutate(Diff_Post_OP_ONOFF=ifelse(Diff_Post_OP_ONOFF>=5, ">5", "<5")) %>%
  group_by(Diff_Post_OP_ONOFF) %>%
  summarise(n=mean(AxialScoreONOFF))


Asymmetry_Pre_vs_Post %>%
  mutate(Diff_Post_OP_OFFON=ifelse(Diff_Post_OP_OFFON>=5, ">5", "<5")) %>%
  group_by(Diff_Post_OP_OFFON) %>%
  summarise(n=mean(AxialScoreOFFON))



Asymmetry_Pre_vs_Post %>%
  mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5, ">5", "<5")) %>% 
  rename("Asymmetric Post-OP [ON/ON]"="Diff_Post_OP_ONON") %>%
    mutate(`Asymmetric Post-OP [ON/ON]`=ifelse(`Asymmetric Post-OP [ON/ON]`=="<5", "<5", "≥5")) %>% 
    rename("Axial Score Post-OP [ON/ON]"="AxialScoreON") %>%
  ggplot(aes(`Asymmetric Post-OP [ON/ON]`  , `Axial Score Post-OP [ON/ON]`, 
             colour=`Asymmetric Post-OP [ON/ON]`, fill=`Asymmetric Post-OP [ON/ON]` )) +
  geom_jitter(width=0.2, height = 0.2, alpha=0.7, size=1, show.legend = FALSE) +
  geom_violin(alpha=0.5) +
  geom_boxplot(alpha=0.4, notch = F,  show.legend = T, outlier.alpha = 0) +
  theme_minimal() +
  xlab("\n Asymmetric Post-OP [ON/ON]") + ylab("Axial Score Post-OP [ON/ON] \n") +
  scale_fill_manual(values=c("#B5838D", "#0081A7")) +
  scale_colour_manual(values=c("#B5838D", "#0081A7")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


Asymmetry_Pre_vs_Post %>%
  mutate(Diff_Post_OP_ONOFF=ifelse(Diff_Post_OP_ONOFF>=5, ">5", "<5")) %>% 
  rename("Asymmetric Post-OP [ON/OFF]"="Diff_Post_OP_ONOFF") %>%
    mutate(`Asymmetric Post-OP [ON/OFF]`=ifelse(`Asymmetric Post-OP [ON/OFF]`=="<5", "<5", "≥5")) %>% 
    rename("Axial Score Post-OP [ON/OFF]"="AxialScoreONOFF") %>%
  ggplot(aes(`Asymmetric Post-OP [ON/OFF]`  , `Axial Score Post-OP [ON/OFF]`, 
             colour=`Asymmetric Post-OP [ON/OFF]`, fill=`Asymmetric Post-OP [ON/OFF]` )) +
  geom_jitter(width=0.2, height = 0.2, alpha=0.7, size=1, show.legend = FALSE) +
  geom_violin(alpha=0.5) +
  geom_boxplot(alpha=0.4, notch = F,  show.legend = T, outlier.alpha = 0) +
  theme_minimal() +
  xlab("\n Asymmetric Post-OP [ON Stim/OFF Med]") + ylab("Axial Score Post-OP [ON Stim/OFF Med] \n") +
  scale_fill_manual(values=c("#B5838D", "#0081A7")) +
  scale_colour_manual(values=c("#B5838D", "#0081A7")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



Asymmetry_Pre_vs_Post %>%
  mutate(Diff_Post_OP_OFFON=ifelse(Diff_Post_OP_OFFON>=5, ">5", "<5")) %>% 
  rename("Asymmetric Post-OP [OFF/ON]"="Diff_Post_OP_OFFON") %>%
    mutate(`Asymmetric Post-OP [OFF/ON]`=ifelse(`Asymmetric Post-OP [OFF/ON]`=="<5", "<5", "≥5")) %>% 
    rename("Axial Score Post-OP [OFF/ON]"="AxialScoreOFFON") %>%
  ggplot(aes(`Asymmetric Post-OP [OFF/ON]`  , `Axial Score Post-OP [OFF/ON]`, 
             colour=`Asymmetric Post-OP [OFF/ON]`, fill=`Asymmetric Post-OP [OFF/ON]` )) +
  geom_jitter(width=0.2, height = 0.2, alpha=0.7, size=1, show.legend = FALSE) +
  geom_violin(alpha=0.5) +
  geom_boxplot(alpha=0.4, notch = F,  show.legend = T, outlier.alpha = 0) +
  theme_minimal() +
  xlab("\n Asymmetric Post-OP [OFF Stim/ON Med]") + ylab("Axial Score Post-OP [OFF Stim/ON Med] \n") +
  scale_fill_manual(values=c("#B5838D", "#0081A7")) +
  scale_colour_manual(values=c("#B5838D", "#0081A7")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5, ">5", "<5"))
Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% mutate(Diff_Post_OP_OFFOFF=ifelse(Diff_Post_OP_OFFOFF>=5, ">5", "<5"))
Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% mutate(Diff_Post_OP_OFFON=ifelse(Diff_Post_OP_OFFON>=5, ">5", "<5"))
Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% mutate(Diff_Post_OP_ONOFF=ifelse(Diff_Post_OP_ONOFF>=5, ">5", "<5"))

wilcox.test(Asymmetry_Pre_vs_Post$AxialScoreON[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON==">5"], 
            Asymmetry_Pre_vs_Post$AxialScoreON[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON=="<5"], paired=F, conf.int=F)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  Asymmetry_Pre_vs_Post$AxialScoreON[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON == ">5"] and Asymmetry_Pre_vs_Post$AxialScoreON[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON == "<5"]
# W = 18033, p-value = 0.0004219
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(Asymmetry_Pre_vs_Post$AxialScoreOFF[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF==">5"], 
            Asymmetry_Pre_vs_Post$AxialScoreOFF[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF=="<5"], paired=F, conf.int=F)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  Asymmetry_Pre_vs_Post$AxialScoreOFF[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF == ">5"] and Asymmetry_Pre_vs_Post$AxialScoreOFF[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF == "<5"]
# W = 32312, p-value = 0.5224
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(Asymmetry_Pre_vs_Post$AxialScoreOFFON[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFON==">5"], 
            Asymmetry_Pre_vs_Post$AxialScoreOFFON[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFON=="<5"], paired=F, conf.int=F)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  Asymmetry_Pre_vs_Post$AxialScoreOFFON[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFON == ">5"] and Asymmetry_Pre_vs_Post$AxialScoreOFFON[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFON == "<5"]
# W = 26972, p-value = 0.3432
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(Asymmetry_Pre_vs_Post$AxialScoreONOFF[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONOFF==">5"], 
            Asymmetry_Pre_vs_Post$AxialScoreONOFF[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONOFF=="<5"], paired=F, conf.int=F)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  Asymmetry_Pre_vs_Post$AxialScoreONOFF[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONOFF == ">5"] and Asymmetry_Pre_vs_Post$AxialScoreONOFF[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONOFF == "<5"]
# W = 34496, p-value = 0.003258
# alternative hypothesis: true location shift is not equal to 0

# -----------------
# PIGD vs TD dominant phenotypes UPDRSIII & UPDRSII ---------------------------------------------------------------

OFF_before_ALL <- data.frame(df_names) %>%
  filter(grepl("^OFF_", df_names)) %>%
    filter(!grepl("1$", df_names)) %>%
    filter(grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)|
           grepl("3.18", df_names)
           ) %>%
  arrange(df_names) 

toString(as.list(OFF_before_ALL))

match <- c("OFF_3.10_", "OFF_3.11_", "OFF_3.12_", "OFF_3.15_Left", "OFF_3.15_Right_", 
  "OFF_3.16_Left", "OFF_3.16_Right", "OFF_3.17_Inf_Left_", "OFF_3.17_Inf_Right", "OFF_3.17_Sup_Left_", 
  "OFF_3.17_Sup_Right", "OFF_3.17_lip_", "OFF_3.18_")

match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFF_before_ALL <- UPDRSIII_COMPLET_V0_V1[which_names]
OFF_before_ALL <- OFF_before_ALL[-1,]

names(OFF_before_ALL)

OFF_before_ALL <- data.frame(OFF_before_ALL %>% gather(Var, Value, OFF_3.10_:OFF_3.18_ ) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<13)) %>% select(SUBJID) %>%
  inner_join(OFF_before_ALL)
 
OFF_before_ALL <- data.frame(OFF_before_ALL) %>% mutate_each(as.numeric, OFF_3.10_:OFF_3.18_)
sum(is.na(OFF_before_ALL))

UPDRSI_II <- fread("Processed_data/UPDRSI_II.txt")
UPDRSI_II <- UPDRSI_II %>% filter(VISIT == 0) %>% select(SUBJID, MDS2_10OFF, MDS2_12OFF, MDS2_13OFF)
sum(is.na(UPDRSI_II))


for(i in 2:14){
  cat(i)
  print(round(mean(OFF_before_ALL[,i], na.rm = T),5))
}

# 2[1] 1.62921
# 3[1] 0.81054
# 4[1] 1.1225
# 5[1] 0.56696
# 6[1] 0.54261
# 7[1] 0.29036
# 8[1] 0.3584
# 9[1] 0.14518
# 10[1] 0.70213
# 11[1] 0.67459
# 12[1] 0.43304
# 13[1] 0.40426
# 14[1] 1.50877

dim_desc(OFF_before_ALL) 
sum(is.na(OFF_before_ALL)) 
drop_na(OFF_before_ALL) 

Imputed <- imputePCA(OFF_before_ALL[,-1],ncp=2, scale = T)

OFF_before_ALL <- OFF_before_ALL %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:14){
  cat(i)
  print(round(mean(OFF_before_ALL[,i], na.rm = T),5))
}

# 2[1] 1.62921
# 3[1] 0.81712
# 4[1] 1.12381
# 5[1] 0.56667
# 6[1] 0.5443
# 7[1] 0.29022
# 8[1] 0.3596
# 9[1] 0.14518
# 10[1] 0.70174
# 11[1] 0.6743
# 12[1] 0.43295
# 13[1] 0.40431
# 14[1] 1.50843

sum(is.na(OFF_before_ALL))

sum(OFF_before_ALL<0)

PIGD_TD <- OFF_before_ALL %>% inner_join(UPDRSI_II)
names(PIGD_TD)

PIGD_TD <- PIGD_TD %>% 
  mutate(PIGD_Score = MDS2_12OFF+MDS2_13OFF+OFF_3.10_+OFF_3.11_+OFF_3.12_ ) %>%
  mutate(TD_Score = OFF_3.15_Right_+OFF_3.15_Left+OFF_3.16_Right+OFF_3.16_Left+
           OFF_3.17_lip_+OFF_3.17_Sup_Right+OFF_3.17_Sup_Left_+OFF_3.17_Inf_Right+OFF_3.17_Inf_Left_+OFF_3.18_) %>%
  select(SUBJID, PIGD_Score, TD_Score) %>%
  mutate(PIGD_Score=PIGD_Score/5, TD_Score=TD_Score/11) %>%
  mutate(Type=TD_Score/PIGD_Score)

PIGD_TD %>% mutate(Pheno = ifelse(Type>=1.15, "TD", ifelse(Type<=0.9, "PIGD", "Indet"))) %>%
  group_by(Pheno) %>% count()

#   Pheno     n
#   <chr> <int>
# 1 Indet    21
# 2 PIGD    262
# 3 TD       69
# > 69/(21+262+69)
# [1] 0.1960227
# > 262/(21+262+69)
# [1] 0.7443182
# > 21/(21+262+69)
# [1] 0.05965909

PIGD_TD <- PIGD_TD %>% mutate(Pheno = ifelse(Type>=1.15, "TD", ifelse(Type<=0.9, "PIGD", "Indet")))

Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")

Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% select(SUBJID, Diff_Pre_OP)

PIGD_TD <- Asymmetry_Pre_vs_Post %>% inner_join(PIGD_TD)

PIGD_TD %>% group_by(Pheno) %>% summarise(n=mean(Diff_Pre_OP))

#   Pheno     n
# 1 Indet  6.56
# 2 PIGD   4.58
# 3 TD     7.23

PIGD_TD <- PIGD_TD %>% select(SUBJID, Pheno, Diff_Pre_OP)

kruskal.test(Diff_Pre_OP ~ Pheno, data = PIGD_TD)

# 	Kruskal-Wallis rank sum test
# 
# data:  Diff_Pre_OP by Pheno
# Kruskal-Wallis chi-squared = 13.638, df = 2, p-value = 0.001093

pairwise.wilcox.test(PIGD_TD$Diff_Pre_OP, PIGD_TD$Pheno,
                 p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# 
# data:  PIGD_TD$Diff_Pre_OP and PIGD_TD$Pheno 
# 
#      Indet  PIGD  
# PIGD 0.1223 -     
# TD   1.0000 0.0029
# 
# P value adjustment method: bonferroni 


PIGD_TD %>%
   mutate(Pheno=factor(Pheno, levels=c("PIGD" ,"Indet", "TD"))) %>%
  ggplot(aes(Pheno, Diff_Pre_OP, colour=Pheno, fill=Pheno )) +
  geom_violin(alpha=0.4, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = FALSE, notchwidth = 0.3, varwidth = F, show.legend = FALSE) +
  geom_jitter(width=0.2, height = 0.6, alpha=0.9, show.legend = FALSE) +
  theme_minimal() +
  xlab("\n  Clinical Phenotype") + ylab("Absolute R-to-L Difference \n (i.e. Asymmetry) \n") +
  scale_fill_brewer(palette="PuBu") +
  scale_colour_brewer(palette="PuBu") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




# --------------
# Which side is worst before and after surgery ? -------------------------------------------------

sheets_list <- excel_sheets(path = "Raw_Database/Asymmetry_DeepBrainStimulation.xlsx")

#  [1] "DEMOGRAPHIE "             "FACTEURSDERISQUE "        "ATCD_MED_CHIR"           
#  [4] "SOCIAL "                  "PDQ39-CGIS-SCOPA"         "PGI"                     
#  [7] "UPDRS II"                 "UPDRSIII_TOTAUX"          "UPDRSIII_COMPLET_V0_V1"  
# [10] "UPDRSI_II_IV"             "Hoehn&Yarh-S&E"           "EVA_FNM_V0_V1"           
# [13] "HAM-D"                    "HAM-A"                    "TCI_TCSP_V0"             
# [16] "Hallu_Miami"              "MoCA V0"                  "MoCA V1"                 
# [19] "Clox"                     "Boston_Fluence"           "PEROP_COMPLPEROP"        
# [22] "FREQUENCE_V0"             "FREQUENCE_V1"             "EVENEMENTSINDESIRABLES"  
# [25] "CONSO_SPE"                "PSYCHOTROPES"             "AUTRE_PARKINSON"         
# [28] "MEDICAMENTS dans Rapport" "DATES_DE_VISITES "


UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

# Items 3.3–3.8 and 3.15–3.17

df_names <- names(UPDRSIII_COMPLET_V0_V1)


# Pre OP OFF

OFF_before <- data.frame(df_names) %>%
  filter(grepl("^OFF_", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names) %>%
  filter(!grepl("1$", df_names))

toString(as.list(OFF_before))

match <- c("OFF_3.15_Left", "OFF_3.15_Right_", "OFF_3.16_Left", "OFF_3.16_Right", "OFF_3.17_Inf_Left_", 
  "OFF_3.17_Inf_Right", "OFF_3.17_Sup_Left_", "OFF_3.17_Sup_Right", "OFF_3.3_Inf_Left", "OFF_3.3_Inf_Right", 
  "OFF_3.3_S_Left", "OFF_3.3_S_Right", "OFF_3.4_Left_", "OFF_3.4_Right_", "OFF_3.5_Left_", "OFF_3.5_Right_", 
  "OFF_3.6_Left_", "OFF_3.6_Right_", "OFF_3.7_Left", "OFF_3.7_Right_", "OFF_3.8_Left", "OFF_3.8_Right_")

match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFF_before <- UPDRSIII_COMPLET_V0_V1[which_names]
OFF_before <- OFF_before[-1,]

names(OFF_before)

OFF_before <- data.frame(OFF_before %>% gather(Var, Value, OFF_3.3_S_Right:OFF_3.17_Inf_Left_) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(OFF_before)
 
OFF_before <- data.frame(OFF_before) %>% mutate_each(as.numeric, OFF_3.3_S_Right:OFF_3.17_Inf_Left_)

dim_desc(OFF_before) # "[802 x 23]"  -> 18446
sum(is.na(OFF_before))# 39  0.00211428   0.2%
drop_na(OFF_before) # 7 pats

Imputed <- imputePCA(OFF_before[,-1],ncp=2, scale = T)

OFF_before <- OFF_before %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

sum(is.na(OFF_before))

sum(OFF_before<0)

OFF_before <- data.frame(OFF_before) %>% drop_na() %>% gather(Var, Value, OFF_3.3_S_Right:OFF_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>%  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(OFF_before) %>% drop_na() %>% gather(Var, Value, OFF_3.3_S_Right:OFF_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

# Post OP ON OFF


ONOFF_After <- data.frame(df_names) %>%
  filter(grepl("^ONOFF", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names) 

toString(as.list(ONOFF_After))

match <- c("ONOFF_3.15_Left", "ONOFF_3.15_Right_", "ONOFF_3.16_Left", "ONOFF_3.16_Right", "ONOFF_3.17_Inf_Left_", 
           "ONOFF_3.17_Inf_Right", "ONOFF_3.17_Sup_Left_", "ONOFF_3.17_Sup_Right", "ONOFF_3.3_Inf_Left", 
           "ONOFF_3.3_Inf_Right", "ONOFF_3.3_S_Left", "ONOFF_3.3_S_Right", "ONOFF_3.4_Left_", "ONOFF_3.4_Right_",
           "ONOFF_3.5_Left_", "ONOFF_3.5_Right_", "ONOFF_3.6_Left_", "ONOFF_3.6_Right_", "ONOFF_3.7_Left", 
           "ONOFF_3.7_Right_", "ONOFF_3.8_Left", "ONOFF_3.8_Right_")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONOFF_After <- ONOFF_After[-1,]

names(ONOFF_After)

# 291 patients had no data in the ON OFF post-OP whatsoever

ONOFF_After <- data.frame(ONOFF_After %>% gather(Var, Value, ONOFF_3.3_S_Right:ONOFF_3.17_Inf_Left_) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONOFF_After)
 
ONOFF_After <- data.frame(ONOFF_After) %>% mutate_each(as.numeric, ONOFF_3.3_S_Right:ONOFF_3.17_Inf_Left_)

dim_desc(ONOFF_After) # "[545 x 23]"  -> 12535
sum(is.na(ONOFF_After))# 143  0.01140806   0.1%
drop_na(ONOFF_After) # 11 pats

Imputed <- imputePCA(ONOFF_After[,-1],ncp=2, scale = T)

ONOFF_After <- ONOFF_After %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

ONOFF_After <- data.frame(ONOFF_After) %>% drop_na() %>% gather(Var, Value, ONOFF_3.3_S_Right:ONOFF_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(ONOFF_After) %>% drop_na() %>% gather(Var, Value, ONOFF_3.3_S_Right:ONOFF_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

# Post OP OFF ON

OFFON_After <- data.frame(df_names) %>%
  filter(grepl("^OFFON", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names) 

toString(as.list(OFFON_After))

match <- c("OFFON_3.15_Left", "OFFON_3.15_Right_", "OFFON_3.16_Left", "OFFON_3.16_Right", 
           "OFFON_3.17_Inf_Left_", "OFFON_3.17_Inf_Right", "OFFON_3.17_Sup_Left_",
           "OFFON_3.17_Sup_Right", "OFFON_3.3_Inf_Left", "OFFON_3.3_Inf_Right",
           "OFFON_3.3_S_Left", "OFFON_3.3_S_Right", "OFFON_3.4_Left_", "OFFON_3.4_Right_", 
           "OFFON_3.5_Left_", "OFFON_3.5_Right_", "OFFON_3.6_Left_", 
           "OFFON_3.6_Right_", "OFFON_3.7_Left", "OFFON_3.7_Right_", "OFFON_3.8_Left", "OFFON_3.8_Right_")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFFON_After <- UPDRSIII_COMPLET_V0_V1[which_names]
OFFON_After <- OFFON_After[-1,]

names(OFFON_After)

# 296 patients had no data in the ON OFF post-OP whatsoever

OFFON_After <- data.frame(OFFON_After %>% gather(Var, Value, OFFON_3.3_S_Right:OFFON_3.17_Inf_Left_) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(OFFON_After)
 
OFFON_After <- data.frame(OFFON_After) %>% mutate_each(as.numeric, OFFON_3.3_S_Right:OFFON_3.17_Inf_Left_)

dim_desc(OFFON_After) # "[539 x 23]"  -> 12397
sum(is.na(OFFON_After))# 280  0.02258611   0.2%
drop_na(OFFON_After) # 17 pats

Imputed <- imputePCA(OFFON_After[,-1],ncp=2, scale = T)

OFFON_After <- OFFON_After %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

sum(OFFON_After<0)
OFFON_After[OFFON_After<0] <- 0
sum(OFFON_After<0)

OFFON_After <- data.frame(OFFON_After) %>% drop_na() %>% gather(Var, Value, OFFON_3.3_S_Right:OFFON_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(OFFON_After) %>% drop_na() %>% gather(Var, Value, OFFON_3.3_S_Right:OFFON_3.17_Inf_Left_) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 


# Post OP ON ON

ONON_After <- data.frame(df_names) %>%
  filter(row_number()>272) %>%
  filter(grepl("^ON", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names)  %>%
  filter(!grepl("OFF", df_names)) 


toString(as.list(ONON_After))

match <- c("ON_3.15_Left6", "ON_3.15_Right_6", "ON_3.16_Left6", "ON_3.16_Right6", 
           "ON_3.17_Inf_Left_6", "ON_3.17_Inf_Right6", "ON_3.17_Sup_Left_6", 
           "ON_3.17_Sup_Right6", "ON_3.3_Inf_Left", "ON_3.3_Inf_Right", "ON_3.3_S_Left", 
           "ON_3.3_S_Right", "ON_3.4_Left_", "ON_3.4_Right_", "ON_3.5_Left_", "ON_3.5_Right_", 
           "ON_3.6_Left_", "ON_3.6_Right_", "ON_3.7_Left", "ON_3.7_Right_", "ON_3.8_Left6", "ON_3.8_Right_6")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONON_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONON_After <- ONON_After[-1,]

names(ONON_After)

# 292 patients had no data in the ON OFF post-OP whatsoever

ONON_After <- data.frame(ONON_After %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After)
 
ONON_After <- data.frame(ONON_After) %>% mutate_each(as.numeric, ON_3.3_S_Right:ON_3.17_Inf_Left_6)

dim_desc(ONON_After) # "[543 x 23]"  -> 12489
sum(is.na(ONON_After))# 189  0.01513332   0.15%
drop_na(ONON_After) # 14 pats

Imputed <- imputePCA(ONON_After[,-1],ncp=2, scale = T)

ONON_After <- ONON_After %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)


sum(ONON_After<0)
ONON_After[ONON_After<0] <- 0
sum(ONON_After<0)

ONON_After <- data.frame(ONON_After) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(ONON_After) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 



# Post OP OFF OFF


OFFOFF_After <- data.frame(df_names) %>%
  filter(row_number()>272) %>%
  filter(grepl("^OFF", df_names)) %>%
    filter(grepl("3.3", df_names)|
           grepl("3.4", df_names)|
           grepl("3.5", df_names)|
           grepl("3.6", df_names)|
           grepl("3.7", df_names)|
           grepl("3.8", df_names)|
           grepl("3.15", df_names)|
           grepl("3.16", df_names)|
           grepl("3.17", df_names)
           ) %>%
  filter(grepl("Right", df_names)|grepl("right", df_names)|grepl("left", df_names)|grepl("Left", df_names)) %>%
  arrange(df_names)  %>%
  filter(!grepl("ON", df_names)) 


toString(as.list(OFFOFF_After))

match <- c("OFF_3.15_Left1", "OFF_3.15_Right_1", "OFF_3.16_Left1", "OFF_3.16_Right1", "OFF_3.17_Inf_Left_1",
           "OFF_3.17_Inf_Right1", "OFF_3.17_Sup_Left_1", "OFF_3.17_Sup_Right1", "OFF_3.3_Inf_Left1",
           "OFF_3.3_Inf_Right1", "OFF_3.3_S_Left1", "OFF_3.3_S_Right1", "OFF_3.4_Left_1", "OFF_3.4_Right_1", 
           "OFF_3.5_Left_1", "OFF_3.5_Right_1", "OFF_3.6_Left_1", "OFF_3.6_Right_1", "OFF_3.7_Left1", 
           "OFF_3.7_Right_1", "OFF_3.8_Left1", "OFF_3.8_Right_1")
           
match <- append("SUBJID", match)
           
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFFOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
OFFOFF_After <- OFFOFF_After[-1,]

names(OFFOFF_After)

# 292 patients had no data in the OFF OFF post-OP whatsoever

OFFOFF_After <- data.frame(OFFOFF_After %>% gather(Var, Value, OFF_3.3_S_Right1:OFF_3.17_Inf_Left_1) %>%
  group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(OFFOFF_After)
 
OFFOFF_After <- data.frame(OFFOFF_After) %>% mutate_each(as.numeric, OFF_3.3_S_Right1:OFF_3.17_Inf_Left_1)

dim_desc(OFFOFF_After) # "[543 x 23]"  -> 12489
sum(is.na(OFFOFF_After))# 165  0.01321163   
drop_na(OFFOFF_After) # 12 pats

Imputed <- imputePCA(OFFOFF_After[,-1],ncp=2, scale = T)

OFFOFF_After <- OFFOFF_After %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)


OFFOFF_After <- data.frame(OFFOFF_After) %>% drop_na() %>% gather(Var, Value, OFF_3.3_S_Right1:OFF_3.17_Inf_Left_1) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
data.frame(OFFOFF_After) %>% drop_na() %>% gather(Var, Value, OFF_3.3_S_Right1:OFF_3.17_Inf_Left_1) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

OFF_before <- OFF_before %>% mutate(Worst_OFF_before=ifelse(Right>Left, "Right", ifelse(Left>Right, "Left", "Equal"))) %>% select(SUBJID, Worst_OFF_before)
OFFOFF_After <- OFFOFF_After %>% mutate(Worst_OFFOFF_After=ifelse(Right>Left, "Right", ifelse(Left>Right, "Left", "Equal"))) %>% select(SUBJID, Worst_OFFOFF_After)
OFFON_After <- OFFON_After %>% mutate(Worst_OFFON_After=ifelse(Right>Left, "Right", ifelse(Left>Right, "Left", "Equal"))) %>% select(SUBJID, Worst_OFFON_After)
ONOFF_After <- ONOFF_After %>% mutate(Worst_ONOFF_After=ifelse(Right>Left, "Right", ifelse(Left>Right, "Left", "Equal"))) %>% select(SUBJID, Worst_ONOFF_After)
ONON_After <- ONON_After %>% mutate(Worst_ONON_After=ifelse(Right>Left, "Right", ifelse(Left>Right, "Left", "Equal"))) %>% select(SUBJID, Worst_ONON_After)

Pooled <- OFF_before %>% inner_join(OFFOFF_After) %>% inner_join(OFFON_After) %>% inner_join(ONOFF_After) %>% inner_join(ONON_After)

Pooled %>% group_by(Worst_OFF_before, Worst_OFFOFF_After) %>% count()

#   Worst_OFF_before Worst_OFFOFF_After     n
# 1 Equal            Equal                  2
# 2 Equal            Left                  14
# 3 Equal            Right                 15
# 4 Left             Equal                 15
# 5 Left             Left                 208
# 6 Left             Right                 63
# 7 Right            Equal                 11
# 8 Right            Left                  77
# 9 Right            Right                132

Pooled %>% group_by(Worst_OFF_before, Worst_ONON_After) %>% count()

#   Worst_OFF_before Worst_ONON_After     n
# 1 Equal            Equal                7
# 2 Equal            Left                17
# 3 Equal            Right                7
# 4 Left             Equal               37
# 5 Left             Left               188
# 6 Left             Right               61
# 7 Right            Equal               41
# 8 Right            Left               108
# 9 Right            Right               71

# ---------------------------------
# MEDICATIONS -----------------------------------------------------------------

sheets_list <- excel_sheets(path = "Raw_Database/Asymmetry_DeepBrainStimulation.xlsx")

#  [1] "DEMOGRAPHIE "             "FACTEURSDERISQUE "        "ATCD_MED_CHIR"           
#  [4] "SOCIAL "                  "PDQ39-CGIS-SCOPA"         "PGI"                     
#  [7] "UPDRS II"                 "UPDRSIII_TOTAUX"          "UPDRSIII_COMPLET_V0_V1"  
# [10] "UPDRSI_II_IV"             "Hoehn&Yarh-S&E"           "EVA_FNM_V0_V1"           
# [13] "HAM-D"                    "HAM-A"                    "TCI_TCSP_V0"             
# [16] "Hallu_Miami"              "MoCA V0"                  "MoCA V1"                 
# [19] "Clox"                     "Boston_Fluence"           "PEROP_COMPLPEROP"        
# [22] "FREQUENCE_V0"             "FREQUENCE_V1"             "EVENEMENTSINDESIRABLES"  
# [25] "CONSO_SPE"                "PSYCHOTROPES"             "AUTRE_PARKINSON"         
# [28] "MEDICAMENTS dans Rapport" "DATES_DE_VISITES "

CONSO_SPE <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "CONSO_SPE", skip=0, col_types = "text", trim_ws = TRUE)

df_names <- names(CONSO_SPE)

df_names <- data.frame(df_names) %>%
  filter(df_names!="VISIT"&
           !grepl("INIT_PAT", df_names)&
           !grepl("D_", df_names)&
           !grepl("ENCOURS", df_names)&
           !grepl("NUM", df_names))


toString(as.list(df_names))

match <- c("SUBJID", "VISIT_NOM", "MEDICMT1", "POSO1", "CAT_TRT_ANTIPARK1", "MEDICMT2", "POSO2", "CAT_TRT_ANTIPARK2", 
           "MEDICMT3", "POSO3", "CAT_TRT_ANTIPARK3", "MEDICMT4", "POSO4", "CAT_TRT_ANTIPARK4", "MEDICMT5", 
           "POSO5", "CAT_TRT_ANTIPARK5", "MEDICMT6", "POSO6", "CAT_TRT_ANTIPARK6", "MEDICMT7", "POSO7", "CAT_TRT_ANTIPARK7", 
           "MEDICMT8", "POSO8", "CAT_TRT_ANTIPARK8", "MEDICMT9", "POSO9", "CAT_TRT_ANTIPARK9", "MEDICMT10", "POSO10", 
           "CAT_TRT_ANTIPARK10", "MEDICMT11", "POSO11", "CAT_TRT_ANTIPARK11", "MEDICMT12", "POSO12", "CAT_TRT_ANTIPARK12", 
           "MEDICMT13", "POSO13", "CAT_TRT_ANTIPARK13")
  

which_names <- which(names(CONSO_SPE) %in%  match)

CONSO_SPE <- CONSO_SPE[which_names]
CONSO_SPE <- CONSO_SPE[-1,]

CONSO_SPE %>% select(contains("CAT_TRT")) %>%
  gather(MEDS, NAMES,  CAT_TRT_ANTIPARK1 :CAT_TRT_ANTIPARK13 ) %>%
  select(NAMES) %>% distinct()

#  NAMES                  
# 1 NA                     
# 2 L-dopa                 
# 3 Agoniste dopaminergique
# 4 IMAO                   
# 5 Autre                  
# 6 ICOMT   

data.frame(CONSO_SPE %>% select(contains("POSO")) %>%
  gather(MEDS, NAMES,  POSO1:POSO13 ) %>%
  select(NAMES) %>% distinct())


data.frame(CONSO_SPE %>% select(contains("MEDICMT")) %>%
  gather(MEDS, NAMES,  MEDICMT1:MEDICMT13 ) %>%
  select(NAMES) %>% distinct())

CONSO_SPE <- CONSO_SPE %>% select(!contains("CAT_TRT"))

Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")

CONSO_SPE <- Asymmetry_Pre_vs_Post %>% select(SUBJID) %>%
  inner_join(CONSO_SPE)

fwrite(CONSO_SPE, "Processed_data/CONSO_SPE_Simplified.csv", sep=",")


CONSO_SPE_v2 <- CONSO_SPE %>% select(1,2,3,4) %>% rename("DRUG"=3, "DOSE"=4) %>%
  bind_rows(CONSO_SPE %>% select(1,2,5,6) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,7,8) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,9,10) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,11,12) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,13,14) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,15,16) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,17,18) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,19,20) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,21,22) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,23,24) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,25,26) %>% rename("DRUG"=3, "DOSE"=4))  %>%
    bind_rows(CONSO_SPE %>% select(1,2,27,28) %>% rename("DRUG"=3, "DOSE"=4))  


CONSO_SPE_v2 <- CONSO_SPE_v2 %>% drop_na()
CONSO_SPE_v2 <- CONSO_SPE_v2 %>% arrange(SUBJID, VISIT_NOM, DRUG, DOSE)

fwrite(CONSO_SPE_v2, "Processed_data/CONSO_SPE_Simplified_V2.csv", sep=",")

# ----------------------------------------------------

# STIMULATION ------------------

sheets_list <- excel_sheets(path = "Raw_Database/Asymmetry_DeepBrainStimulation.xlsx")

#  [1] "DEMOGRAPHIE "             "FACTEURSDERISQUE "        "ATCD_MED_CHIR"           
#  [4] "SOCIAL "                  "PDQ39-CGIS-SCOPA"         "PGI"                     
#  [7] "UPDRS II"                 "UPDRSIII_TOTAUX"          "UPDRSIII_COMPLET_V0_V1"  
# [10] "UPDRSI_II_IV"             "Hoehn&Yarh-S&E"           "EVA_FNM_V0_V1"           
# [13] "HAM-D"                    "HAM-A"                    "TCI_TCSP_V0"             
# [16] "Hallu_Miami"              "MoCA V0"                  "MoCA V1"                 
# [19] "Clox"                     "Boston_Fluence"           "PEROP_COMPLPEROP"        
# [22] "FREQUENCE_V0"             "FREQUENCE_V1"             "EVENEMENTSINDESIRABLES"  
# [25] "CONSO_SPE"                "PSYCHOTROPES"             "AUTRE_PARKINSON"         
# [28] "MEDICAMENTS dans Rapport" "DATES_DE_VISITES "

FREQUENCE_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "FREQUENCE_V1", skip=0, col_types = "text", trim_ws = TRUE)

df_names <- names(FREQUENCE_V1)

df_names <- data.frame(df_names) 
toString(as.list(df_names))

match <- c("SUBJID", "PLOTG1", "AMPLITUDEG1", "UNITEG1", "DUREEG1", "FREQUENCEG1", "IMPEDANCEG1", "PLOTD1", 
           "AMPLITUDED1", "UNITED1", "DUREED1", "FREQUENCED1", "IMPEDANCED1")
  
which_names <- which(names(FREQUENCE_V1) %in%  match)

FREQUENCE_V1 <- FREQUENCE_V1[which_names]
FREQUENCE_V1 <- FREQUENCE_V1[-1,]

FREQUENCE_V1 <- FREQUENCE_V1[, c(1,3,5,6,7,9,11,12,13)]

FREQUENCE_V1 <- data.frame(FREQUENCE_V1) %>% mutate_each(as.numeric, AMPLITUDEG1:IMPEDANCED1)

FREQUENCE_V1 <- na.omit(FREQUENCE_V1)

head(FREQUENCE_V1)
FREQUENCE_V1$Diff_Amp <- abs(FREQUENCE_V1$AMPLITUDED1-FREQUENCE_V1$AMPLITUDEG1)
FREQUENCE_V1$Diff_Freq <- abs(FREQUENCE_V1$FREQUENCED1 -FREQUENCE_V1$FREQUENCEG1)

mean(FREQUENCE_V1$AMPLITUDEG1) # 2.427941
mean(FREQUENCE_V1$AMPLITUDED1) # 2.464706
mean(FREQUENCE_V1$Diff_Amp)
max(FREQUENCE_V1$Diff_Amp)

mean(FREQUENCE_V1$FREQUENCEG1) # 130.6127
mean(FREQUENCE_V1$FREQUENCED1) # 130.6765
mean(FREQUENCE_V1$Diff_Freq)
max(FREQUENCE_V1$Diff_Freq)



Asymmetry_Pre_vs_Post <- fread("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")
names(Asymmetry_Pre_vs_Post)
#Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post[, c("SUBJID", "Diff_Post_OP_ONON")]

Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% inner_join(FREQUENCE_V1)


Asymmetry_Pre_vs_Post %>%  mutate(Diff_Pre_OP=ifelse(Diff_Pre_OP>=5, ">5", "<5")) %>% group_by(Diff_Pre_OP) %>% summarise(n=mean(Diff_Amp))
Asymmetry_Pre_vs_Post %>%  mutate(Diff_Pre_OP=ifelse(Diff_Pre_OP>=5, ">5", "<5")) %>% group_by(Diff_Pre_OP) %>% summarise(n=mean(Diff_Freq))


Asymmetry_Pre_vs_Post %>%  mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5, ">5", "<5")) %>% group_by(Diff_Post_OP_ONON) %>% summarise(n=mean(Diff_Amp))
Asymmetry_Pre_vs_Post %>%  mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5, ">5", "<5")) %>% group_by(Diff_Post_OP_ONON) %>% summarise(n=mean(Diff_Freq))

Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>%  mutate(Diff_Pre_OP=ifelse(Diff_Pre_OP>=5, ">5", "<5")) 

wilcox.test(Asymmetry_Pre_vs_Post$Diff_Amp[Asymmetry_Pre_vs_Post$Diff_Pre_OP==">5"], 
            Asymmetry_Pre_vs_Post$Diff_Amp[Asymmetry_Pre_vs_Post$Diff_Pre_OP=="<5"], paired=F, conf.int=F)

Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>%  mutate(Diff_Post_OP_ONON=ifelse(Diff_Post_OP_ONON>=5, ">5", "<5")) 

wilcox.test(Asymmetry_Pre_vs_Post$Diff_Amp[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON==">5"], 
            Asymmetry_Pre_vs_Post$Diff_Amp[Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON=="<5"], paired=F, conf.int=F)


Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>%  mutate(Diff_Post_OP_OFFOFF=ifelse(Diff_Post_OP_OFFOFF>=5, ">5", "<5")) 

wilcox.test(Asymmetry_Pre_vs_Post$Diff_Amp[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF==">5"], 
            Asymmetry_Pre_vs_Post$Diff_Amp[Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF=="<5"], paired=F, conf.int=F)


cor.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON, Asymmetry_Pre_vs_Post$Diff_Amp)
cor.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_ONON, Asymmetry_Pre_vs_Post$Diff_Freq)

cor.test(Asymmetry_Pre_vs_Post$Diff_Pre_OP , Asymmetry_Pre_vs_Post$Diff_Amp)
cor.test(Asymmetry_Pre_vs_Post$Diff_Pre_OP , Asymmetry_Pre_vs_Post$Diff_Freq)

cor.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF , Asymmetry_Pre_vs_Post$Diff_Amp)
cor.test(Asymmetry_Pre_vs_Post$Diff_Post_OP_OFFOFF , Asymmetry_Pre_vs_Post$Diff_Freq)


# ---------------------------------------