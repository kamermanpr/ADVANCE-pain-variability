library(tidyverse)

data <- readxl::read_xlsx('data-original/ADVANCE-bpns-all.xlsx')
data

# Extract
df_enrol <- data %>% 
    filter(visit == 'Enrolment') %>% 
    select(-group, -visit_date, -sex, -desc_pain, -comment)

df_48 <- data %>% 
    filter(visit == 'Visit 5 W48') %>% 
    select(-group, -visit_date, -sex, -desc_pain, -comment)

# Levels
unique(df_enrol$dip_rtoe)
unique(df_enrol$dip_ltoe)
unique(df_enrol$reflex_right)
unique(df_enrol$reflex_left)

unique(df_48$dip_rtoe)
unique(df_48$dip_ltoe)
unique(df_48$reflex_right)
unique(df_48$reflex_left)

# Clean
foo <- df_enrol %>% 
    mutate_at(3:8, ~ifelse(. == 11,
                           yes = 0,
                           no = .)) %>% 
    mutate_at(9:10, ~case_when(
        . == 'felt 6-10 seconds (mild loss)' ~ 0,
        . == 'felt <5 seconds (moderate loss)' ~ 0,
        . == 'not felt (severe loss)' ~ 0,
        . == 'felt >10 seconds (normal)' ~ 1
    )) %>% 
    mutate_at(11:12, ~case_when(
        . == 'Present' ~ 1,
        . == 'Absent' ~ 0
    )) %>% 
    mutate(bilateral_dip = dip_rtoe + dip_ltoe,
           bilateral_reflex = reflex_right + reflex_left,
           neuropathy_signs = ifelse(bilateral_dip == 0 | bilateral_reflex == 0,
                                     yes = 'yes',
                                     no = 'no'))

unique(foo$neuropathy_signs)
foo <- foo %>% 
    filter(!is.na(neuropathy_signs))
nrow(foo)
mean(foo$neuropathy_signs == 'yes', na.rm = TRUE)

bar <- df_48 %>% 
    mutate_at(3:8, ~ifelse(. == 11,
                           yes = 0,
                           no = .)) %>% 
    mutate_at(9:10, ~case_when(
        . == 'felt 6-10 seconds (mild loss)' ~ 0,
        . == 'felt <5 seconds (moderate loss)' ~ 0,
        . == 'not felt (severe loss)' ~ 0,
        . == 'felt >10 seconds (normal)' ~ 1
    )) %>% 
    mutate_at(11:12, ~case_when(
        . == 'Present' ~ 1,
        . == 'Absent' ~ 0
    )) %>% 
    mutate(bilateral_dip = dip_rtoe + dip_ltoe,
           bilateral_reflex = reflex_right + reflex_left,
           neuropathy_signs = ifelse(bilateral_dip == 0 | bilateral_reflex == 0,
                                     yes = 'yes',
                                     no = 'no'))

unique(bar$neuropathy_signs)
nrow(bar)
mean(bar$neuropathy_signs == 'yes', na.rm = TRUE)


    
    