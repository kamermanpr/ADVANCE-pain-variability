############################################################
#                                                          #
#              Clean ADVANCE pain sites data               #
#                                                          #
############################################################

# Load packages
library(tidyverse)
library(magrittr)

# Set print options
options(max.print=999999)

# Make data-cleaned directory
if(!dir.exists('data-cleaned')) {
    dir.create('data-cleaned')
}

############################################################
#                                                          #
#                 Clean and merge datasets                 #
#                                                          #
############################################################
#-- BPI --#
## Import BPI data
sites <- readxl::read_xlsx(path = 'data-original/ADVANCE-bpi.xlsx') %>% 
    janitor::clean_names()

## Inspect BPI data
glimpse(sites)

## Clean BPI data
sites %<>% 
    select(-hurt_most_part_comments,
           -have_you_had_any_of_these_pains_every_day_or_most_days_of_the_week_for_last_three_months,
           -have_you_had_any_of_these_pains_every_day_or_most_days_of_the_week_for_last_six_months) %>% 
    rename(date_of_visit = dovisit,
           pain_in_the_last_week = have_you_had_pain_other_than_these_everyday_kinds_of_pain_during_the_last_week,
           pain_worst = please_rate_your_pain_by_circling_the_one_number_that_best_describes_your_pain_at_its_worst_in_the_last_week,
           pain_now = please_rate_your_pain_by_circling_the_one_number_that_tells_how_much_pain_you_have_right_now) %>% 
    # Ensure there are no white spaces
    mutate_at(vars(ends_with('_right'), ends_with('_left')), ~str_trim(., side = 'both')) %>% 
    # Set all to sentence case
    mutate_at(vars(ends_with('_right'), ends_with('_left')), ~str_to_sentence(.))

### Parse head
sites %<>% 
    mutate(head_pain = case_when(
        head == 'True' ~ 'Yes',
        head == 'False' ~ 'No'
    )) %>% 
    select(-head)

### Parse neck
sites %<>% 
    mutate(cervical_pain = case_when(
        neck_left == 'True' |
            neck_right == 'True' ~ 'Yes',
        neck_left == 'False' &
            neck_right == 'False' ~ 'No'
    )) %>% 
    select(-starts_with('neck'))

### Parse shoulder
sites %<>% 
    mutate(shoulder_pain = case_when(
        shoulder_left == 'True' |
            shoulder_right == 'True' ~ 'Yes',
        shoulder_left == 'False' &
            shoulder_right == 'False' ~ 'No'
    )) %>% 
    select(-shoulder_left,
           -shoulder_right)


### Parse arm (upper-arm to wrist)
sites %<>% 
    mutate(arm_pain = case_when(
        arm_left == 'True' |
            arm_right == 'True' ~ 'Yes',
        arm_left == 'False' &
            arm_right == 'False' ~ 'No'
    )) %>% 
    select(-arm_left,
           -arm_right)

### Parse hand
sites %<>% 
    mutate(hand_pain = case_when(
        hand_left == 'True' |
            hand_right == 'True' ~ 'Yes',
        hand_left == 'False' &
            hand_right == 'False' ~ 'No'
    )) %>% 
    select(-hand_left,
           -hand_right)

### Parse chest
sites %<>% 
    mutate(chest_pain = case_when(
        chest_left == 'True' |
            chest_right == 'True' ~ 'Yes',
        chest_left == 'False' &
            chest_right == 'False' ~ 'No'
    )) %>% 
    select(-chest_left,
           -chest_right)
    
### Parse abdomen
sites %<>% 
    mutate(abdominal_pain = case_when(
        abdomen_left == 'True' |
            abdomen_right == 'True' ~ 'Yes',
        abdomen_left == 'False' &
            abdomen_right == 'False' ~ 'No'
    )) %>% 
    select(-abdomen_left,
           -abdomen_right)

### Parse low back
sites %<>% 
    mutate(low_back_pain = case_when(
        lowback_left == 'True' |
            lowback_right == 'True' ~ 'Yes',
        lowback_left == 'False' &
            lowback_right == 'False' ~ 'No'
    )) %>% 
    select(-lowback_left,
           -lowback_right)

### Parse buttocks
sites %<>% 
    mutate(buttock_pain = case_when(
        buttocks_left == 'True' |
            buttocks_right == 'True' ~ 'Yes',
        buttocks_left == 'False' &
            buttocks_right == 'False' ~ 'No'
    )) %>% 
    select(-buttocks_left,
           -buttocks_right)

### Parse hip/groin
sites %<>% 
    mutate(hip_groin_pain = case_when(
        hip_groin_left == 'True' |
            hip_groin_right == 'True' ~ 'Yes',
        hip_groin_left == 'False' &
            hip_groin_right == 'False' ~ 'No'
    )) %>% 
    select(-hip_groin_left,
           -hip_groin_right)

### Parse leg (thigh to ankle)
sites %<>% 
    mutate(leg_pain = case_when(
        leg_left == 'True' |
            leg_right == 'True' |
            knee_left == 'True' |
            knee_right == 'True' ~ 'Yes',
        leg_left == 'False' &
            leg_right == 'False' &
            knee_left == 'False' &
            knee_right == 'False' ~ 'No'
    )) %>% 
    select(-leg_left,
           -leg_right,
           -knee_left,
           -knee_right)

### Parse genitals
sites %<>% 
    mutate(genital_pain = case_when(
        genitals == 'True' ~ 'Yes',
        genitals == 'False' ~ 'No'
    )) %>% 
    select(-genitals)

### Parse foot pain
sites %<>% 
    mutate(foot_pain = case_when(
        feet_left == 'True' |
            feet_right == 'True' ~ 'Yes',
        feet_left == 'False' &
            feet_right == 'False' ~ 'No'
    )) %>% 
    select(-feet_left,
           -feet_right)

sites %<>% 
    # Include all combinations of visit and ranid
    complete(ranid, interval_name) %>% 
    # Retain only enrolment to visit 9
    filter(interval_name %in% c('Enrolment', 'Visit  1', 'Visit  2',
                                 'Visit  3', 'Visit  4', 'Visit  5', 
                                 'Visit  6', 'Visit  7', 'Visit  8',
                                 'Visit  9/EOS'))
    
unique(sites$interval_name)
length(unique(sites$ranid))
nrow(sites)

#-- Site of worst pain --#
sites2 <- readxl::read_xlsx(path = 'data-original/ADVANCE-bpi.xlsx') %>% 
    janitor::clean_names()

## Inspect BPI data
glimpse(sites2)

## Clean BPI data
sites2 %<>% 
    select(ranid, interval_name, where_does_it_hurt_most) 

## Clean site of worst pain
sites2 %<>% 
    rename(where = where_does_it_hurt_most) %>% 
    mutate(site_worst = case_when(
        str_detect(where, 'Head') ~ 'Head',
        str_detect(where, 'Neck') ~ 'Neck',
        str_detect(where, 'Chest') ~ 'Chest',
        str_detect(where, 'Abdomen') ~ 'Abdomen',
        str_detect(where, 'Low back') ~ 'Low back',
        str_detect(where, 'Genitals') ~ 'Genitals',
        str_detect(where, 'Buttocks') ~ 'Buttocks',
        str_detect(where, 'Shoulder') ~ 'Shoulder',
        str_detect(where, 'Arm') ~ 'Arm',
        str_detect(where, 'Hand') ~ 'Hand',
        str_detect(where, 'Hip') ~ 'Hip/groin',
        str_detect(where, 'Leg') ~ 'Leg',
        str_detect(where, 'Knee') ~ 'Leg',
        str_detect(where, 'Feet') ~ 'Feet',
    )) %>% 
    select(-where)
 
sites2 %<>% 
    # Include all combinations of visit and ranid
    complete(ranid, interval_name) %>% 
    # Retain only enrolment to visit 9
    filter(interval_name %in% c('Enrolment', 'Visit  1', 'Visit  2',
                                'Visit  3', 'Visit  4', 'Visit  5', 
                                'Visit  6', 'Visit  7', 'Visit  8',
                                'Visit  9/EOS'))

unique(sites2$interval_name)
length(unique(sites2$ranid))
nrow(sites2)   

#-- Demographics --#
## Import demographic data
demo <- readxl::read_xlsx(path = 'data-original/ADVANCE-demographics.xlsx') %>% 
    janitor::clean_names()

## Inspect demographic data
glimpse(demo)

## Clean demographic data
demo %<>% 
    select(ranid,
           age,
           sex,
           race,
           educationlevel,
           employmentstatus) %>% 
    rename(employment_status = employmentstatus,
           education = educationlevel,
           ancestry = race) %>% 
    mutate(ancestry = stringr::str_to_sentence(ancestry),
           ancestry = ifelse(ancestry == 'Colored',
                             yes = 'Coloured',
                             no = ancestry)) %>% 
    mutate(employment_status = str_trim(employment_status),
           employment_status = str_to_title(employment_status)) %>% 
    mutate(education = str_trim(education)) %>% 
    mutate(sex = ifelse(sex == 'MALE',
                        yes = 'Male',
                        no = 'Female'))

length(unique(demo$ranid))

#-- CD4 T-cell count --#
## Import CD4 data
cd4 <- readxl::read_xlsx(path = 'data-original/ADVANCE-cd4-all.xlsx') %>% 
    janitor::clean_names()

## Inspect CD4 data
glimpse(cd4)

## Clean CD4 data
cd4 %<>% 
    select(ranid,
           visit,
           result) %>% 
    rename(cd4 = result) %>% 
    # Remove 'ND' values
    filter(cd4 != 'ND')

cd4 %<>% 
    # Fix visits column to match that of the sites data
    mutate(visit = case_when(
        str_detect(visit, pattern = 'screening') ~ 'Visit 0 W00',
        str_detect(visit, pattern = 'Visit 9 EOS') ~ 'Visit 9 W96',
        TRUE ~ visit
    )) %>%
    separate(col = visit,
             into = c('visit', 'number', 'weeks'),
             sep = ' ') %>% 
    select(-weeks) %>% 
    unite(col = 'visit', visit, number, sep = '  ') %>% 
    mutate(visit = case_when(
        str_detect(visit, pattern = 'Visit  0') ~ 'Enrolment',
        str_detect(visit, pattern = 'Visit  9') ~ 'Visit  9/EOS',
        TRUE ~ visit
    )) %>% 
    rename(interval_name = visit,
           cd4_cells.ul = cd4) %>% 
    # Include all combinations of visit and ranid
    complete(ranid, interval_name) %>% 
    # Retain only enrolment to visit 9
    filter(interval_name %in% c('Enrolment', 'Visit  1', 'Visit  2',
                                'Visit  3', 'Visit  4', 'Visit  5', 
                                'Visit  6', 'Visit  7', 'Visit  8',
                                'Visit  9/EOS'))

unique(cd4$interval_name)
length(unique(cd4$ranid))
nrow(cd4)

# Find duplicates
cd4 %>% 
    group_by(ranid, interval_name) %>% 
    summarise(count = n()) %>% 
    filter(count > 1)

# Extract distinct rows
cd4 %<>%
    distinct()

# Find duplicates again
cd4 %>% 
    group_by(ranid, interval_name) %>% 
    summarise(count = n()) %>% 
    filter(count > 1) 

# Check duplicates
cd4[cd4$ranid == '01-0396', ]
cd4[cd4$ranid == '01-0520', ]
cd4[cd4$ranid == '01-0722', ]

# Eliminate duplicates by removing the repeat (second value)
cd4 %<>% 
    group_by(ranid, interval_name) %>% 
    mutate(row = row_number()) %>% 
    filter(!row > 1) %>% 
    select(-row)

nrow(cd4)

#-- Viral load --#
## Import viral load data
vl <- readxl::read_xlsx(path = 'data-original/ADVANCE-viral-load-all.xlsx') %>% 
     janitor::clean_names()

## Inspect CD4 data
glimpse(vl)

## Clean CD4 data
vl %<>% 
    select(ranid,
           visit,
           result) %>% 
    rename(viral_load = result)

vl %<>% 
    # Fix visits column to match that of the sites data
    mutate(visit = case_when(
        visit == 'Screening' ~ 'Visit 0 Week 00',
        TRUE ~ visit
    )) %>% 
    separate(col = visit,
             into = c('visit', 'number', 'weeks', 'weeks2'),
             sep = ' ') %>% 
    select(-starts_with('weeks')) %>% 
    unite(col = 'visit', visit, number, sep = '  ') %>% 
    mutate(visit = case_when(
        str_detect(visit, pattern = 'Visit  0') ~ 'Enrolment',
        str_detect(visit, pattern = 'Visit  9') ~ 'Visit  9/EOS',
        TRUE ~ visit
    )) %>% 
    rename(interval_name = visit,
           viral_load_cp.ml = viral_load) %>% 
    mutate(viral_load_cp.ml = case_when(
        viral_load_cp.ml == 'Not detected' ~ '50',
        viral_load_cp.ml == 'ND' ~ '50',
        viral_load_cp.ml == '< 40' ~ '50',
        TRUE ~ viral_load_cp.ml
    )) %>% 
    # Include all combinations of visit and ranid
    complete(ranid, interval_name) %>% 
    # Retain only enrolment to visit 9
    filter(interval_name %in% c('Enrolment', 'Visit  1', 'Visit  2',
                                'Visit  3', 'Visit  4', 'Visit  5', 
                                'Visit  6', 'Visit  7', 'Visit  8',
                                'Visit  9/EOS'))

unique(vl$interval_name)
length(unique(vl$ranid))
nrow(vl)

# Find duplicates
vl %>% 
    group_by(ranid, interval_name) %>% 
    summarise(count = n()) %>% 
    filter(count > 1)

# Extract distinct rows
vl %<>%
    distinct()

# Find duplicates again
vl %>% 
    group_by(ranid, interval_name) %>% 
    summarise(count = n()) %>% 
    filter(count > 1)

# Check duplicates
vl[vl$ranid == '01-0520', ]

# Correct duplicates by removing the repeat (second value)
vl %<>% 
    group_by(ranid, interval_name) %>% 
    mutate(row = row_number()) %>% 
    filter(!row > 1) %>% 
    select(-row)

nrow(vl)

#-- Modified Mini Screen (MMS) --#
## Import MMS data
mms <- readxl::read_xlsx(path = 'data-original/ADVANCE-mental-health.xlsx') %>% 
    janitor::clean_names()

## Inspect MMS data
glimpse(mms)

## Clean MMS data
mms %<>% 
    select(-visit_date, -sex, -comments) %>% 
    # Fix visits column to match that of the sites data
    mutate(visit = case_when(
        str_detect(visit, pattern = 'Enrolment') ~ 'Visit 0 W00',
        str_detect(visit, pattern = 'EOS') ~ 'Visit 9 W98',
        TRUE ~ visit
    )) %>% 
    separate(col = visit,
             into = c('visit', 'number', 'weeks'),
             sep = ' ') %>% 
    select(-weeks) %>% 
    unite(col = 'visit', visit, number, sep = '  ') %>% 
    mutate(visit = case_when(
               str_detect(visit, pattern = 'Visit  0') ~ 'Enrolment',
               str_detect(visit, pattern = 'Visit  9') ~ 'Visit  9/EOS',
               TRUE ~ visit
           )) %>% 
    rename(interval_name = visit) %>% 
    # Calculate MMS score
    mutate_at(4:25, ~ifelse(. == 'No',
                            yes = 0,
                            no = 1)) %>% 
    mutate(mms_total = rowSums(.[4:25])) %>% 
    select(ranid, interval_name, group, mms_total) %>% 
    # Include all combinations of visit and ranid
    complete(ranid, interval_name) %>% 
    # Retain only enrolment to visit 9
    filter(interval_name %in% c('Enrolment', 'Visit  1', 'Visit  2',
                                'Visit  3', 'Visit  4', 'Visit  5', 
                                'Visit  6', 'Visit  7', 'Visit  8',
                                'Visit  9/EOS'))

unique(mms$interval_name)
length(unique(mms$ranid))
nrow(mms)
    
#-- TB screening --#
## Import TB data
tb <- readxl::read_xlsx(path = 'data-original/ADVANCE-tb-screening.xlsx') %>% 
    janitor::clean_names()

## Inspect tb data
glimpse(tb)

## Clean TB data
tb %<>% 
    select(ranid, group, visit, result) %>% 
    # Fix visits column to match that of the sites data
    mutate(visit = case_when(
        str_detect(visit, pattern = 'Enrolment') ~ 'Visit 0 W00',
        str_detect(visit, pattern = 'EOS') ~ 'Visit 9 W98',
        TRUE ~ visit
    )) %>% 
    separate(col = visit,
             into = c('visit', 'number', 'weeks'),
             sep = ' ') %>% 
    select(-weeks) %>% 
    unite(col = 'visit', visit, number, sep = '  ') %>% 
    mutate(visit = case_when(
        str_detect(visit, pattern = 'Visit  0') ~ 'Enrolment',
        str_detect(visit, pattern = 'Visit  9') ~ 'Visit  9/EOS',
        TRUE ~ visit
    )) %>% 
    rename(interval_name = visit,
           tb_screen = result) %>% 
    # Include all combinations of visit and ranid
    complete(ranid, interval_name) %>% 
    # Retain only enrolment to visit 9
    filter(interval_name %in% c('Enrolment', 'Visit  1', 'Visit  2',
                                'Visit  3', 'Visit  4', 'Visit  5', 
                                'Visit  6', 'Visit  7', 'Visit  8',
                                'Visit  9/EOS'))
    
unique(tb$interval_name)
length(unique(tb$ranid))
nrow(tb)

# Check unique ranid
unique(tb$ranid)

# Remove rows with non-"0X-XXXX" format
tb %<>% 
    filter(str_detect(ranid, pattern = '^0'))

unique(tb$interval_name)
length(unique(tb$ranid))
nrow(tb)

#-- General health perception --#
## Import general health data
qol <- readxl::read_xlsx(path = 'data-original/ADVANCE-QoL-domain-score.xlsx') %>% 
    janitor::clean_names()

## Inspect general health data
glimpse(qol)

## Clean general health data
qol %<>% 
    select(ranid, visit, gen) %>% 
    # Fix visits column to match that of the sites data
    mutate(visit = case_when(
        str_detect(visit, pattern = 'Enrolment') ~ 'Visit 0 W00',
        TRUE ~ visit
    )) %>% 
    separate(col = visit,
             into = c('visit', 'number', 'weeks'),
             sep = ' ') %>% 
    select(-weeks) %>% 
    unite(col = 'visit', visit, number, sep = '  ') %>% 
    mutate(visit = case_when(
        str_detect(visit, pattern = 'Visit  0') ~ 'Enrolment',
        str_detect(visit, pattern = 'Visit  9') ~ 'Visit  9/EOS',
        TRUE ~ visit
    )) %>% 
    rename(interval_name = visit,
           general_health = gen) %>% 
    # Include all combinations of visit and ranid
    complete(ranid, interval_name) %>% 
    # Retain only enrolment to visit 9
    filter(interval_name %in% c('Enrolment', 'Visit  1', 'Visit  2',
                                'Visit  3', 'Visit  4', 'Visit  5', 
                                'Visit  6', 'Visit  7', 'Visit  8',
                                'Visit  9/EOS'))

unique(qol$interval_name)
length(unique(qol$ranid))
nrow(qol)

# Check unique ranid
unique(qol$ranid)

# Remove rows with non-"0X-XXXX" format
qol %<>% 
    filter(!str_detect(ranid, pattern = 'Deleted'))

unique(qol$interval_name)
length(unique(qol$ranid))
nrow(qol)

#-- Join datasets --#
df <- sites %>% 
    left_join(sites2) %>% 
    left_join(demo) %>% 
    left_join(cd4) %>% 
    left_join(vl) %>% 
    left_join(tb) %>%
    left_join(qol) %>% 
    left_join(mms) 

#-- Final fixes following a visual inspection --#
# Missing site_name data
df %<>% 
    group_by(ranid) %>% 
    fill(site_name, .direction = 'updown')

# Missing group data
df %<>%
    group_by(ranid) %>% 
    fill(group, .direction = 'updown')

df %<>% ungroup()

# cd4 and viral load to numeric
df %<>% 
    mutate(cd4_cells.ul = as.numeric(cd4_cells.ul),
           viral_load_cp.ml = as.numeric(viral_load_cp.ml))

############################################################
#                                                          #
#            Remove incongruencies in the data             #
#                                                          #
############################################################

############################################################
#                                                          #
#               Pain was defined as _pain in               #
#             the last week_  being 'Yes', and             #
#              _pain at its worst_ being > 0.              #
#           These two measurements were then the           #
#             "gatekeeper" measurements, such              #
#                that the two measurements                 #
#            both had to be positive ('Yes' and            #
#         '> 0', respectively) in order for there          #
#              to be any entries for _site of              #
#           pain_ and _site of worst pain_. Were           #
#       the data were incongruent (e.g., when there        #
#              was no _pain in the last week_              #
#               and _pain at its worst_ = 0,               #
#           but there were entries for _site of            #
#          pain_ and _site of worst pain_), then           #
#             the _site of pain_ and _site of              #
#   worst pain_ entries were marked as **incongruent**.    #
#                 Data also were considered                #
#       **incongruent** when _pain in the last week_       #
#            = 'Yes', but _site of worst pain_             #
#              = 'None'.   Lastly, data were               #
#   considered **incongruent** when _site of worst pain_   #
#               was not listed as one of the               #
#       pain locations for a given measurement week.       #
#            For analyses purposes, missing data           #
#            in the _site of pain_ columns were            #
#         changed to **'No'** (pain not present in         #
#      the site). This approach was conservative, but      #
#         we believed that the approach would have         #
#          the least effect on the outcome, while          #
#    still retaining as many participants as possible.     #
#                                                          #
############################################################

#---Define pain: pain_in_the_last_week + pain_worst > 0 ---#
df %<>% 
    # Recode pain_in_the_last_week based on:
    # pain_in_the_last_week == Yes & pain_worst > 0
    mutate(pain_in_the_last_week = ifelse(pain_in_the_last_week == 'Yes' &
                                              pain_worst > 0,
                                          yes = 'Yes',
                                          no = 'No')) %>% 
    # Recode worst pain as <NA> if NRS = 0
    mutate(pain_worst = ifelse(pain_worst == 0,
                               yes = NA,
                               no = pain_worst))

#- Remove individuals (ranid) with incomplete pain_in_the_last_week --#
# Generate ranid filter for those with complete pain_in_the_last_week data
df_filter <- df %>% 
    select(ranid, interval_name, pain_in_the_last_week) %>% 
    # Extract complete cases for the period 0 to 48 weeks
    # Remove unneeded intervals
    filter(!interval_name %in% c('Visit  1', 'Visit  6', 'Visit  7',
                                 'Visit  8', 'Visit  9/EOS')) %>% 
    # Change class of interval_name and pain_in_the_last_week variables
    mutate(interval_name = factor(interval_name, ordered = TRUE),
           pain_in_the_last_week = factor(pain_in_the_last_week)) %>% 
    # Retain complete cases only
    pivot_wider(names_from = interval_name,
                values_from = pain_in_the_last_week) %>% 
    filter(complete.cases(.))

# Generate a filtering vector of ranid
vec_filter <- unique(df_filter$ranid)

# Clean and process data
## COMPLETE DATA
df_complete <- df %>% 
    # Remove unneeded intervals
    filter(!interval_name %in% c('Visit  1', 'Visit  6', 'Visit  7',
                                 'Visit  8', 'Visit  9/EOS')) %>% 
    # Key step: retain those in the filtering vector created above
    filter(ranid %in% vec_filter) %>% 
    # Fix interval names
    mutate(interval_name = case_when(
        interval_name == 'Enrolment' ~ '0 weeks',
        interval_name == 'Visit  2' ~ '12 weeks',
        interval_name == 'Visit  3' ~ '24 weeks',
        interval_name == 'Visit  4' ~ '36 weeks',
        interval_name == 'Visit  5' ~ '48 weeks'
    )) %>%
    # Order weeks
    mutate(interval_name = factor(interval_name,
                                  levels = c('0 weeks', '12 weeks',
                                             '24 weeks', '36 weeks', '48 weeks'),
                                  ordered = TRUE)) %>% 
    # Interval_name as numeric
    mutate(interval_numeric = str_extract(as.character(interval_name), 
                                          pattern = '[0-9]?[0-9]'),
           interval_numeric = as.numeric(interval_numeric)) %>% 
    # Convert pain_worst == <NA> to 0 pain
    mutate(pain_worst = ifelse(pain_in_the_last_week == 'No' & 
                                   is.na(pain_worst),
                               yes = 0,
                               no = pain_worst)) %>% 
    # Convert all *_pain <NA> to "No"
    ## Conservative analysis approach
    mutate_at(vars(ends_with('_pain')), 
              ~ ifelse(is.na(.),
                       yes = 'No',
                       no = .)) %>% 
    # Mark incongruent cases with no pain but having pain sites 
    mutate(site_worst = ifelse(pain_in_the_last_week == 'No' & 
                                   pain_worst == 0 &
                                   !is.na(site_worst),
                               yes = 'Incongruent',
                               no = site_worst)) %>% 
    mutate(site_worst = ifelse(is.na(site_worst),
                               yes = 'None',
                               no = site_worst)) %>% 
    # Incongruency marker: pain_in_the_last_week = 'No' and *_pain = 'Yes'
    mutate_at(vars(ends_with('_pain')), 
              ~ ifelse(pain_in_the_last_week == 'No' & . == 'Yes',
                       yes = 'Incongruent',
                       no = .)) 

## INCOMPLETE DATA
df_incomplete <- df %>% 
    # Remove unneeded intervals
    filter(!interval_name %in% c('Visit  1', 'Visit  6', 'Visit  7',
                                 'Visit  8', 'Visit  9/EOS')) %>% 
    # Key step: remove those in the filtering vector created above
    filter(!ranid %in% vec_filter) %>% 
    # Fix interval names
    mutate(interval_name = case_when(
        interval_name == 'Enrolment' ~ '0 weeks',
        interval_name == 'Visit  2' ~ '12 weeks',
        interval_name == 'Visit  3' ~ '24 weeks',
        interval_name == 'Visit  4' ~ '36 weeks',
        interval_name == 'Visit  5' ~ '48 weeks'
    )) %>%
    # Order weeks
    mutate(interval_name = factor(interval_name,
                                  levels = c('0 weeks', '12 weeks',
                                             '24 weeks', '36 weeks', '48 weeks'),
                                  ordered = TRUE)) %>% 
    # Interval_name as numeric
    mutate(interval_numeric = str_extract(as.character(interval_name), 
                                          pattern = '[0-9]?[0-9]'),
           interval_numeric = as.numeric(interval_numeric)) %>% 
    # Convert pain_worst == <NA> to 0 pain
    mutate(pain_worst = ifelse(pain_in_the_last_week == 'No' & 
                                   is.na(pain_worst),
                               yes = 0,
                               no = pain_worst)) %>% 
    # Convert all *_pain <NA> to "No"
    ## Conservative analysis approach
    mutate_at(vars(ends_with('_pain')), 
              ~ ifelse(is.na(.),
                       yes = 'No',
                       no = .)) %>% 
    # Mark incongruent cases with no pain but having pain sites 
    mutate(site_worst = ifelse(pain_in_the_last_week == 'No' & 
                                   pain_worst == 0 &
                                   !is.na(site_worst),
                               yes = 'Incongruent',
                               no = site_worst)) %>% 
    mutate(site_worst = ifelse(is.na(site_worst),
                               yes = 'None',
                               no = site_worst)) %>% 
    # Incongruency marker: pain_in_the_last_week = 'No' and *_pain = 'Yes'
    mutate_at(vars(ends_with('_pain')), 
              ~ ifelse(pain_in_the_last_week == 'No' & . == 'Yes',
                       yes = 'Incongruent',
                       no = .)) 

# Make incongruency filter
## See last steps above
vec_incongruency <- df_complete %>% 
    filter(site_worst == 'Incongruent') %>% 
    .$ranid

# Make second incongruency filter
## When pain_in_the_last_week = 'Yes' & pain_worst >0, but worst_site = 'None'
vec_incongruency2 <- df_complete %>% 
    filter(pain_in_the_last_week == 'Yes' & pain_worst > 0) %>% 
    filter(pain_in_the_last_week == 'Yes' & site_worst == 'None') %>% 
    .$ranid

# Make third incongruency filter
## When pain_in_the_last_week = 'Yes' & pain_worst > 0, but no. of pain sites = 0
vec_incongruency3 <- df_complete %>% 
    filter(pain_in_the_last_week == 'Yes' & pain_worst > 0) %>%  
    pivot_longer(cols = ends_with('_pain'),
                 names_to = 'sites',
                 values_to = 'values') %>% 
    mutate(values = ifelse(values == 'Yes',
                           yes = TRUE,
                           no = FALSE)) %>% 
    group_by(ranid, interval_name, pain_in_the_last_week) %>% 
    summarise(sum = sum(values)) %>% 
    ungroup() %>% 
    filter(sum == 0) %>% 
    .$ranid %>% 
    unique(.)

# Make fourth incongruency filter
## When worst pain sites not found in the list of pain sites in a matching week
# Convert 'Yes' to site name
vec_incongruency4 <- df_complete %>% 
    mutate(head_pain = ifelse(head_pain == 'Yes',
                              yes = 'Head',
                              no = 'No')) %>% 
    mutate(cervical_pain = ifelse(cervical_pain == 'Yes',
                                  yes = 'Neck',
                                  no = 'No')) %>% 
    mutate(shoulder_pain = ifelse(shoulder_pain == 'Yes',
                                  yes = 'Shoulder',
                                  no = 'No')) %>% 
    mutate(arm_pain = ifelse(arm_pain == 'Yes',
                             yes = 'Arm',
                             no = 'No')) %>% 
    mutate(hand_pain = ifelse(hand_pain == 'Yes',
                              yes = 'Hand',
                              no = 'No')) %>% 
    mutate(chest_pain = ifelse(chest_pain == 'Yes',
                               yes = 'Chest',
                               no = 'No')) %>% 
    mutate(abdominal_pain = ifelse(abdominal_pain == 'Yes',
                                   yes = 'Abdomen',
                                   no = 'No')) %>% 
    mutate(low_back_pain = ifelse(low_back_pain == 'Yes',
                                  yes = 'Low back',
                                  no = 'No')) %>% 
    mutate(buttock_pain = ifelse(buttock_pain == 'Yes',
                                 yes = 'Buttocks',
                                 no = 'No')) %>% 
    mutate(hip_groin_pain = ifelse(hip_groin_pain == 'Yes',
                                   yes = 'Hip/groin',
                                   no = 'No')) %>% 
    mutate(leg_pain = ifelse(leg_pain == 'Yes',
                             yes = 'Leg',
                             no = 'No')) %>% 
    mutate(genital_pain = ifelse(genital_pain == 'Yes',
                                 yes = 'Genitals',
                                 no = 'No')) %>% 
    mutate(foot_pain = ifelse(foot_pain == 'Yes',
                              yes = 'Feet',
                              no = 'No')) %>% 
    unite(col = 'united', 
          ends_with('_pain'),
          sep = ' ') %>% 
    filter(pain_in_the_last_week == 'Yes' & pain_worst > 0) %>% 
    mutate(count = str_count(united, 
                             pattern = site_worst)) %>% 
    filter(count == 0) %>% 
    .$ranid %>% 
    unique(.)

# Retain incongruent data
vec_incomplete <- unique(c(vec_incongruency, vec_incongruency2, 
                           vec_incongruency3, vec_incongruency4))

df_incomplete2 <- df_complete %>% 
    filter(ranid %in% vec_incomplete)

df_incomplete3 <- rbind(df_incomplete, df_incomplete2) %>% 
    mutate(any_missing = 'Yes')

# Remove incongruent data
df_complete %<>%
    filter(!ranid %in% vec_incongruency) %>% 
    filter(!ranid %in% vec_incongruency2) %>% 
    filter(!ranid %in% vec_incongruency3) %>% 
    filter(!ranid %in% vec_incongruency4) %>% 
    mutate(any_missing = 'No') 

# Bind datasets
df_joined <- rbind(df_complete, df_incomplete3)

## Entries with missing data
length(vec_filter)

## Entries with incongruent data (once entries with missing data removed)
length(unique(c(vec_incongruency, vec_incongruency2, 
                vec_incongruency3, vec_incongruency4)))

# Inspect data
## COMPLETE
xtabs(~interval_name, data = df_complete)

## INCOMPLETE
xtabs(~interval_name, data = df_incomplete3)

## JOINED
xtabs(~interval_name, data = df_joined)

############################################################
#                                                          #
#                      Write to file                       #
#                                                          #
############################################################
write_csv(df_joined, path = 'data-cleaned/data-ADVANCE.csv')
write_rds(df_joined, path = 'data-cleaned/data-ADVANCE.rds')

