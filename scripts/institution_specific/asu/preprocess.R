library(data.table)
library(tidyverse)

# Load Full Data
Full.Data = fread("F://full_data_SPR21ENRL_2020-12-18.csv")

# Restrict to UGs
Data.Set = Full.Data[acad_level %in% c("Freshman", "Sophomore", "Junior", "Senior")]

# Focus on recent FALL terms 2013-2019
.target_terms = c(2137, 2147, 2157, 2167, 2177, 2187, 2197)
Data.Set = Data.Set[strm %in% .target_terms]

# Select relevant variables
Data.Set.Sub = Data.Set %>%
    select(emplid_anon, strm, acad_level, course_code, unt_taken, crse_grade_off, cgrade, passing, complete, cum_gpa,
           trnsfr_credit_accpt, age_at_class_start, hs_gpa, act_eng_max, act_math_max, sat_math_max, sat_verb_max,
           cip_family, cip_code, ssr_component, crse_modality, stud_modality, gender, ethnicity, underrep, school_year,
           acad_major_1, acad_major_2, acad_minor_1, acad_minor_2)

# Compute simple Student-Term level variables for Freshman
tmp = Data.Set.Sub %>%
    filter(acad_level == "Freshman") %>%
    group_by(emplid_anon, strm) %>%
    summarise(
        units = sum(unt_taken, na.rm=T),
        units_failed = sum(unt_taken[crse_grade_off %in% c("E","EN","EU","X","XE")], na.rm=T),
        units_incompleted = sum(unt_taken[crse_grade_off == "I"], na.rm=T),
        units_withdrawn = sum(unt_taken[crse_grade_off == "W"], na.rm=T),
        gpa_cumulative = cum_gpa[1],
        units_transferred = trnsfr_credit_accpt[1],
        age = age_at_class_start[1],
        gpa_high_school = hs_gpa[1],
        act_english = act_eng_max[1],
        act_math = act_math_max[1],
        sat_math = sat_math_max[1],
        sat_verbal = sat_verb_max[1],
        gpa_avg = weighted.mean(cgrade, unt_taken, na.rm = T),
        gpa_stddev = sum(unt_taken * (cgrade-weighted.mean(cgrade, unt_taken, na.rm = T))^2, na.rm=T)/sum(unt_taken),
        units_modality_inperson = sum(unt_taken[crse_modality == "F2F"], na.rm=T),
        units_modality_online = sum(unt_taken[crse_modality %in% c("ASUO","ICOURSE")], na.rm=T),
        modality = stud_modality[1],
        sex = gender[1],
        ethnicity = ethnicity[1],
        urm_status = underrep[1],
        cip6_major_1 = acad_major_1[1], # need CIP6
        cip6_major_2 = acad_major_2[1], # need CIP6
        cip6_minor_1 = acad_minor_1[1], # need CIP6
        cip6_minor_2 = acad_minor_2[1], # need CIP6
        year = school_year[1]
    )

head(tmp)

# Compute more complex variables

# Next year retention (outcome)
retention = Data.Set.Sub %>%
    group_by(emplid_anon, nextyr_strm=strm) %>%
    summarise(
        strm = nextyr_strm[1]-10,
        retention = as.integer(sum(unt_taken)>0)
    ) %>%
    ungroup %>%
    select(-nextyr_strm)

# Units by cip2 code
units_cip2 = Data.Set.Sub %>%
    filter(acad_level == "Freshman") %>%
    group_by(emplid_anon, strm, units_cip2=cip_family) %>%
    summarise(
        units = sum(unt_taken, na.rm=T)
    ) %>%
    pivot_wider(names_from = units_cip2, values_from = units, names_prefix = "units_cip2_")

# Units by course type
units_type = Data.Set.Sub %>%
    filter(acad_level == "Freshman") %>%
    mutate(
        type = ifelse(ssr_component%in%c("FLD","IND","PRA","PRO","RSC"), "OTHER",
                               ifelse(ssr_component=="STO", "LAB",
                                      ifelse(ssr_component=="REC", "DIS",
                                             ifelse(ssr_component=="LEL", "LAB-LEC", ssr_component))))
    ) %>%
    group_by(emplid_anon, strm, course_code) %>%
    summarise(
        units_type = paste(sort(unique(type)), collapse = "-"),
        units = sum(unt_taken, na.rm=T)
    ) %>%
    group_by(emplid_anon, strm, units_type) %>%
    summarise(
        units = sum(units, na.rm=T)
    ) %>%
    pivot_wider(names_from = units_type, values_from = units, names_prefix = "units_type_")

# GPA zscore
gpa_zscore = Data.Set.Sub %>%
    group_by(course_code, strm) %>%
    mutate(
        cgrade_z = scale(cgrade)
    ) %>%
    filter(acad_level == "Freshman") %>%
    group_by(emplid_anon, strm) %>%
    summarise(
        gpa_zscore_avg = weighted.mean(cgrade_z, unt_taken, na.rm = T),
        gpa_zscore_stddev = sum(unt_taken * (cgrade_z-weighted.mean(cgrade_z, unt_taken, na.rm = T))^2, na.rm=T)/sum(unt_taken),
    )

### Add complex vars into data frame

tmp2 = tmp %>%
    left_join(retention) %>%
    left_join(units_cip2) %>%
    left_join(units_type) %>%
    left_join(gpa_zscore)

# Impute zeros for empty unit counts    
tmp3 = tmp2 %>% 
    mutate_at(
        vars(starts_with("units"), retention),
        ~ replace(., is.na(.), 0)
    )

# Export Data
saveRDS(tmp3, file = "output.rds")
