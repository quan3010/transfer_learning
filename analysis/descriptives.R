## Descriptives of models for paper
## RFK Jan 8, 2022

# install.packages("tidyverse")
# install.packages("scales")
library(tidyverse)
library(scales)

# Get all descriptives
a = bind_rows(
    read.csv("../metrics/asu_train_descriptivestats.csv"),
    read.csv("../metrics/asuo_train_descriptivestats.csv"),
    read.csv("../metrics/uci_train_descriptivestats.csv"),
    read.csv("../metrics/um_train_descriptivestats.csv"),
    read.csv("../metrics/asu_test_descriptivestats.csv"),
    read.csv("../metrics/asuo_test_descriptivestats.csv"),
    read.csv("../metrics/uci_test_descriptivestats.csv"),
    read.csv("../metrics/um_test_descriptivestats.csv"),
    read.csv("../metrics/asu_validation_descriptivestats.csv"),
    read.csv("../metrics/asuo_validation_descriptivestats.csv"),
    read.csv("../metrics/uci_validation_descriptivestats.csv"),
    read.csv("../metrics/um_validation_descriptivestats.csv")
) %>% 
    mutate(
        type = rep(c("train", "test", "validation"), each=40),
        institution = rep(rep(c("asu", "asuo", "uci", "um"), each=10), 3)
    )

# Table with descriptives of key vars
a %>% 
    filter(metric %in% c("mean","std")) %>%
    group_by(institution, type, metric) %>%
    select(
        starts_with("urm_status_"),
        starts_with("ethnicity_"),
        starts_with("sex_"),
        "age", "units","units_failed","units_transferred","gpa_avg","retention"
    ) %>%
    gather(var, val, -(institution:metric)) %>%
    mutate(val = format(round(val, 3), nsmall=3)) %>%
    group_by(institution, type, var) %>%
    summarise(
        val = paste0(val[metric=="mean"], " (", val[metric=="std"], ")")
    )

# Todo: export into latex