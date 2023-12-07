## Plots of model output for paper
## RFK Jan 2, 2022

# install.packages("tidyverse")
# install.packages("scales")
library(tidyverse)
library(scales)

# If needed, change to the directory containing the results files.
METRICS_DIR = "../metrics/05.2022"
source("utils.R")

######################################################
#### Results Overall
######################################################

# Figure 1 (without ensemble)
d %>%
    filter(overall & model_type == "LR") %>%
    ggplot(aes(src_institution, auc, fill=(src_institution==target_institution))) + 
    geom_bar(stat="identity") + 
    geom_errorbar(aes(ymin = auc - seauc, ymax = auc + seauc), width=0.2) +
    labs(x="Source Institution", fill="Local Model", y="Test AUC") +
    theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=1) +
    scale_y_continuous(limits = c(0,.9), breaks = seq(0,1,.2), expand=c(0,0)) +
    scale_fill_manual(values = c("gray60", "gray15")) +
    theme(
        legend.position = "top", 
        strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "white")
    )
ggsave("overall_LR_auc.pdf", width=7, height=3.5)

# Figure 1 (with ensemble)
fig_1a = d %>%
    filter(overall & model_type %in% c("LR", "ENm", "ENs")) %>%
    mutate(
        model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
        src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
        src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked"))
    ) %>%
    ggplot(aes(src, auc, fill=model)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin = auc - seauc, ymax = auc + seauc), width=0.2) +
    geom_text(aes(label=format(round(auc,3), nsmall = 3), y = auc + seauc), vjust=-1.25, size=3) +
    labs(x="Source Institution", fill="", y="Test AUC") +
    theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2), expand=c(0,0)) +
    scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
    theme(
        legend.position = "top", 
        strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "white")
    )
fig_1a
ggsave("overall_LR_auc_ens.pdf", width=7, height=7)

model_means = d %>%
  filter(overall & model_type %in% c("LR", "ENm", "ENs")) %>%
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", 
                   ifelse(model_type=="LR", "Direct Transfer",
                          ifelse(model_type=="ENm", "Voting", "Stacked"))),
    # src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    # src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked"))
  ) %>%
  mutate(
    model=fct_relevel(model, "Stacked", after = Inf),
    model=fct_relevel(model, "Local Model"),
  ) %>%
  group_by(model) %>%
  summarise(mean_auc = mean(auc))


d %>%
  filter(overall & model_type %in% c("LR", "ENm", "ENs")) %>%
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", 
                   ifelse(model_type=="LR", "Direct Transfer",
                          ifelse(model_type=="ENm", "Voting", "Stacked"))),
  ) %>%
  mutate(
    model=fct_relevel(model, "Stacked", after = Inf),
    model=fct_relevel(model, "Local Model"),
  ) %>%
  
  ggplot(aes(x=model, y=auc)) + 
  geom_bar(data=model_means, aes(x=model, y=mean_auc), stat="identity", width=0.8, fill="dodgerblue") +
  geom_jitter(width=0.1, size=2.5, alpha=0.9) +
  labs(x="Transfer Type", fill="", y="Test AUC") +
  theme_bw() +
  scale_x_discrete(labels=c("Local Model",
                            "Direct Transfer:\nTrained at\nOther Institution",
                            "Voting Transfer:\nWeighted Voting via\nDirect Transfer (Zero-Shot,\nWithout Local Model)",
                            "Stacked Transfer:\nEnsemble of\nAll Institutions")) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2), expand=c(0,0)) +
  scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white"),
    axis.text.y = element_text(size=7)
  ) +
  coord_flip()
ggsave("fig0.pdf", width=6, height=2.5)

######################################################
#### Results Model Complexity (lambda)
######################################################

# Overall AUC vs. lambda
lambda_overall = d %>%
    filter(overall & model_type == "LRlambda") %>%
    mutate(
      pen = factor(ifelse(lambda==0, "None", log10(lambda)), levels=c("None", (-4:4))),
      is_local = ifelse(src_institution==target_institution, "Local Model", "Direct Transfer"),
        )
lambda_overall %>%
    ggplot(aes(pen, auc, group=src_institution, color = is_local)) + 
    geom_line() + 
    geom_point() +
    geom_errorbar(aes(ymin = auc - seauc, ymax = auc + seauc), width=0.2) +
    facet_grid(paste("Source Institution:", src_institution) ~ paste("Target Institution:", target_institution)) +
    labs(x=TeX("$L_2$ Penalty: $log_{10}(\\lambda)$"), color="Source Model", y="Test AUC") +
    scale_color_manual(values = c("gray75", "gray45")) +
    scale_y_continuous(limits = c(0.3, 0.9), breaks=seq(0.5,0.9,0.1), expand=c(0,0)) +
    theme_bw() + 
    theme(
        legend.position = "top", 
        strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "white")
    )
ggsave("overall_LRlambda_auc.pdf", width=7, height=6)

d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & model_type == "LRlambda") %>%
  mutate(pen = factor(ifelse(lambda==0, "None", log10(lambda)), levels=c("None", (-4:4)))) %>%
  ggplot(aes(pen, auc, color=src_institution, group=target_institution)) + 
  geom_line() + 
  geom_point() +
  # geom_errorbar(aes(ymin = auc - seauc, ymax = auc + seauc), width=0.2) +
  facet_grid(paste("Source Institution:", src_institution) ~ subgroup) +
  labs(x=TeX("$L_2$ Penalty: $log_{10}(\\lambda)$"), color="Source Model", y="Test AUC") +
  scale_color_manual(values = c("#8C1D40", "red", "#FFCB05", "#0064A4")) +
  # scale_y_continuous(limits = c(0.5, 0.9), breaks=seq(0.5,0.9,0.1), expand=c(0,0)) +
  theme_bw() + 
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )
ggsave("subgroup_LRlambda_auc.pdf", width=7, height=6)

fig_4b = d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & model_type == "LRlambda") %>%
  mutate(
    pen = factor(ifelse(lambda==0, "None", log10(lambda)), levels=c("None", (-4:4))),
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
    is_local = ifelse(src_institution==target_institution, "Local Model", "Direct Transfer"),
    ) %>%
  ggplot(aes(pen, auc, shape=grp, group=pen, color=is_local)) + 
  geom_boxplot(color="grey", width=0.25, coef=NULL, size=0.2, show.legend = FALSE) +
  geom_point(stat="identity", size=rel(2), alpha=0.9) +
  facet_grid(paste("Source:", src_institution) ~ paste("Target Institution:", target_institution)) +
  labs(x=TeX("$L_2$ Penalty: $log_{10}(\\lambda)$"), color="Source Model", y="Test AUC", shape="Student Subgroup") +
  scale_color_manual(values = c("gray45", "gray10")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  guides(color="none") +
  theme_bw() + 
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )
fig_4b
ggsave("subgroup_institution_LRlambda_auc.pdf", width=7, height=6)


# Auc gap vs. lambda with "star" markers at best value of lambda by test AUC
best_lambda_per_group = lambda_overall %>% 
  group_by(src_institution, target_institution) %>% 
  mutate(max_auc = max(auc)) %>% 
  ungroup() %>%
  filter(auc==max_auc) %>%
  select(c(src_institution, target_institution, pen, auc))
best_lambda_per_group$best_lambda = TRUE

auc_summary = d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & model_type == "LRlambda") %>%
  mutate(
    pen = factor(ifelse(lambda==0, "None", log10(lambda)), levels=c("None", (-4:4))),
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
    is_local = ifelse(src_institution==target_institution, "Local Model", "Direct Transfer"),
    ) %>%
  group_by(src_institution, target_institution, pen) %>%
  summarise(
    auc_sd = sd(auc, na.rm=T),
    auc_range = diff(range(auc, na.rm=T)),
    max_range = max(diff(range(auc, na.rm=T))),
  )
auc_summary %>% dplyr::left_join(
  best_lambda_per_group, by=c("src_institution", "target_institution", "pen")) %>%
  ggplot(aes(pen, auc_range, group=pen, fill=target_institution)) + 
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label=if_else(best_lambda, "*", ""), vjust=-0.1, size=4)) +
  facet_grid(paste("Source Institution:", src_institution) ~ paste("Target Institution:", target_institution)) +
  ylim(0,0.75)+
  labs(x=TeX("$L_2$ Penalty: $log_{10}(\\lambda)$"), color="Source Model", y="AUC Gap") +
  scale_fill_manual(values = c("#8C1D40", "red", "#FFCB05", "#0064A4")) +
  theme_bw() + 
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )
ggsave("institution_LRlambda_auc_gap.pdf", width=7, height=6)

# Co-plot overall AUC and AUC gap

fig_4a = auc_summary %>% dplyr::left_join(
  best_lambda_per_group, by=c("src_institution", "target_institution", "pen")) %>%
  dplyr::left_join(dplyr::select(lambda_overall, c("src_institution", "target_institution", "pen", "auc")), 
                                 by=c("src_institution", "target_institution", "pen"),
                   suffix=c("", "_overall")) %>%
  mutate(
    is_local = ifelse(src_institution==target_institution, "Local Model", "Direct Transfer"),
  ) %>%
  ggplot(aes(pen, auc_range, group=pen, fill=is_local)) + 
  geom_bar(stat="identity", width=0.8, alpha=0.9) +
  # geom_text(aes(label=if_else(best_lambda, "*", ""), vjust=-0.1, size=4)) +
  ###
  geom_line(aes(x=pen, y=auc_overall, group=src_institution, color = is_local)) + 
  geom_point(aes(x=pen, y=auc_overall, group=src_institution, color = is_local, shape=is.na(best_lambda))) +
  # geom_errorbar(data = lambda_overall, aes(ymin = auc - seauc, ymax = auc + seauc), width=0.2) +
  ###
  facet_grid(paste("Source:", src_institution) ~ paste("Target Institution:", target_institution)) +
  ylim(0,1)+
  labs(x=TeX("$L_2$ Penalty: $log_{10}(\\lambda)$"), color="Test AUC", y="AUC Gap / AUC", fill=NULL) +
  scale_fill_manual(name=NULL, values = c("gray45", "gray10")) +
  scale_color_manual(name=NULL, values = c("gray45", "gray10")) +
  scale_shape_manual(values=c(8, 20)) +
  theme_bw() + 
  guides(shape="none", color="none", fill = guide_legend(override.aes = list(shape = NA))) +
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )

fig_4a
######################################################
#### Results Fairness
######################################################

### Figure XX Fairness SD
d %>%
    filter(!overall & model_type %in% c("LR", "ENm", "ENs")) %>%
    mutate(
        model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
        src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
        src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked"))
    ) %>%
    group_by(src_institution, target_institution, model, src) %>%
    summarise(
        auc_sd = sd(auc, na.rm=T),
        auc_range = diff(range(auc, na.rm=T))
    ) %>%
    ggplot(aes(src, auc_sd, fill=model)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=format(round(auc_sd,3), nsmall = 3)), vjust=-0.25, size=3) +
    labs(x="Source Institution", fill="", y="Std. Dev. of Subgroup AUCs") +
    theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
    scale_y_continuous(limits = c(0,.2), breaks = seq(0,.2,.05), expand=c(0,0)) +
    scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
    theme(
        legend.position = "top", 
        strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "white")
    )
ggsave("subgroups_LR_auc_sd.pdf", width=7, height=6)

### Figure XX Fairness Range
d %>%
    filter(!overall & model_type %in% c("LR", "ENm", "ENs")) %>%
    mutate(
        model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
        src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
        src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked"))
    ) %>%
    group_by(src_institution, target_institution, model, src) %>%
    summarise(
        auc_sd = sd(auc, na.rm=T),
        auc_range = diff(range(auc, na.rm=T))
    ) %>%
    ggplot(aes(src, auc_range, fill=model)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=format(round(auc_range,3), nsmall = 3)), vjust=-0.25, size=3) +
    labs(x="Source Institution", fill="", y="AUC Gap") +
    theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
    scale_y_continuous(limits = c(0,.6), breaks = seq(0,.6,.1), expand=c(0,0)) +
    scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
    theme(
        legend.position = "top", 
        strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "white")
    )
ggsave("subgroups_LR_auc_range.pdf", width=7, height=6)

d %>%
  filter(!overall & model_type %in% c("LR", "ENm", "ENs") & !is.na(auc)) %>%
  select(subgroup) %>%
  unique()

######################################################
#### Results Fairness Group-specific
######################################################

# AUC gap, non-intersectional
fig_2a_nonint_version = d %>%
  filter(!overall & model_type %in% c("LR", "ENm", "ENs") & subgroup %in% MARGINAL_GROUPS) %>%
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
  ) %>%
  group_by(src_institution, target_institution, model, src) %>%
  summarise(
    auc_sd = sd(auc, na.rm=T),
    auc_range = diff(range(auc, na.rm=T))
  ) %>%
  ggplot(aes(src, auc_range, fill=model)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=format(round(auc_range,3), nsmall = 3)), vjust=-0.25, size=3) +
  labs(x="Source Institution", fill="", y="AUC Gap") +
  theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
  scale_y_continuous(limits = c(0,.6), breaks = seq(0,.6,.1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )
fig_2a_nonint_version
ggsave("sexURM_LR_auc_ens_range.pdf", width=7, height=6)

### Boxplots for all subgroups (non-intersectional)

fig_2b_nonint_version = d %>%
  filter(subgroup %in% MARGINAL_GROUPS & model_type %in% c("LR", "ENm", "ENs")) %>%
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
    grp = factor(subgroup, labels=c("Female", "Male", "Non-URM", "URM")),
  ) %>%
  ggplot(aes(x=src, y=auc, color=model, shape=grp, group=src)) +
  geom_boxplot(coef=NULL, size=0.5, width=0, position=position_nudge(x=-0.15), show.legend=FALSE) +
  geom_point(stat="identity", size=rel(2), alpha=0.75) +
  labs(x="Source Institution", color="Model Type:", shape="Student Subgroup:", y="Test AUC") +
  theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
  ylim(0.35, 1) +
  scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
  scale_color_manual(values = c("gray45", "gray75", "gray10")) +
  scale_shape_manual(values = c(22, 23, 24, 25)) +
  theme(
    legend.position = "top", 
    legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )
fig_2b_nonint_version
ggsave("subgroupLR_auc_ens.pdf", width=7, height=6)

# Intersectional version


### Fairness range (intersectional)
fig_2a = d %>%
  filter(!overall & model_type %in% c("LR", "ENm", "ENs") & subgroup %in% INTERSECTIONAL_GROUPS) %>%
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
    sex = factor(subgroup, labels=c("Female", "Female", "Male", "Male")),
    urm = factor(subgroup, labels=c("Non-URM", "URM", "Non-URM", "URM"))
  ) %>%
  group_by(src_institution, target_institution, model, src) %>%
  summarise(
    auc_sd = sd(auc, na.rm=T),
    auc_range = diff(range(auc, na.rm=T))
  ) %>%
  ggplot(aes(src, auc_range, fill=model)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=format(round(auc_range,3), nsmall = 3)), vjust=-0.25, size=3) +
  labs(x="Source Institution", fill="", y="AUC Gap") +
  theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
  scale_y_continuous(limits = c(0,.6), breaks = seq(0,.6,.1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )

fig_2a

ggsave("sexURM_LR_auc_ens_range_intersectional.pdf", width=7, height=6)

### Boxplots for subgroups (intersectional)

fig_2b =  d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & model_type %in% c("LR", "ENm", "ENs")) %>%
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
    sex = factor(subgroup, labels=c("Female", "Female", "Male", "Male")),
    urm = factor(subgroup, labels=c("Non-URM", "URM", "Non-URM", "URM"))
  ) %>%
    ggplot(aes(x=src, y=auc, color=model, shape=grp, group=src)) +
    geom_boxplot(coef=NULL, size=0.5, width=0, position=position_nudge(x=-0.15), show.legend=FALSE) +
    geom_point(stat="identity", size=rel(2), alpha=0.75) +
    labs(x="Source Institution", color="Model Type:", shape="Student Subgroup:", y="Test AUC") +
    ylim(0.35, 1) +
    theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
    # scale_y_continuous(limits = c(.55,1), breaks = seq(0,1,.1), expand=c(0,0)) +
    scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
    scale_color_manual(values = c("gray45", "gray75", "gray10")) +
    scale_shape_manual(values = c(0, 15, 2, 17)) +
    theme(
      legend.position = "top", 
      legend.box = "vertical",
      legend.margin = margin(),
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    )

fig_2b

ggsave("sexURM_LR_auc_ens_boxplot_intersectional.pdf", width=7, height=6)


### Subgroup-specific AUC (intersectional)

d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & model_type %in% c("LR", "ENm", "ENs")) %>%
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
    grp = factor(subgroup, labels=c("Female", "Male", "Non-URM", "URM")),
  ) %>%
  ggplot(aes(x=src, y=auc, color=model, shape=grp)) +
  geom_point(stat="identity", position = position_dodge(width=0.5)) +
  geom_errorbar(aes(x = src,
                    ymin=ifelse(auc - seauc < 0, 0, auc - seauc),
                    ymax=ifelse(auc + seauc > 1.0, 1.0, auc + seauc),
                    width=0.2),
                position=position_dodge((width=0.5))) +
  labs(x="Source Institution", color="Model Type:", shape="Student Subgroup:", y="Test AUC") +
  theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
  # scale_y_continuous(limits = c(.55,1), breaks = seq(0,1,.1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
  scale_color_manual(values = c("gray75", "gray45", "gray10")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white")
  )
ggsave("sexURM_LR_auc_ens_intersectional.pdf", width=7, height=6)


## Alternate version with group on x-axis
fig_3a = d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & model_type %in% c("LR", "ENm", "ENs")) %>%
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "Direct Transfer", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
  ) %>%
  ggplot(aes(x=grp, y=auc, color=model, shape=grp, group=src)) +
  geom_point(stat="identity", position = position_dodge(width=0.5)) +
  geom_errorbar(aes(x = grp,
                    ymin=ifelse(auc - seauc < 0, 0, auc - seauc),
                    ymax=ifelse(auc + seauc > 1.0, 1.0, auc + seauc),
                    width=0.2),
                position=position_dodge((width=0.5))) +
  labs(x="Intersectional Subgroup", color=NULL, shape="Student Subgroup:", y="Test AUC") +
  theme_bw() + facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
  # scale_y_continuous(limits = c(.55,1), breaks = seq(0,1,.1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
  scale_color_manual(values = c("gray45", "gray75", "gray10")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size=6),
    strip.background = element_rect(fill = "white")
  )
fig_3a


#################################################
## Delta-AUC (transfer gap)
#################################################


MODELS = c("LR", "ENs", "ENm")

# Results of each institutions' local L2LR model, on itself, by subgroup
same_institution_results = d %>% 
  filter(subgroup == "full_test", model_type=="LR", src_institution == target_institution) %>%
  select(c("auc", "src_institution", "seauc", "target_institution", "model_type"))

# Results of all transfer models (of all types), excluding the local/"same-institution" results above.
cross_institution_results = d %>% 
  filter(subgroup == "full_test", model_type %in% MODELS, src_institution != target_institution) %>%
  select(c("auc", "src_institution", "target_institution", "seauc", "model_type"))

delta_aucs = merge(cross_institution_results, same_institution_results,
                   by=c("target_institution"),
                   suffixes = c("_transfer", "_same")) %>%
  dplyr::rename(src_institution = src_institution_transfer, model_type=model_type_transfer) %>% 
  mutate("delta_auc" = auc_transfer - auc_same) %>%
  mutate("se_delta_auc" = sqrt(seauc_transfer^2 + seauc_same^2)) %>%
  select(c("src_institution", "target_institution", "delta_auc", "se_delta_auc", "model_type"))

fig_1b = delta_aucs %>% 
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "External Model", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
  ) %>%
  ggplot(aes(x = src, y=delta_auc, color=model)) +
  geom_point(position=position_dodge((width=0.5)), size=rel(2)) +
  geom_errorbar(aes(x = src, 
                    ymin=delta_auc - se_delta_auc,
                    ymax=delta_auc + se_delta_auc,
                    width=0.2),
                position=position_dodge((width=0.5))) + 
  geom_hline(yintercept=0, col="dodgerblue", alpha=0.4) + 
  facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
  scale_x_discrete(drop=FALSE) +
  labs(x="Source Institution", color="", shape="", y=TeX("$\\Delta$AUC")) +
  theme_bw() +
  scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
  scale_color_manual(values = c("gray75", "gray45", "gray10")) +
  theme(
    legend.position = "top", 
    # legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )

fig_1b

ggsave("delta_auc.pdf", width=7, height=6)


# Two-sided z-test of difference in AUC for all institutions (we report these for Target A, Source B/D)
delta_aucs %>% 
  # filter(model_type=="LR") %>% 
  mutate(nsd = delta_auc/se_delta_auc) %>% 
  mutate(pval = 2 * pnorm(-abs(nsd)))


# Compute average delta auc
delta_aucs %>% filter(src_institution %in% c("A","B","C")) %>% pull(delta_auc) %>% mean()




## Compare transfer gap by institution + subgroup (non-intersectional)

# Results of each institutions' local L2LR model, on itself, by subgroup
same_institution_results_marginal = d %>% 
  filter(
    subgroup %in% MARGINAL_GROUPS,
    model_type=="LR", 
    src_institution == target_institution) %>%
  select(c("auc", "src_institution", "subgroup", "seauc", "target_institution", "model_type"))

# Results of all transfer models (of all types), excluding the local/"same-institution" results above.
cross_institution_results_marginal = d %>% 
  filter(
    subgroup %in% MARGINAL_GROUPS, 
    model_type %in% MODELS, 
    src_institution != target_institution) %>%
  select(c("auc", "src_institution", "target_institution", "subgroup", "seauc", "model_type"))

delta_aucs_marginal = merge(cross_institution_results_marginal, same_institution_results_marginal,
                            by=c("target_institution", 
                                 "subgroup" 
                                 # "model_type"
                            ),
                            suffixes = c("_transfer", "_same")) %>%
  dplyr::rename(src_institution = src_institution_transfer, model_type=model_type_transfer) %>% 
  mutate("delta_auc" = auc_transfer - auc_same) %>%
  mutate("se_delta_auc" = sqrt(seauc_transfer^2 + seauc_same^2)) %>%
  select(c("src_institution", "target_institution", "subgroup", "delta_auc", "se_delta_auc", "model_type")) %>%
  mutate(subgroup=make.names(subgroup)) 

delta_aucs_marginal %>% 
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "External Model", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
    # sex = factor(subgroup, labels=c("Female", "Female", "Male", "Male")),
    # urm = factor(subgroup, labels=c("Non-URM", "URM", "Non-URM", "URM"))
  ) %>%
  ggplot(aes(x = src, color = model, shape = subgroup, y=delta_auc)) +
  geom_point(position=position_dodge((width=0.5))) +
  geom_errorbar(aes(x = src, 
                    ymin=delta_auc - se_delta_auc,
                    ymax=delta_auc + se_delta_auc,
                    width=0.2),
                position=position_dodge((width=0.5))) + 
  geom_hline(yintercept=0, col="dodgerblue", alpha=0.4) + 
  facet_wrap(~paste("Target Institution:", target_institution), nrow=2) +
  scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
  scale_color_manual(values = c("gray75", "gray45", "gray10")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  labs(x="Source Institution", color="", shape="", y="∆AUC") +
  theme_bw() +
  theme(
    legend.position = "top", 
    # legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )
ggsave("sexURM_LR_delta_auc_marginal.pdf", width=7, height=6)



## Compare transfer gap by institution + subgroup (intersectional)

# Results of each institutions' local L2LR model, on itself, by subgroup
same_institution_results_intersectional = d %>% 
  filter(
    subgroup %in% INTERSECTIONAL_GROUPS,
    model_type=="LR", 
    src_institution == target_institution) %>%
  select(c("auc", "src_institution", "subgroup", "seauc", "target_institution", "model_type"))

# Results of all transfer models (of all types), excluding the local/"same-institution" results above.
cross_institution_results_intersectional = d %>% 
  filter(
    subgroup %in% INTERSECTIONAL_GROUPS, 
    model_type %in% MODELS, 
    src_institution != target_institution) %>%
  select(c("auc", "src_institution", "target_institution", "subgroup", "seauc", "model_type"))

delta_aucs_intersectional = merge(cross_institution_results_intersectional, same_institution_results_intersectional,
                                  by=c("target_institution", 
                                       "subgroup" 
                                       # "model_type"
                                  ),
                                  suffixes = c("_transfer", "_same")) %>%
  dplyr::rename(src_institution = src_institution_transfer, model_type=model_type_transfer) %>% 
  mutate("delta_auc" = auc_transfer - auc_same) %>%
  mutate("se_delta_auc" = sqrt(seauc_transfer^2 + seauc_same^2)) %>%
  select(c("src_institution", "target_institution", "subgroup", "delta_auc", "se_delta_auc", "model_type")) %>%
  mutate(subgroup=make.names(subgroup)) 

yrange = 0.5  # lower/upper bound of delta auc in plot below.

fig_3b = delta_aucs_intersectional %>% 
  mutate(
    model = ifelse(src_institution==target_institution, "Local Model", ifelse(model_type=="LR", "External Model", "Ensemble Model")),
    src = ifelse(model_type=="LR", src_institution, ifelse(model_type=="ENm", "Voting", "Stacked")),
    src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
  ) %>%
  ggplot(aes(x=grp, y=delta_auc, color=model, shape=grp, group=src)) +
  geom_point(position=position_dodge((width=0.5))) +
  geom_errorbar(aes(x = grp, 
                    ymin=ifelse(delta_auc - se_delta_auc < -yrange, -yrange, delta_auc - se_delta_auc),
                    ymax=ifelse(delta_auc + se_delta_auc > yrange, yrange, delta_auc + se_delta_auc),
                    width=0.2),
                position=position_dodge((width=0.5))) + 
  geom_hline(yintercept=0, col="dodgerblue", alpha=0.4) + 
  facet_wrap(~paste("Target Institution:", target_institution), nrow=2) +
  scale_y_continuous(limits = c(-yrange, yrange), breaks = seq(-yrange, yrange, .1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
  scale_color_manual(values = c("gray75", "gray45", "gray10")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  labs(x="Intersectional Subgroup", color="", shape="", y="∆AUC") +
  guides(shape = "none") + # suppress shape guide to avoid duplication with 3a legend
  theme_bw() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(size=6),
    legend.margin = margin(),
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )
fig_3b
ggsave("sexURM_LR_delta_auc_intersectional.pdf", width=7, height=6)



######################################################
#### Final formatted figures (i.e. multiple-panel plots)
######################################################

fig_1a + plot_spacer() + fig_1b + guides(color="none") + 
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
              ) &
  theme(legend.position='top')
ggsave("fig1.pdf", width=14, height=6, device=cairo_pdf)


fig_2a + plot_spacer() + fig_2b + guides(color="none") + 
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
  ) &
  theme(legend.position='top')
ggsave("fig2.pdf", width=14, height=6, device=cairo_pdf)

fig_2a_nonint_version + plot_spacer() + fig_2b_nonint_version + guides(color="none") + 
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
  ) &
  theme(legend.position='top')
ggsave("fig2_nonintersectional.pdf", width=14, height=6, device=cairo_pdf)

fig_3a + plot_spacer() + fig_3b + guides(color="none") + 
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
  ) &
  theme(legend.position='top')
ggsave("fig3.pdf", width=14, height=6, device=cairo_pdf)



fig_4a + plot_spacer() + fig_4b + 
  # guides(color="none") + 
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
  ) &
  theme(legend.position='top')
ggsave("fig4.pdf", width=14, height=6, device=cairo_pdf)

######################################################
#### Older plots comparing algos etc.
######################################################
d %>%
    filter(
        subgroup %in% c("full_test"),
        model_type == "LR"
    ) %>%
    ggplot(aes(src_institution, auc, fill=target_institution)) + 
    geom_bar(stat="identity", position = position_dodge(width=.9)) + 
    labs(x="Source Institution", fill="Target Institution", y="Test AUC") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(0,.85), expand=c(0,0)) +
    scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
ggsave("auc_full.pdf", width=4.5, height=4)

d %>%
    filter(
        subgroup %in% c("full_test"),
        model_type == "LR"
    ) %>%
    ggplot(aes(src_institution, f1, fill=target_institution)) + 
    geom_bar(stat="identity", position = position_dodge(width=.9)) + 
    labs(x="Source Institution", fill="Target Institution", y="Test F1") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(0,1), expand=c(0,0)) +
    scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
ggsave("f1_full.pdf", width=4.5, height=4)

### Compare Algorithms

d %>%
    filter(
        subgroup %in% c("full_test")
    ) %>%
    ggplot(aes(src_institution, f1, fill=target_institution)) + 
    geom_bar(stat="identity", position = position_dodge(width=.9)) + 
    facet_wrap(~model_type_lng) +
    labs(x="Source Institution", fill="Target Institution", y="Test F1") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(0,1), expand=c(0,0)) +
    scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
ggsave("f1_full_allmodels.pdf", width=9, height=4)

d %>%
    filter(
        subgroup %in% c("full_test")
    ) %>%
    ggplot(aes(src_institution, auc, fill=target_institution)) + 
    geom_bar(stat="identity", position = position_dodge(width=.9)) + 
    facet_wrap(~model_type_lng) +
    labs(x="Source Institution", fill="Target Institution", y="Test AUC") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(0,.85), expand=c(0,0)) +
    scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
ggsave("auc_full_allmodels.pdf", width=9, height=4)

d %>%
    filter(
        subgroup %in% c("full_test")
    ) %>%
    ggplot(aes(src_institution, recall, fill=target_institution)) + 
    geom_bar(stat="identity", position = position_dodge(width=.9)) + 
    facet_wrap(~model_type_lng) +
    labs(x="Source Institution", fill="Target Institution", y="Test Recall") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(0,1), expand=c(0,0)) +
    scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
ggsave("recall_full_allmodels.pdf", width=9, height=4)

d %>%
    filter(
        subgroup %in% c("full_test")
    ) %>%
    ggplot(aes(src_institution, precision, fill=target_institution)) + 
    geom_bar(stat="identity", position = position_dodge(width=.9)) + 
    facet_wrap(~model_type_lng) +
    labs(x="Source Institution", fill="Target Institution", y="Test Recall") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(0,1), expand=c(0,0)) +
    scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
ggsave("precision_full_allmodels.pdf", width=9, height=4)



d %>%
    filter(
        subgroup != "full_test"
    ) %>%
    ggplot(aes(src_institution, auc, fill=target_institution)) +
        geom_boxplot() + facet_wrap(~model_type_lng) +
        labs(x="Source Institution", fill="Target Institution", y="Subgroup Test AUC") +
        theme_bw() + 
        theme(legend.position = "top") + scale_y_continuous(limits = c(.3,1), expand=c(0,0)) +
        scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
ggsave("auc_subgroups_allmodels.pdf", width=9, height=4)

d %>%
    filter(
        subgroup != "full_test"
    ) %>%
    group_by(src_institution, target_institution, model_type_lng) %>%
    summarise(
        auc_sd = sd(auc, na.rm=T),
        f1_sd = sd(f1, na.rm=T),
    ) %>%
    ggplot(aes(src_institution, auc_sd, fill=target_institution)) +
    geom_bar(stat="identity", position = position_dodge(width=.9)) +
    facet_wrap(~model_type_lng) +
    labs(x="Source Institution", fill="Target Institution", y="SD of Subgroup Test AUCs") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(0,.25), expand=c(0,0)) +
    scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
ggsave("aucSD_subgroups_allmodels.pdf", width=9, height=4)


d %>%
    filter(
        subgroup != "full_test"
    ) %>%
    group_by(src_institution, target_institution, model_type_lng) %>%
    summarise(
        auc_sd = sd(auc, na.rm=T),
        auc_mean = mean(auc, na.rm=T),
        f1_sd = sd(f1, na.rm=T),
        f1_mean = mean(f1, na.rm=T),
    ) %>%
    ggplot(aes(auc_mean, auc_sd, color=target_institution)) +
    geom_point(stat="identity") +
    facet_grid(src_institution~model_type_lng) +
    labs(x="AUC Mean", color="Target Institution", y="AUC SD") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(0,.25), expand=c(0,0)) +
    scale_color_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))


d %>%
    filter(
        subgroup != "full_test",
        model_type == "RF"
    ) %>%
    ggplot(aes(src_institution, auc, fill=target_institution)) +
    geom_boxplot() +
    labs(x="Source Institution", fill="Target Institution", y="Subgroup Test AUC") +
    theme_bw() + 
    theme(legend.position = "top") + scale_y_continuous(limits = c(.3,1), expand=c(0,0)) +
    scale_fill_manual(values = c("#8C1D40", "#8a495d", "#FFCB05", "#0064A4"))
