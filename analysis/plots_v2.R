## Script to generate plots of experimental results.

library(tidyverse)
## Ensure that working directory is set to the source file location
## or use an absolute path for METRICS_DIR here.
METRICS_DIR = "../metrics/09.2022"

source("utils.R")

CI_WIDTH = 1.96  # Number of standard errors to use for all confidence intervals.

## Preprocessing specific to new data schema
recode_factors <- function(df){
  df$ensemble_type = fct_recode(df$ensemble_type,
                                "Majority Voting" = "majority_voting_ensemble",
                                "Stacked" = "stacked_ensemble")
  df$model_type = fct_recode(df$model_type,
                                  "L2LR" = "l2lr",
                                  "LightGBM" = "lightgbm",
                                  "MLP" = "mlp")
  df$stacked_model_type = fct_recode(df$stacked_model_type,
                                  "L2LR" = "l2lr",
                                  "LightGBM" = "lightgbm",
                                  "MLP" = "mlp")
  ## Categorical variable for local/direct transfer/ensemble models.
  df = df %>% mutate(lde = ifelse(src_institution==target_institution, 
                              "Local Model", 
                              ifelse(is.na(ensemble_type),
                                     "Direct Transfer", "Ensemble Model")),
                     ## Categorical variable for the model type. Note that this will give
                     ## duplicate values for each model (i.e. 'MLP' for both 
                     ## stacked + majority voting models using MLP).
                     ensemble_model_type = ifelse(as.character(stacked_model_type) == "", 
                                           as.character(model_type), 
                                           as.character(stacked_model_type)),
                     ## Unique column per ensemble; i.e. "L2LR (Majority Voting)", "L2LR (Stacked)", etc.
                     ensemble_model_and_transfer_type = paste0(
                       ensemble_model_type, " (", ensemble_type, ")"),
                     ## Categorical variable with ensemble type (A/B/C/D/Majority Voting (MLP) /Stacked (MLP), etc.)
                     src_fine = factor(ifelse(is.na(ensemble_type), 
                                         src_institution, 
                                         ensemble_model_and_transfer_type)),
                     ## Categorical variable with ensemble type (A/B/C/D/Majority Voting/Stacked, etc.)
                     src = factor(ifelse(is.na(ensemble_type), 
                                              src_institution, 
                                              as.character(ensemble_type))),
                     )
  return(df)
}

d = recode_factors(d)

######################################################
#### Results Overall
######################################################

# Figure 1 (without ensemble); previously Figure 1 had only logistic
# regression but here we show all three model types.
d %>%
  filter(overall & is_ensemble == FALSE & is.na(lambda)) %>%
  ggplot(aes(src_institution, auc, fill=(src_institution==target_institution))) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = auc - CI_WIDTH * seauc, ymax = auc + CI_WIDTH * seauc), width=0.2) +
  labs(x="Source Institution / Ensemble Method", fill="Local Model", y="Test AUC") +
  theme_bw() + 
  facet_grid(model_type ~ target_institution) +
  # facet_wrap(~paste("Target Institution:", target_institution), nrow=1) +
  scale_y_continuous(limits = c(0,.9), breaks = seq(0,1,.2), expand=c(0,0)) +
  scale_fill_manual(values = c("gray60", "gray15")) +
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  ) +
  labs(caption = "Transfer performance by model type.")

ggsave("overall_direct_auc.pdf", width=7, height=7)
ggsave("overall_direct_auc.png", width=7, height=7)

# Ensemble models: show voting + all selection strategies
pd = 0.75
d %>%
  filter(overall & is_ensemble & is.na(lambda)) %>%
  mutate(selection_rule = dplyr::recode(
    selection_rule, 
    `best_auc`="Best AUC",
    `best_fairness`="Best Fairness",
    `gbm_only`="Same Family (GBM)",
    `kitchen_sink`="Kitchen Sink",
    `l2lr_only`="Same Faily (L2LR)",
    `mlp_only`="Same Faily (MLP")) %>%
  ggplot(aes(paste(ensemble_type, 
                   replace_na(ensemble_model_type, '')), 
             auc, 
             fill=selection_rule)) + 
  geom_bar(stat="identity", position=position_dodge(pd),
           width=pd) +
  geom_errorbar(aes(ymin = auc - CI_WIDTH * seauc, 
                    ymax = auc + CI_WIDTH * seauc), 
                width=0.2,
                position=position_dodge(pd)) +
  labs(fill="Selection Rule",  y="Test AUC", x = "Ensemble Type and Model") +
  theme_bw() + 
  facet_grid(. ~ paste("Target Institution:", target_institution)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2), expand=c(0,0)) +
  scale_fill_manual(values=c("#7fc97f", "#beaed4",
                             "#fdc086", "#ffff99",
                             "#386cb0", "#f0027f"))+
  theme(
    axis.text.x = element_text(angle=40, hjust=1),
    legend.position = "bottom", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")) +
  labs(caption = "Ensemble performance by ensemble, selection rule, and base model type.")

ggsave("ensemble_all_auc.pdf", width=10, height=5)
ggsave("ensemble_all_auc.png", width=10, height=5)


# Figure 1 (with ensemble, L2LR only)

d %>%
  filter(overall & is.na(lambda) & (model_type=="L2LR" | (ensemble_model_and_transfer_type %in% c("L2LR (Stacked)", "NA (Majority Voting)") & selection_rule=="l2lr_only"))) %>%
  ggplot(aes(src, auc, fill=lde)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = auc - CI_WIDTH * seauc, ymax = auc + CI_WIDTH * seauc), width=0.2) +
  geom_text(aes(label=format(round(auc,3), nsmall = 3), y = auc + CI_WIDTH * seauc), vjust=-1.25, size=3) +
  labs(x="Source Institution / Ensemble Method", 
       fill="", 
       y="Test AUC") +
  theme_bw() + 
  facet_grid(. ~ target_institution) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2), expand=c(0,0)) +
  scale_fill_manual(values = c("gray45", "gray75", "gray20")) +
  theme(
  	axis.text.x = element_text(angle=40, hjust=1),
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  ) +
  labs(caption = "All logistic regression.")

# TODO(jpgard): finish this plot; need to handle majority voting.
d %>%
  filter(overall & is.na(lambda),
         (model_type=="L2LR" | (ensemble_model_and_transfer_type %in% c("L2LR (Stacked)", "NA (Majority Voting)") & selection_rule=="l2lr_only"))
         | (model_type=="MLP" | (ensemble_model_and_transfer_type %in% c("MLP (Stacked)", "NA (Majority Voting)") & selection_rule=="mlp_only"))
         | (model_type=="LightGBM" | (ensemble_model_and_transfer_type %in% c("LightGBM (Stacked)", "NA (Majority Voting)") & selection_rule=="gbm_only"))) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  dplyr::mutate(
    mty=ifelse(is.na(model_type), ensemble_model_type, model_type)
  )
  ggplot(aes(src, auc, fill=lde)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = auc - CI_WIDTH * seauc, ymax = auc + CI_WIDTH * seauc), width=0.2) +
  geom_text(aes(label=format(round(auc,3), nsmall = 3), y = auc + CI_WIDTH * seauc), vjust=-1.25, size=3) +
  labs(x="Source Institution / Ensemble Method", 
       fill="", 
       y="Test AUC") +
  theme_bw() + 
  facet_grid(model_type ~ target_institution) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2), expand=c(0,0)) +
  scale_fill_manual(values = c("gray45", "gray75", "gray20")) +
  theme(
    axis.text.x = element_text(angle=40, hjust=1),
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )


ggsave("overall_LR_auc_ens.pdf", width=10, height=4)
ggsave("overall_LR_auc_ens.png", width=10, height=4)




######################################################
#### Fairness Results
######################################################

# Barplot of AUC gap
d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & is.na(lambda) & split=="test" & selection_rule %in% c(NA, "l2lr_only")) %>%
  mutate(
    model_type = ifelse(is.na(model_type),
                        as.character(stacked_model_type),
                        as.character(model_type))
  ) %>%
  group_by(lde, src, target_institution, model_type) %>%
  summarise(
    # auc_sd = sd(auc, na.rm=T),
    auc_gap = diff(range(auc, na.rm=T)),
    # max_range = max(diff(range(auc, na.rm=T))),
  ) %>% 
  ungroup() %>%
  ggplot(aes(x=src, y=auc_gap, fill=lde)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=format(round(auc_gap, 2), nsmall = 2)), vjust=-0.25, size=3) +
    labs(x="Source Institution / Ensemble Method", fill="", y="AUC Gap", 
         caption="AUC Gap metric by model type and institution.") +
    theme_bw() + 
    theme(
      axis.text.x = element_text(angle=40, hjust=1),
      legend.position = "top", 
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    ) +
    facet_grid(model_type ~ target_institution) +
    scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
	labs(caption = "Best AUC selection rule for ensemble.")

ggsave("aucGap_bestAuc.pdf", width=10, height=10)
ggsave("aucGap_bestAuc.png", width=10, height=10)

# Per-subgroup AUC distributions
d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & is.na(lambda) & split=="test" & selection_rule %in% c(NA, "l2lr_only")) %>%
  mutate(
    model_type = ifelse(is.na(model_type), 
    					as.character(stacked_model_type),
                        as.character(model_type))
  ) %>%
  mutate(
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
    sex = factor(subgroup, labels=c("Female", "Female", "Male", "Male")),
    urm = factor(subgroup, labels=c("Non-URM", "URM", "Non-URM", "URM"))
  ) %>%
  ggplot(aes(x=src, y=auc, color=lde, shape=grp, group=src)) +
  geom_boxplot(coef=NULL, size=0.5, width=0, position=position_nudge(x=-0.15), show.legend=FALSE) +
  geom_point(stat="identity", size=rel(2), alpha=0.75) +
  labs(x="Source Institution / Ensemble Method", color="Model Type:", shape="Student Subgroup:", y="Test AUC") +
  ylim(0.35, 1) +
  theme_bw() + 
  facet_grid(model_type ~ target_institution, scales = "free_x") +
  # scale_y_continuous(limits = c(.55,1), breaks = seq(0,1,.1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray45", "gray75", "gray10")) +
  scale_color_manual(values = c("gray45", "gray75", "gray10")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  theme(
    legend.position = "top", 
    legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle=40, hjust=1)
  ) +
	labs(caption = "Best AUC selection rule for ensemble.")

ggsave("auc_intersect_all_l2lr.pdf", width=10, height=10)
ggsave("auc_intersect_all_l2lr.png", width=10, height=10)

# Plot of intersectional subgroup performance, with AUC Gap indicated

for (mtype in c("LightGBM", "MLP", "L2LR")){
  if (mtype != "LightGBM"){
    srule = tolower(mtype)
  } else {
    srule = "gbm"
  }
  message(mtype)
  
  
  fig_3a = d %>%
    filter(subgroup %in% INTERSECTIONAL_GROUPS & is.na(lambda) & split=="test" & selection_rule %in% c(NA, glue("{srule}_only")) & model_type %in% c(mtype, NA) & ensemble_model_type %in% c(mtype, NA)) %>%
    mutate(
      grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
      sex = factor(subgroup, labels=c("Female", "Female", "Male", "Male")),
      urm = factor(subgroup, labels=c("Non-URM", "URM", "Non-URM", "URM")),
      src = factor(src, labels=c("A","B","C","D","Voting", "Stacked")),
      lde = factor(lde, levels = c("Local Model", "Direct Transfer", "Ensemble Model"))
    ) %>%
    ggplot(aes(x=src, y=auc, color=lde, shape=grp, group=src)) +
    geom_boxplot(coef=NULL, size=0.5, width=0, position=position_nudge(x=-0.15), show.legend=FALSE) +
    geom_point(stat="identity", size=rel(2), alpha=0.75) +
    labs(x="Source Institution / Ensemble Method", 
         color="Model Type:", 
         shape="Student Subgroup:", 
         linetype="THIS",
         y="Test AUC") +
    facet_wrap( ~ paste("Target Institution:", target_institution), scales = "free_x") +
    theme_bw() + 
    # scale_y_continuous(limits = c(.55,1), breaks = seq(0,1,.1), expand=c(0,0)) +
    scale_y_continuous(limits = c(0.4, 1), breaks = seq(0,1,.1), expand=c(0,0)) +
    scale_fill_manual(values = c("gray10", "gray45", "gray75")) +
    scale_color_manual(values = c("gray10", "gray45", "gray75")) +
    scale_shape_manual(values = c(0, 15, 2, 17)) +
    theme(
      legend.position = "top", 
      legend.box = "vertical",
      legend.margin = margin(),
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    )
  
  fig_3a
  
  ggsave(glue("auc_intersect_{mtype}.pdf"), width=7, height=7)
  ggsave(glue("auc_intersect_{mtype}.png"), width=7, height=7)
  
  ## Compute AUC gap and its SE.
  max_auc = d %>%
    filter(subgroup %in% INTERSECTIONAL_GROUPS,
           is.na(lambda),
           split=="test",
           selection_rule %in% c(NA, glue("{srule}_only")),
           model_type %in% c(mtype, NA),
           ensemble_model_type %in% c(mtype, NA)) %>%
    group_by(lde, src, target_institution, model_type) %>%
    top_n(n=1, wt=auc) %>%
    ungroup() %>%
    select(c("subgroup", "auc", "seauc", "src_institution", "target_institution",
             "uid", "selection_rule", "ensemble_type", "stacked_model_type",
             "model_type", "is_ensemble", "lde", "ensemble_model_type",
             "ensemble_model_and_transfer_type", "src", "src_fine")) %>%
    rename(max_auc=auc)
  
  min_auc = d %>%
    filter(subgroup %in% INTERSECTIONAL_GROUPS,
           is.na(lambda),
           split=="test",
           selection_rule %in% c(NA, glue("{srule}_only")),
           model_type %in% c(mtype, NA),
           ensemble_model_type %in% c(mtype, NA)) %>%
    group_by(lde, src, target_institution, model_type) %>%
    top_n(n=-1, wt=auc) %>%
    ungroup() %>%
    select(c("subgroup", "auc", "seauc", "src_institution", "target_institution",
             "uid", "selection_rule", "ensemble_type", "stacked_model_type",
             "model_type", "is_ensemble", "lde", "ensemble_model_type",
             "ensemble_model_and_transfer_type", "src", "src_fine")) %>%
    rename(min_auc=auc)
  
  auc_gap_data = merge(max_auc, min_auc,
                       by=c("lde", "src", "src_institution",
                            "target_institution", "model_type"),
                       suffixes = c("_max_auc", "_min_auc")) %>%
    mutate(auc_gap = max_auc - min_auc,
           se_auc_gap = sqrt(seauc_max_auc**2 + seauc_min_auc**2))
  
  # Plot of AUC gap for L2LR models, by institution.
  y_min = -0.01  # set below zero to see colored line plotted at y = 0.
  y_max = 0.55
  fig_3b = auc_gap_data %>%
    mutate(
      src = factor(src, labels=c("A","B","C","D","Voting", "Stacked")),
      model = ifelse(src_institution==target_institution,
                     "Local Model",
                     ifelse(model_type==mtype,
                            "External Model",
                            "Ensemble Model")),
      lde = factor(lde, levels = c("Local Model", "Direct Transfer", "Ensemble Model")),
      ci_lower = ifelse(auc_gap - CI_WIDTH * se_auc_gap < y_min, y_min, auc_gap - CI_WIDTH * se_auc_gap),
      ci_upper = ifelse(auc_gap + CI_WIDTH * se_auc_gap > y_max, y_max, auc_gap + CI_WIDTH * se_auc_gap),
    ) %>%
    ggplot(aes(x=src, y=auc_gap, color=model)) +
    geom_point() +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width=0.2) +
    geom_text(aes(label=format(round(auc_gap, 2), nsmall = 2)), color="black", 
              vjust=-1.25, size=3) +
    geom_hline(yintercept=0, col="dodgerblue", alpha=0.4) + 
    theme_bw() + 
    labs(x="Source Institution / Ensemble Method", fill="", y="AUC Gap") +
    scale_y_continuous(limits = c(y_min, y_max), breaks = seq(0,1,.1), expand=c(0,0)) +
    theme(
      legend.position = "top", 
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    ) +
    facet_wrap( ~ paste("Target Institution:", target_institution), scales = "free_x") +
    labs(x="Source Institution / Ensemble Method", color="", shape="", y=TeX("$\\Delta$AUC")) +
    # scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
    scale_color_manual(values = c("gray75", "gray45", "gray10"))
  
  fig_3b
  
  ggsave(glue("aucGap_{mtype}.pdf"), width=7, height=7)
  ggsave(glue("aucGap_{mtype}.png"), width=7, height=7)
  
  p = fig_3a + ptheme + theme(legend.text = element_text(size=12), legend.title = element_text(face="bold")) + plot_spacer() + 
    fig_3b + ptheme + theme(legend.text = element_text(size=12), legend.title = element_text(face="bold")) + guides(color="none") + 
    plot_layout(guides = 'collect',  # Shared legend 
                widths = c(10,0.5,10)  # Plots are 10x size of spacer
    ) &
    theme(legend.position='bottom')
  ggsave(glue("fig3_{mtype}.pdf"), plot=p, width=14, height=6, device=cairo_pdf)
}



# # Two-sided z-test of difference in AUC for all institutions (we report these for Target A, Source B/D)
auc_gap_ztest_summary = auc_gap_data %>%
  mutate(nsd = auc_gap/se_auc_gap) %>% 
  mutate(pval = 2 * pnorm(-abs(nsd))) %>%
  select(c("target_institution", "src_institution", "auc_gap", "se_auc_gap", 
           "ensemble_type_min_auc", "pval")) %>%
  arrange(target_institution, ensemble_type_min_auc)

auc_gap_ztest_summary

write.csv(auc_gap_ztest_summary, file="auc_gap_ztest_summary.csv")

## Non-intersectional version of figure 3a

fig_3a_marginal = d %>%
  filter(subgroup %in% MARGINAL_GROUPS & is.na(lambda) & split=="test" & selection_rule %in% c(NA, "l2lr_only") & model_type %in% c("L2LR", NA) & ensemble_model_type %in% c("L2LR", NA)) %>%
  # pull(subgroup) %>% levels()
  mutate(
    grp = factor(subgroup, 
                 labels=c("Female", "Male",
                          "Non-URM", "URM")
                 ),
    src = factor(src, labels=c("A","B","C","D","Voting", "Stacked")),
    lde = factor(lde, levels = c("Local Model", "Direct Transfer", "Ensemble Model"))
  ) %>%
  ggplot(aes(x=src, y=auc, color=lde, shape=grp, group=src)) +
  geom_boxplot(coef=NULL, size=0.5, width=0, position=position_nudge(x=-0.15), show.legend=FALSE) +
  geom_point(stat="identity", size=rel(2), alpha=0.75) +
  labs(x="Source Institution / Ensemble Method", 
       color="Model Type:", 
       shape="Student Subgroup:", 
       linetype="THIS",
       y="Test AUC") +
  facet_wrap( ~ paste("Target Institution:", target_institution), scales = "free_x") +
  theme_bw() + 
  # scale_y_continuous(limits = c(.55,1), breaks = seq(0,1,.1), expand=c(0,0)) +
  scale_y_continuous(limits = c(0.4, 1), breaks = seq(0,1,.1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray10", "gray45", "gray75")) +
  scale_color_manual(values = c("gray10", "gray45", "gray75")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  theme(
    legend.position = "top", 
    legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )

fig_3a_marginal

## Non-intersectional version of figure 3b


## Compute AUC gap and its SE.
max_auc_marginal = d %>%
  filter(subgroup %in% MARGINAL_GROUPS,
         is.na(lambda),
         split=="test",
         selection_rule %in% c(NA, "l2lr_only"),
         model_type %in% c("L2LR", NA),
         ensemble_model_type %in% c("L2LR", NA)) %>%
  group_by(lde, src, target_institution, model_type) %>%
  top_n(n=1, wt=auc) %>%
  ungroup() %>%
  select(c("subgroup", "auc", "seauc", "src_institution", "target_institution",
           "uid", "selection_rule", "ensemble_type", "stacked_model_type",
           "model_type", "is_ensemble", "lde", "ensemble_model_type",
           "ensemble_model_and_transfer_type", "src", "src_fine")) %>%
  rename(max_auc=auc)

min_auc_marginal = d %>%
  filter(subgroup %in% MARGINAL_GROUPS,
         is.na(lambda),
         split=="test",
         selection_rule %in% c(NA, "l2lr_only"),
         model_type %in% c("L2LR", NA),
         ensemble_model_type %in% c("L2LR", NA)) %>%
  group_by(lde, src, target_institution, model_type) %>%
  top_n(n=-1, wt=auc) %>%
  ungroup() %>%
  select(c("subgroup", "auc", "seauc", "src_institution", "target_institution",
           "uid", "selection_rule", "ensemble_type", "stacked_model_type",
           "model_type", "is_ensemble", "lde", "ensemble_model_type",
           "ensemble_model_and_transfer_type", "src", "src_fine")) %>%
  rename(min_auc=auc)

auc_gap_data_marginal = merge(max_auc_marginal, 
                              min_auc_marginal,
                     by=c("lde", "src", "src_institution",
                          "target_institution", "model_type"),
                     suffixes = c("_max_auc", "_min_auc")) %>%
  mutate(auc_gap = max_auc - min_auc,
         se_auc_gap = sqrt(seauc_max_auc**2 + seauc_min_auc**2))


# Plot of AUC gap for L2LR models, by institution.
y_min = -0.01  # set below zero to see colored line plotted at y = 0.
y_max = 0.55
fig_3b_marginal = auc_gap_data_marginal %>%
  mutate(
    src = factor(src, labels=c("A","B","C","D","Voting", "Stacked")),
    model = ifelse(src_institution==target_institution,
                   "Local Model",
                   ifelse(model_type=="L2LR",
                          "External Model",
                          "Ensemble Model")),
    lde = factor(lde, levels = c("Local Model", "Direct Transfer", "Ensemble Model")),
    ci_lower = ifelse(auc_gap - CI_WIDTH * se_auc_gap < y_min, y_min, auc_gap - CI_WIDTH * se_auc_gap),
    ci_upper = ifelse(auc_gap + CI_WIDTH * se_auc_gap > y_max, y_max, auc_gap + CI_WIDTH * se_auc_gap),
  ) %>%
  ggplot(aes(x=src, y=auc_gap, color=model)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width=0.2) +
  geom_text(aes(label=format(round(auc_gap, 2), nsmall = 2)), color="black", 
            vjust=-1.25, size=3) +
  geom_hline(yintercept=0, col="dodgerblue", alpha=0.4) + 
  theme_bw() + 
  labs(x="Source Institution / Ensemble Method", fill="", y="AUC Gap") +
  scale_y_continuous(limits = c(y_min, y_max), breaks = seq(0,1,.1), expand=c(0,0)) +
  theme(
    legend.position = "top", 
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  ) +
  facet_wrap( ~ paste("Target Institution:", target_institution), scales = "free_x") +
  labs(x="Source Institution / Ensemble Method", color="", shape="", y=TeX("$\\Delta$AUC")) +
  # scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
  scale_color_manual(values = c("gray75", "gray45", "gray10"))

fig_3b_marginal



# Ensembles
d %>%
	filter(subgroup %in% INTERSECTIONAL_GROUPS & is_ensemble & is.na(lambda)) %>%
	mutate(etype = paste(ensemble_type, replace_na(ensemble_model_type, ''))) %>%
	group_by(etype, selection_rule, target_institution, model_type) %>%
	summarise(
		# auc_sd = sd(auc, na.rm=T),
		auc_gap = diff(range(auc, na.rm=T)),
		# max_range = max(diff(range(auc, na.rm=T))),
	) %>% 
	ungroup() %>%
	ggplot(aes(etype, auc_gap, fill=selection_rule)) + 
	geom_bar(stat="identity", position=position_dodge(1)) +
	labs(fill="Selection Rule",  y="Test AUC Gap", x = "Ensemble Type and Model") +
	theme_bw() + 
	facet_grid(. ~ target_institution) +
	scale_y_continuous(limits = c(0,.4), breaks = seq(0,1,.1), expand=c(0,0)) +
	theme(
		axis.text.x = element_text(angle=40, hjust=1),
		legend.position = "top", 
		strip.text = element_text(face = "bold"), 
		strip.background = element_rect(fill = "white")) +
	labs(caption = "Ensemble fairness (intersectional groups) by ensemble and base model type.")

ggsave("ensemble_all_aucGAP.pdf", width=10, height=5)
ggsave("ensemble_all_aucGAP.png", width=10, height=5)


######################################################
#### Overview of outcomes
######################################################


##  Fig. 0: barplot of AUC by transfer type.

for (mtype in c("L2LR", "LightGBM", "MLP")){
  if (mtype != "LightGBM"){
    srule = tolower(mtype)
  } else {
    srule = "gbm"
  }
  message(mtype)
  d %>%
    filter(overall, 
           is.na(lambda), 
           (model_type==mtype | 
              (ensemble_model_and_transfer_type %in% c(
                glue("{mtype} (Stacked)"), 
                "NA (Majority Voting)"
              ) & selection_rule==glue("{srule}_only")))) %>%
    mutate(
      src = factor(src, labels=c("A","B","C","D","Voting", "Stacked")),
      ty = factor(paste(lde, ensemble_type)),
      ty = factor(ty, labels = c("DIRECT TRANSFER\nTrained at other institution", 
                                 "VOTING TRANSFER\nZero-shot weighted vote of\ndirect transfer (no local model)",
                                 "STACKED TRANSFER\nEnsemble of all institutions",
                                 "LOCAL MODEL\nLocal Institution data only"))
    ) %>% 
    ggplot(aes(reorder(ty, auc, mean), auc)) + 
    stat_summary(fun = mean, geom="bar", fill="lightblue", width=0.75) + 
    geom_jitter(color="grey20", size=1.5, width = .1) + 
    theme_bw() +
    coord_flip(ylim=c(0.5, 0.8)) +
    theme(axis.text.y = element_text(size=14),
          axis.title = element_text(size=12),
          axis.text.x = element_text(size=12)) + 
    labs(x=NULL, y="Test AUC")
  
  ggsave(glue("overview_testauc_{tolower(mtype)}.pdf"), width=6, height=3.25)
  ggsave(glue("overview_testauc_{tolower(mtype)}.pdf"), width=6, height=3.25)
}


## Fig. 2 (a,b): Delta AUC plots.

# Results of each institutions' local L2LR model, on itself, by subgroup
for (mtype in c("LightGBM", "MLP", "L2LR")){
  if (mtype != "LightGBM"){
    srule = tolower(mtype)
  } else {
    srule = "gbm"
  }
  message(mtype)
  
  # Figure 2a: test AUC by transfer scheme and institution
  fig_2a = d %>%
    filter(overall & is.na(lambda) & (model_type==mtype | (ensemble_model_and_transfer_type %in% c(glue("{mtype} (Stacked)"), "NA (Majority Voting)") & selection_rule==glue("{srule}_only")))) %>%
    mutate(
      src = factor(src, labels=c("A","B","C","D","Voting", "Stacked")),
      lde = factor(lde, levels = c("Local Model", "Direct Transfer", "Ensemble Model"))
    ) %>%
    ggplot(aes(src, auc, fill=lde)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin = auc - CI_WIDTH * seauc, ymax = auc + CI_WIDTH * seauc), width=0.2) +
    geom_text(aes(label=format(round(auc,3), nsmall = 3), y = auc + CI_WIDTH * seauc), vjust=-1.25, size=3.5) +
    labs(x="Source Institution / Ensemble Method", fill="", y="Test AUC ") +
    theme_bw() + 
    facet_wrap(. ~ paste("Target Institution:", target_institution), scales = "free_x", nrow = 2) +
    scale_fill_manual(values = c("gray20", "gray45", "gray75")) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1), expand=c(0,0)) +
    coord_cartesian(ylim=c(.5,.9)) +
    theme(
      # axis.text.x = element_text(angle=40, hjust=1),
      legend.position = "top", 
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    )
  
  fig_2a
  
  ggsave("overall_LR_auc_ens_v2.pdf", width=6.5, height=5.5)
  ggsave("overall_LR_auc_ens_v2.png", width=6.5, height=5.5)
  
  
  same_institution_results = d %>% 
    filter(
      overall, 
      is.na(lambda), 
      src_institution == target_institution,
      model_type==mtype) %>%
    select(c("auc", "src_institution", "seauc", 
             "target_institution", "model_type"))
  
  # Results of all transfer models (of all types), excluding the local/"same-institution" results above.
  cross_institution_results = d %>% 
    filter(
      overall, 
      is.na(lambda), 
      src_institution != target_institution,
      (model_type==mtype | 
         (ensemble_model_and_transfer_type %in% c(glue("{mtype} (Stacked)"), 
                                                  "NA (Majority Voting)")
          & selection_rule==glue("{srule}_only")))) %>%
    select(c("auc", "src_institution", "seauc", 
             "target_institution", "model_type",
             "ensemble_model_and_transfer_type", "ensemble_type"))
  
  
  delta_aucs = merge(cross_institution_results, 
                     same_institution_results,
                     by=c("target_institution"),
                     suffixes = c("_transfer", "_same")) %>%
    dplyr::rename(src_institution = src_institution_transfer, 
                  model_type=model_type_transfer) %>% 
    mutate("delta_auc" = auc_transfer - auc_same) %>%
    mutate("se_delta_auc" = sqrt(seauc_transfer^2 + seauc_same^2)) %>%
    select(c("src_institution", "target_institution", 
             "delta_auc", "se_delta_auc", "model_type",
             "ensemble_model_and_transfer_type", "ensemble_type"))
  
  
  # Two-sided z-test of difference in AUC for all institutions (we report these for Target A, Source B/D)
  delta_auc_ztest_summary = delta_aucs %>% 
    mutate(nsd = delta_auc/se_delta_auc) %>% 
    mutate(pval = 2 * pnorm(-abs(nsd))) %>%
    select(c("target_institution", "src_institution", "delta_auc", "se_delta_auc", 
             "ensemble_type", "pval"))
  delta_auc_ztest_summary 
  write.csv(delta_auc_ztest_summary, file="delta_auc_ztest_summary.csv")
  
  # Compute average (mean) delta auc
  delta_aucs %>% 
    filter(src_institution %in% c("A","B","C")) %>% 
    pull(delta_auc) %>% 
    mean()
  
  # Compute SE of mean delta auc
  tmp = delta_aucs %>% 
    filter(src_institution %in% c("A","B","C")) %>% 
    mutate(var_delta_auc = se_delta_auc**2) %>%
    pull(var_delta_auc)
  
  se_delta_auc_mean = sum(tmp) / (length(tmp)**2)
  se_delta_auc_mean
  
  fig_2b = delta_aucs %>% 
    mutate(
      model = ifelse(src_institution==target_institution,
                     "Local Model",
                     ifelse(model_type==mtype,
                            "External Model",
                            "Ensemble Model")),
      src = ifelse(is.na(ensemble_type), src_institution, as.character(ensemble_type)),
      src = dplyr::recode(src, `Majority Voting`="Voting"),
      src = factor(src),
      src = factor(src, levels = c("A", "B", "C", "D", "Voting", "Stacked")),
    ) %>%
    ggplot(aes(x = src, y=delta_auc, color=model)) +
    geom_point(position=position_dodge((width=0.5)), size=rel(2)) +
    geom_errorbar(aes(x = src, 
                      ymin=delta_auc - CI_WIDTH * se_delta_auc,
                      ymax=delta_auc + CI_WIDTH * se_delta_auc,
                      width=0.2),
                  position=position_dodge((width=0.5))) + 
    geom_text(aes(label=format(round(delta_auc,3), nsmall = 3), y = delta_auc + CI_WIDTH * se_delta_auc), vjust=-1.25, size=3.5) +
    geom_hline(yintercept=0, col="dodgerblue", alpha=0.4) + 
    facet_wrap(~paste("Target Institution:", target_institution), nrow=2, scales = "free_x") +
    scale_x_discrete(drop=FALSE) +
    labs(x="Source Institution / Ensemble Method", color="", shape="", y=TeX("$\\Delta$AUC")) +
    theme_bw() +
    scale_fill_manual(values = c("gray45", "gray75", "gray20")) +
    scale_color_manual(values = c("gray45", "gray75", "gray20")) +
    theme(
      legend.position = "top", 
      # legend.box = "vertical",
      legend.margin = margin(),
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    )
  
  fig_2b
  
  p = fig_2a + ptheme +
    plot_spacer() + 
    fig_2b + ptheme +
    guides(color="none") + 
    
    plot_layout(guides = 'collect',  # Shared legend 
                widths = c(10,0.5,10)  # Plots are 10x size of spacer
    ) &
    theme(legend.position='bottom')
  ggsave(glue("fig2_{mtype}.pdf"), plot=p, width=14, height=6, device=cairo_pdf)
  
}


######################################################
#### Performance--Fairness Trade-off
######################################################

ensemble_gaps = d %>%
	filter(subgroup %in% INTERSECTIONAL_GROUPS & is_ensemble & is.na(lambda)) %>%
	mutate(etype = paste(ensemble_type, replace_na(ensemble_model_type, ''))) %>%
	group_by(etype, selection_rule, target_institution) %>%
	summarise(auc_gap = diff(range(auc, na.rm=T))) %>%
	select(target_institution, selection_rule, etype, auc_gap)

ensemble_auc = d %>%
	filter(overall & is_ensemble & is.na(lambda)) %>%
	mutate(etype = paste(ensemble_type, replace_na(ensemble_model_type, ''))) %>%
	select(target_institution, selection_rule, etype, auc)
	
inner_join(ensemble_gaps, ensemble_auc) %>%
	ggplot(aes(auc, auc_gap, color=target_institution, shape=etype)) + 
	geom_point(size=4, alpha=0.75) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.justification = "left",
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10, face="bold"),
        legend.margin = margin(t=-5,r=0, b=-5, l=0)) +
	labs(x="Test AUC", y="AUC Gap", shape="Transfer:", color="Target Institution:") +
  guides(colour = guide_legend(nrow = 1), shape = guide_legend(ncol=2)) +
  scale_color_manual(values=c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")) +
  scale_shape_manual(values=c(15, 16, 17, 18))

ggsave("auc_aucGAP_ensemble.pdf", width=5, height=5)
ggsave("auc_aucGAP_ensemble.png", width=5, height=5)




ensemble_gaps = d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS,
         is.na(lambda),
         split=="test",
         selection_rule %in% c(NA, "mlp_only", "gbm_only", "l2lr_only"),
         model_type %in% c("L2LR", "LightGBM", "MLP", NA),
         ensemble_model_type %in% c("L2LR", "LightGBM", "MLP", NA)) %>%
  dplyr::mutate(mty = ifelse(ensemble_model_and_transfer_type == "NA (NA)", 
                             as.character(model_type), 
                             ensemble_model_and_transfer_type)) %>%
  group_by(mty, selection_rule, target_institution) %>%
  summarise(auc_gap = diff(range(auc, na.rm=T)))

ensemble_auc = d %>%
  filter(overall,
         is.na(lambda),
         split=="test",
         selection_rule %in% c(NA, "mlp_only", "gbm_only", "l2lr_only"),
         model_type %in% c("L2LR", "LightGBM", "MLP", NA),
         ensemble_model_type %in% c("L2LR", "LightGBM", "MLP", NA)) %>%
  dplyr::mutate(mty = ifelse(ensemble_model_and_transfer_type == "NA (NA)", 
                             as.character(model_type), 
                             ensemble_model_and_transfer_type)) %>%
  group_by(mty, selection_rule, target_institution) %>%
  select(mty, selection_rule, target_institution, auc)

tmp = inner_join(ensemble_gaps, ensemble_auc)
  
tmp %>% ggplot(aes(auc, auc_gap, color=target_institution, shape=mty)) + 
  geom_point(size=4, alpha=0.75) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.justification = "left",
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10, face="bold"),
        legend.margin = margin(t=-5,r=0, b=-5, l=0)) +
  labs(x="Test AUC", y="AUC Gap", shape="Model Type:", color="Target Institution:") +
  guides(colour = guide_legend(nrow = 1), shape = guide_legend(ncol=4)) +
  scale_color_manual(values=c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")) +
  scale_shape_manual(values=c(15,  0, 16, 1, 17, 2, 18))

ggsave("auc_aucGAP_all.pdf", width=7, height=7)
ggsave("auc_aucGAP_all.png", width=7, height=7)

for (inst in c("A", "B", "C", "D")){
  inst_results = tmp %>% filter(target_institution==inst)
  print(cor.test(inst_results$auc, inst_results$auc_gap))
}

res = lm(auc_gap ~ auc + target_institution + mty, data=tmp)
summary(res)

######################################################
#### Regularization Study Results
######################################################

regularization_plot <- function(df, modeltype = "L2LR"){
  p = df %>%
    filter(overall & !is.na(lambda)) %>%
    mutate(
      model_type = ifelse(is.na(model_type), 
                          as.character(base_model_type), 
                          as.character(model_type)),
      pen = factor(ifelse(lambda==0, "None", log10(lambda)), levels=c("None", (-4:4))),
      src_label = paste0("Src. Inst. ", src_institution),
      tgt_label = paste0("Target Inst. ", target_institution),
    ) %>%
    filter(model_type == modeltype) %>%
    ggplot(aes(pen, auc, group=1, color=factor(src_institution==target_institution))) + 
    geom_point() +
    geom_line() +
    theme_bw() + 
    facet_grid(src_label ~ tgt_label) +
    # scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2), expand=c(0,0)) +
    scale_color_manual(values = c("gray45", "gray10")) +
    theme(
      legend.position = "top", 
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    ) +
    labs(color="Local Model", 
         x=TeX("$L_2$ Penalty: $log_{10}(\\lambda)$"),
         y="AUC",
         caption = glue("Regularization effects for model type {modeltype}."))
  return(p)
}

regularization_plot(d, "L2LR")
ggsave("auc_reg_L2LR.pdf", width=10, height=10)
ggsave("auc_reg_L2LR.png", width=10, height=10)

regularization_plot(d, "LightGBM")
ggsave("auc_reg_LightGBM.pdf", width=10, height=10)
ggsave("auc_reg_LightGBM.png", width=10, height=10)

regularization_plot(d, "MLP")
ggsave("auc_reg_MLP.pdf", width=10, height=10)
ggsave("auc_reg_MLP.png", width=10, height=10)

regularization_subgroup_plot <- function(df, modeltype = "L2LR"){
  p = df %>%
    filter(subgroup %in% INTERSECTIONAL_GROUPS & !is.na(lambda)) %>%
    mutate(
      model_type = ifelse(is.na(model_type), 
                          as.character(base_model_type), 
                          as.character(model_type)),
      pen = factor(ifelse(lambda==0, "None", log10(lambda)), levels=c("None", (-4:4))),
      src_label = paste0("Src. Inst. ", src_institution),
      tgt_label = paste0("Target Inst. ", target_institution),
      grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
      sex = factor(subgroup, labels=c("Female", "Female", "Male", "Male")),
      urm = factor(subgroup, labels=c("Non-URM", "URM", "Non-URM", "URM"))
    ) %>%
    filter(model_type == modeltype) %>%
    ggplot(aes(pen, auc, shape=grp, group=src,
               color=factor(src_institution==target_institution))) + 
    # geom_boxplot(coef=NULL, size=0.5, width=0, position=position_nudge(x=-0.15), show.legend=FALSE) +
    geom_point(stat="identity", size=rel(2), alpha=0.75) +
    theme_bw() + 
    facet_grid(src_label ~ tgt_label) +
    # scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2), expand=c(0,0)) +
    scale_color_manual(values = c("gray45", "gray10")) +
    scale_shape_manual(values = c(0, 15, 2, 17)) +
    theme(
      legend.position = "top", 
      strip.text = element_text(face = "bold"), 
      legend.box = "vertical",
      legend.margin = margin(),
      strip.background = element_rect(fill = "white")
    ) +
    labs(color="Local Model", 
         x=TeX("$L_2$ Penalty: $log_{10}(\\lambda)$"),
         y="AUC",
         shape="Student Subgroup:",
         caption = glue("Regularization effects for model type {modeltype}."))
  return(p)
}

regularization_subgroup_plot(d, "L2LR")
regularization_subgroup_plot(d, "LightGBM")
regularization_subgroup_plot(d, "MLP")


######################################################
#### Additional Supplementary figures
######################################################


# Per-subgroup AUC , with CIs
fig_per_subgroup_auc <- d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS & is.na(lambda) & split=="test" & selection_rule %in% c(NA, "l2lr_only") & model_type %in% c("L2LR", NA) & ensemble_model_type %in% c("L2LR", NA)) %>%
  mutate(
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
    sex = factor(subgroup, labels=c("Female", "Female", "Male", "Male")),
    urm = factor(subgroup, labels=c("Non-URM", "URM", "Non-URM", "URM")),
    src = factor(src, labels=c("A","B","C","D","Voting", "Stacked")),
    lde = factor(lde, levels = c("Local Model", "Direct Transfer", "Ensemble Model"))
  ) %>%
  ggplot(aes(x=src, y=auc, color=lde, shape=grp)) +
  geom_point(stat="identity", position = position_dodge(width=0.5)) +
  geom_errorbar(aes(x = src,
                    ymin=ifelse(auc - CI_WIDTH * seauc < 0, 0, auc - CI_WIDTH * seauc),
                    ymax=ifelse(auc + CI_WIDTH * seauc > 1.0, 1.0, auc + CI_WIDTH * seauc),
                    width=0.2),
                position=position_dodge((width=0.5))) +
  labs(x="Source Institution / Ensemble Method", 
       color="Model Type:", 
       shape="Student Subgroup:", 
       y="Test AUC") +
  facet_wrap( ~ paste("Target Institution:", target_institution), scales = "free_x") +
  theme_bw() + 
  # scale_y_continuous(limits = c(.55,1), breaks = seq(0,1,.1), expand=c(0,0)) +
  scale_y_continuous(limits = c(0.4, 1), breaks = seq(0,1,.1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
  scale_color_manual(values = c("gray75", "gray45", "gray10")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  theme(
    legend.position = "bottom", 
    legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )

fig_per_subgroup_auc

ggsave("sexURM_L2LR_auc_ens_intersectional.pdf", width=7, height=6)



# Results of each institutions' local L2LR model, on itself, by subgroup
same_institution_results_intersectional = d %>% 
  filter(
    subgroup %in% INTERSECTIONAL_GROUPS,
    split=="test",
    is.na(lambda), 
    src_institution == target_institution,
    model_type=="L2LR") %>%
  select(c("auc", "src_institution", "seauc", 
           "subgroup", "lambda",
           "target_institution", "model_type",
           "selection_rule"))


# Results of all transfer models (of all types), excluding the local/"same-institution" results above.
cross_institution_results_intersectional = d %>% 
  filter(
    subgroup %in% INTERSECTIONAL_GROUPS,
    split=="test",
    is.na(lambda), 
    src_institution != target_institution,
    (model_type=="L2LR" | 
       (ensemble_model_and_transfer_type %in% c("L2LR (Stacked)", 
                                                "NA (Majority Voting)")
        & selection_rule=="l2lr_only"))) %>%
  select(c("auc", "src_institution", "seauc", 
           "subgroup", "lambda",
           "target_institution", "model_type",
           "selection_rule",
           "ensemble_model_and_transfer_type", 
           "ensemble_model_type",
           "lde",
           "ensemble_type"))


delta_aucs_intersectional = merge(
  cross_institution_results_intersectional, 
  same_institution_results_intersectional,
  by=c("target_institution", "subgroup", "lambda"),
  suffixes = c("_transfer", "_same")) %>%
  dplyr::rename(src_institution = src_institution_transfer, 
                model_type=model_type_transfer,
                selection_rule=selection_rule_transfer) %>% 
  mutate("delta_auc" = auc_transfer - auc_same) %>%
  mutate("se_delta_auc" = sqrt(seauc_transfer^2 + seauc_same^2)) %>%
  select(c("src_institution", "target_institution", 
           "subgroup", "lambda",
           "delta_auc", "se_delta_auc", "model_type",
           "selection_rule",
           "ensemble_model_type",
           "lde",
           "ensemble_model_and_transfer_type", "ensemble_type"))


# Per-subgroup Delta AUC , with CIs

yrange = 0.5  # lower/upper bound of delta auc in plot below.

fig_per_subgroup_delta_auc <- delta_aucs_intersectional %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS,
         is.na(lambda),
         selection_rule %in% c(NA, "l2lr_only"),
         model_type %in% c("L2LR", NA),
         ensemble_model_type %in% c("L2LR", NA)) %>%
  mutate(
    grp = factor(subgroup, labels=c("Female Non-URM", "Female URM", "Male Non-URM", "Male URM")),
    sex = factor(subgroup, labels=c("Female", "Female", "Male", "Male")),
    urm = factor(subgroup, labels=c("Non-URM", "URM", "Non-URM", "URM")),
    lde = factor(lde, levels = c("Local Model", "Direct Transfer", "Ensemble Model")),
    model = ifelse(src_institution==target_institution,
                                "Local Model",
                                ifelse(model_type=="L2LR",
                                       "External Model",
                                       "Ensemble Model")),
    src = ifelse(is.na(ensemble_type), src_institution, as.character(ensemble_type)),
    src = dplyr::recode(src, `Majority Voting`="Voting"),
    src = factor(src),
  ) %>%
  # ggplot(aes(x=grp, y=delta_auc, color=lde, shape=grp, group=src)) +
  ggplot(aes(x=src, y=delta_auc, color=lde, shape=grp)) +
  geom_point(position=position_dodge((width=0.5))) +
  geom_errorbar(aes(x = src, 
                    ymin=ifelse(delta_auc - CI_WIDTH * se_delta_auc < -yrange, -yrange, delta_auc - CI_WIDTH * se_delta_auc),
                    ymax=ifelse(delta_auc + CI_WIDTH * se_delta_auc > yrange, yrange, delta_auc + CI_WIDTH * se_delta_auc),
                    width=0.2),
                position=position_dodge((width=0.5))) + 
  geom_hline(yintercept=0, col="dodgerblue", alpha=0.4) + 
  labs(x="Source Institution / Ensemble Method",
       color="Model Type:",
       shape="Student Subgroup:",
       y=TeX("$\\Delta$AUC")) +
  facet_wrap( ~ paste("Target Institution:", target_institution), scales = "free_x") +
  theme_bw() + 
  # scale_y_continuous(limits = c(.55,1), breaks = seq(0,1,.1), expand=c(0,0)) +
  scale_y_continuous(limits = c(-yrange, yrange), breaks = seq(-yrange, yrange, .1), expand=c(0,0)) +
  scale_fill_manual(values = c("gray75", "gray45", "gray10")) +
  scale_color_manual(values = c("gray75", "gray45", "gray10")) +
  scale_shape_manual(values = c(0, 15, 2, 17)) +
  theme(
    legend.position = "bottom", 
    legend.box = "vertical",
    legend.margin = margin(),
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  )
fig_per_subgroup_delta_auc 
ggsave("sexURM_L2:R_delta_auc_intersectional.pdf", width=7, height=6)


## Regularization analysis


# Overall AUC vs. lambda, for each model type

for (mtype in c("MLP", "LightGBM" ,"L2LR")){

  message(mtype)
  
  
  lambda_overall = d %>%
    filter(overall, 
           model_type == mtype,
           split=="test") %>%
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
      legend.position = "bottom", 
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    )
  ggsave(glue("overall_{mtype}_auc.pdf"), width=7, height=6)
  
  d %>%
    filter(subgroup %in% INTERSECTIONAL_GROUPS,
           model_type == mtype,
           split=="test") %>%
    mutate(pen = factor(ifelse(lambda==0, "None", log10(lambda)), levels=c("None", (-4:4)))) %>%
    ggplot(aes(pen, auc, color=src_institution, group=target_institution)) + 
    geom_line() + 
    geom_point() +
    # geom_errorbar(aes(ymin = auc - seauc, ymax = auc + seauc), width=0.2) +
    facet_grid(paste("Source Institution:", src_institution) ~ subgroup) +
    labs(x=TeX("$L_2$ Penalty: $log_{10}(\\lambda)$"), color="Source Model", y="Test AUC") +
    scale_color_manual(values = c("#8C1D40", "red", "#FFCB05", "#0064A4")) +
    theme_bw() + 
    theme(
      legend.position = "top", 
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "white")
    )
  ggsave(glue("subgroup_{mtype}_auc.pdf"), width=7, height=6)
  
  
  
  # Auc gap vs. lambda with "star" markers at best value of lambda by test AUC
  best_lambda_per_group = lambda_overall %>% 
    group_by(src_institution, target_institution) %>% 
    mutate(max_auc = max(auc)) %>% 
    ungroup() %>%
    filter(auc==max_auc) %>%
    select(c(src_institution, target_institution, pen, auc))
  best_lambda_per_group$best_lambda = TRUE
  
  # Co-plot of AUC and AUC Gap, with best lambda starred.
  best_lambda_plot = auc_summary %>% dplyr::left_join(
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
  
  best_lambda_plot
  
  
  subgroups_lambda_plot = d %>%
    filter(subgroup %in% INTERSECTIONAL_GROUPS & model_type == mtype) %>%
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
  subgroups_lambda_plot
  
  p = best_lambda_plot +  plot_spacer() + 
    subgroups_lambda_plot + guides(color="none") + 
    plot_layout(guides = 'collect',  # Shared legend 
                widths = c(10,0.5,10)  # Plots are 10x size of spacer
    ) &
    theme(legend.position='bottom')
  ggsave(glue("regularization_subgroup_analysis_{mtype}.pdf"), plot=p, width=14, height=6, device=cairo_pdf)
  
}



auc_summary = d %>%
  filter(subgroup %in% INTERSECTIONAL_GROUPS,
         model_type == "L2LR") %>%
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
  

######################################################
#### Stylized/arranged figures for paper
######################################################

ptheme = theme(strip.text.x = element_text(size=16),
               axis.title = element_text(size=16),
               axis.text = element_text(size=10))
fig_2a + ptheme +
  plot_spacer() + 
  fig_2b + ptheme +
  guides(color="none") + 
  
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
  ) &
  theme(legend.position='bottom')
ggsave("fig2.pdf", width=14, height=6, device=cairo_pdf)




fig_3a_marginal + ptheme + plot_spacer() + 
  fig_3b_marginal + ptheme + guides(color="none") + 
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
  ) &
  theme(legend.position='bottom')
ggsave("fig3_marginal.pdf", width=14, height=6, device=cairo_pdf)

fig_per_subgroup_auc + ptheme + plot_spacer() + 
  fig_per_subgroup_delta_auc + ptheme + guides(color="none") + 
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
  ) &
  theme(legend.position='bottom')
ggsave("per_subgroup_results.pdf", width=14, height=6, device=cairo_pdf)

best_lambda_plot +  plot_spacer() + 
  subgroups_lambda_plot + guides(color="none") + 
  plot_layout(guides = 'collect',  # Shared legend 
              widths = c(10,0.5,10)  # Plots are 10x size of spacer
  ) &
  theme(legend.position='bottom')
ggsave("regularization_subgroup_analysis.pdf", width=14, height=6, device=cairo_pdf)

