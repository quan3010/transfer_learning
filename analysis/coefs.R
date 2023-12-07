
source("utils.R")
coefs_overlap = read.csv("../metrics/coefs_at_k.csv")

# ASU-> A; ASUO -> B; UCI -> C; UM -> D
coefs_overlap$inst_x = recode_factor(coefs_overlap$inst_x,  "asu" = "A", "asuo" = "B", "uci" = "C", "um" = "D")
coefs_overlap$inst_y = recode_factor(coefs_overlap$inst_y,  "asu" = "A", "asuo" = "B", "uci" = "C", "um" = "D")

ggplot(coefs_overlap, aes(x=k, y=overlap)) + 
  geom_line() + 
  geom_abline(slope=1, intercept=1, color="dodgerblue") + 
  facet_wrap(~paste("Institution", inst_x, "vs. Institution", inst_y), scales = "free_x") +
  # facet_wrap(vars(inst_x, inst_y), labeller = "label_both")
  theme_bw() + 
  theme(
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  ) + 
  ylab("Overlap@k")
ggsave("overlap_at_k.pdf", width=7, height=6)

ggplot(coefs_overlap, aes(x=k, y=overlap/k)) + 
  geom_line() + 
  geom_abline(slope=1, intercept=1, color="dodgerblue") + 
  facet_wrap(~paste("Institution", inst_x, "vs. Institution", inst_y), scales = "free_x") +
  # facet_wrap(vars(inst_x, inst_y), labeller = "label_both")
  theme_bw() + 
  theme(
    strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = "white")
  ) + 
  ylab("Normalized Overlap@k")
ggsave("overlap_at_k_normalized.pdf", width=7, height=6)
