# Ancestry Packages HGDP
# autosomes vs X chr

library("tidyverse")

ind2pop_file <- read_table("ancestry_packages_HGDP_AvsX/pong_ind2pop.txt", col_names = "POP")
aQfile <- "ancestry_packages_HGDP_AvsX/Stoneking_Africa+RefPop_0sexRem_MindGeno_RelRem_hwe_IndPw_pruned_autosome_K15.Run0.Q"
xQfile <- "ancestry_packages_HGDP_AvsX/Stoneking_Africa+RefPop_0sexRem_MindGeno_RelRem_hwe_IndPw_pruned_Xchr_K15.Run3.Q"


a_file <- aQfile %>% 
  read_table(col_names = F, progress = F, col_types = cols()) %>% 
  rename_with(~gsub("X", "A", .x, fixed = TRUE))

x_file <- xQfile %>% 
  read_table(col_names = F, progress = F, col_types = cols())


df_pops <- bind_cols(POP = ind2pop_file$POP, a_file, x_file)


unique(df_pops$POP)

# renaming variables
# heatplot
df_pops %>% 
  select(matches("[A|X][1-9]")) %>% 
  as.matrix() %>%
  heatmap(labRow=df_pops$POP)

old_x_names <- cor(a_file, x_file) %>% 
  as_tibble(rownames = "A") %>% 
  pivot_longer(!A, names_to = "X", values_to = "cor") %>%
  group_by(A) %>% 
  slice_max(cor) %>% 
  ungroup() %>% 
  pull(X)

df_pops_renamed <- df_pops %>% 
  rename_at(vars(old_x_names), ~paste0("X", 1:length(old_x_names)))

num_pop <- length(unique(df_pops_renamed$POP))

# diamond plots with all individuals
df_pops_renamed %>%
  pivot_longer(cols = -c(POP), 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)") %>% 
  ggplot(aes(x=A, y=X, color = Ancestry)) + 
  geom_point(show.legend = FALSE) + 
  geom_abline() +
  geom_abline(intercept = rep(seq(from =0.1, to=1, by=0.1), num_pop),
              alpha= rep(seq(from =0.1, to=1, by=0.1), num_pop*num_pop),
              slope = 1, color = "#e31a1c") +
  geom_abline(intercept = rep(seq(from =-1, to=-0.1, by=0.1), num_pop),
              alpha=rep(seq(from =1, to=0.1, by=-0.1), num_pop*num_pop),
              slope = 1, color = "#1f78b4") +  
  geom_abline(slope = -1, intercept = 1, linetype = "dashed", color = "gray") +
  coord_fixed(ratio = 1) +
  facet_wrap(~POP)


# diamond plots with all individuals
df_pops_renamed %>%
  pivot_longer(cols = -c(POP), 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)") %>% 
  group_by(POP, Ancestry) %>%
  summarize(mean_a = mean(A), mean_x = mean(X)) %>% 
  ggplot(aes(x=mean_a, y=mean_x, color = Ancestry)) + 
  geom_point(show.legend = FALSE) + 
  geom_abline() +
  geom_abline(intercept = rep(seq(from =0.1, to=1, by=0.1), num_pop),
              alpha= rep(seq(from =0.1, to=1, by=0.1), num_pop*num_pop),
              slope = 1, color = "#e31a1c") +
  geom_abline(intercept = rep(seq(from =-1, to=-0.1, by=0.1), num_pop),
              alpha=rep(seq(from =1, to=0.1, by=-0.1), num_pop*num_pop),
              slope = 1, color = "#1f78b4") +
  geom_abline(slope = -1, intercept = 1, linetype = "dashed") +
  coord_fixed(ratio = 1) +
  facet_wrap(~POP)


# diamond plots with all individuals TESTING simplyfied version
df_pops_renamed %>%
  pivot_longer(cols = -c(POP), 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)") %>% 
  ggplot(aes(x=A-X, y=Ancestry)) + 
  geom_violin(aes(color = Ancestry, alpha = A)) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               aes(colour = Ancestry, alpha=A)) +
  geom_point(aes(alpha = A, color = Ancestry, position = "jitter"), size = 0.5) + 
  geom_vline(xintercept = 0) +
  facet_wrap(~POP)


# boxplot
df_pops_renamed %>%
  pivot_longer(cols = -c(POP), 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)") %>% 
  ggplot(aes(x=A-X, y=Ancestry)) + 
  geom_boxplot(aes(color = Ancestry)) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  facet_wrap(~POP)