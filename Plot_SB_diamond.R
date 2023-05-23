# Plot sex-bias in diamond plots


library("tidyverse")

pth_fam <- "fam_with_meta_africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem.txt"
# pth_a <- "africa_data/A_B_AC/africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_autosome_R5.5.meanQ"
# pth_x <- "africa_data/X_B/africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_Xchr_R1.5.meanQ"

# pth_a <- "africa_data/A_B_AC/africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_autosome_R0.6.meanQ"
# pth_x <- "africa_data/X_B/africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_Xchr_R6.6.meanQ"

# pth_a <- "africa_data/A_F_AC/africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_autosome_femalesOnly_R1.4.meanQ"
# pth_x <- "africa_data/X_F/africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_Xchr_femalesOnly_R9.4.meanQ"
# fam_file <- fam_file %>% filter(X5 == 2) # only females
 
pth_a <- "africa_data/A_M_AC/africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_autosome_malesOnly_R9.3.meanQ"
pth_x <- "africa_data/X_M/africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_Xchr_malesOnly_R6.3.meanQ"
fam_file <- fam_file %>% filter(X5 == 1) # only males

# for HGDP
pth_fam <- "fam_with_meta_HGDP_0sexRem_YMTRem_MindGeno_pruned.txt"
pth_a <- "copy_hgdp/A_B_AC/HGDP_0sexRem_YMTRem_MindGeno_pruned_autosome_R9.6.meanQ"
pth_x <- "copy_hgdp/X_B/HGDP_0sexRem_YMTRem_MindGeno_pruned_Xchr_R8.6.meanQ"
df_pops <- bind_cols(POP = fam_file$X12, 
                     Region = fam_file$X7, 
                     a_file, x_file)



fam_file <- read_tsv(pth_fam, col_names = F)
a_file <- read_table(pth_a, col_names = F) %>% 
  rename_with(~gsub("X", "A", .x, fixed = TRUE))
x_file <- read_table(pth_x, col_names = F)

df_pops <- bind_cols(POP = fam_file$X10, 
                     Region = fam_file$X7, 
                     a_file, x_file)

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
  pivot_longer(cols = -c(POP,Region), 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)") %>% 
  ggplot(aes(x=A, y=X, color = Ancestry, shape = Ancestry)) + 
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
  pivot_longer(cols = -c(POP,Region), 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)") %>% 
  group_by(POP, Ancestry) %>%
  summarize(mean_a = mean(A), mean_x = mean(X)) %>% 
  ggplot(aes(x=mean_a, y=mean_x, color = Ancestry, shape = Ancestry)) + 
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
  pivot_longer(cols = -c(POP,Region), 
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
  pivot_longer(cols = -c(POP,Region), 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)") %>% 
  ggplot(aes(x=A-X, y=Ancestry)) + 
  geom_boxplot(aes(color = Ancestry)) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  facet_wrap(~POP)

df_pops_renamed %>% 
  arrange(-A1)