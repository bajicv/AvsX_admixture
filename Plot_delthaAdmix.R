# Autosomes vs X chr

library("tidyverse")

df_dif <- read_tsv("diff_meta_merged_africa_A_B_AC_vs_X_B.csv")
fam_file <- read_tsv("fam_with_meta_africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem.txt", col_names = F)
a_file <- read_table("africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_autosome_R1.4.meanQ", col_names = paste0("a", seq_along(a_file)))
x_file <- read_table("africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_Xchr_R8.4.meanQ", col_names = paste0("x", seq_along(a_file)))


df_pops <- bind_cols(POP = fam_file$X10, Region = fam_file$X7, df_dif, a_file, x_file)
#names(df_pops) <- c("POP", "Region", "W_African","Eurasian","E_Asian","S_African")
#df_pops_ord <- df_pops %>% relocate(Region, POP, S_African, W_African, Eurasian, E_Asian)



unique(df_pops$POP)

df_pops %>% 
  filter(Region == "WestEurasia") %>%
  select(-Region) %>%
  pivot_longer(!POP, names_to = "Ks", values_to = "diff") %>%
  ggplot(., aes(x=Ks, y=diff, fill = Ks)) + 
  geom_boxplot() +
  ylim(-1.1,1.1) +
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  labs(title="Sex-Bias",
       x = "K",
       y = "ΔAdmix",
       fill = "Ks") + 
  geom_hline(yintercept = 0, alpha = 0.5) + 
  facet_wrap(~POP) + 
  theme_bw()

#######################################################

ggplot(df_pops, aes(x=a4, y=x4)) + 
  geom_point(aes(color = POP, shape = POP), show.legend = FALSE) + 
  geom_abline(slope = 1,intercept = 0) +
  geom_abline(slope = 1,intercept = 0.25, alpha=0.25) +
  geom_abline(slope = 1,intercept = -0.25, alpha=0.25) +
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  coord_fixed(ratio = 1) +
  facet_wrap(~POP)

ggplot(df_pops, aes(x=a1, y=x2)) + 
  geom_point(aes(color = POP, shape = POP)) + 
  geom_abline(slope = 1,intercept = 0) +
  geom_abline(slope = 1,intercept = 0.25, alpha=0.25) +
  geom_abline(slope = 1,intercept = -0.25, alpha=0.25) +
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  coord_fixed(ratio = 1)

ggplot(df_pops, aes(x=a2, y=x1, color = POP)) + 
  geom_point(aes(color = POP, shape = POP)) + 
  geom_abline(slope = 1,intercept = 0) +
  geom_abline(slope = 1,intercept = 0.25, alpha=0.25) +
  geom_abline(slope = 1,intercept = -0.25, alpha=0.25) +
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  coord_fixed(ratio = 1)

ggplot(df_pops, aes(x=a3, y=x3, color = POP)) + 
  geom_point(aes(color = POP, shape = POP)) + 
  geom_abline(slope = 1,intercept = 0) +
  geom_abline(slope = 1,intercept = 0.25, alpha=0.25) +
  geom_abline(slope = 1,intercept = -0.25, alpha=0.25) +
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  coord_fixed(ratio = 1)

#####

ggplot(df_pops, aes(x=a4, y=((x4-a4)/(x4+a4)))) + 
  geom_point(aes(color = POP, shape = POP)) + 
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  geom_hline(yintercept=0)

ggplot(df_pops, aes(x=a1, y=((x2-a1)/(x2+a1)))) +
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  geom_point(aes(color = POP, shape = POP)) + 
  geom_hline(yintercept=0)

ggplot(df_pops, aes(x=a2, y=((x1-a2)/(x1+a2)))) +   
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  geom_point(aes(color = POP, shape = POP)) + 
  geom_hline(yintercept=0)

ggplot(df_pops, aes(x=a3, y=((x3-a3)/(x3+a3)))) + 
  geom_point(aes(color = POP, shape = POP)) + 
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  geom_hline(yintercept=0)


# per pop
tmp_test <- df_pops %>% 
  select(-c("X1", "diff_of_0_and_1", "diff_of_1_and_0", "diff_of_2_and_2", "diff_of_3_and_3")) %>%
  filter(POP=="Gana") %>%
  mutate(Diff1 = x2 - a1,Diff2 = x1 - a2, Diff3 = x3 - a3, Diff4 = x4 - a4) %>%
  #gather("a1","a2","a3","a4","x1","x2","x3","x4", key = Anestry, value = Anc_Freq)
  #gather("a1","Diff1", key = Anc, value = Anc_Freq)
  gather("Diff1","Diff2", "Diff3", "Diff4", key = Diff, value = Diff_Freq) %>%
  gather("a1","a2","a3","a4", key= Aut, value = Aut_Freq)

tmp_test %>%
ggplot(., aes(x=Aut_Freq, y=Diff_Freq)) + 
  geom_point(aes(color = Diff, shape = Aut)) + 
  scale_shape_manual(values=rep(seq(0,25), 3)) +
  geom_hline(yintercept=0)



df_pops %>% 
  filter(Region == "WestEurasia") %>%
  select(-Region) %>%
  pivot_longer(
    cols = 
  )


pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)


######################

tmp_sub <- df_pops %>%
  select(-c(`...3`, Continent, Country, diff_of_0_and_1, diff_of_1_and_0, diff_of_2_and_2, diff_of_3_and_3))

names(tmp_sub)[8] = "X2"
names(tmp_sub)[9] = "X1"
names(tmp_sub)[10] = "X3"
names(tmp_sub)[11] = "X4"

tmp_sub[,c(4:11)] %>% 
  as.matrix() %>%
  heatmap(labRow=tmp_sub$POP)

toplot_tmp <- tmp_sub %>%
  pivot_longer(A1:x3, 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)")


ggplot(toplot_tmp, aes(x=A, y=x, color = Ancestry, shape = Ancestry)) + 
  geom_point(show.legend = FALSE) + 
  geom_abline() +
  geom_abline(slope = 1,intercept = 0.1, alpha=0.1) +
  geom_abline(slope = 1,intercept = -0.1, alpha=0.1) +
  geom_abline(slope = 1,intercept = 0.2, alpha=0.2) +
  geom_abline(slope = 1,intercept = -0.2, alpha=0.2) +
  geom_abline(slope = 1,intercept = 0.3, alpha=0.3) +
  geom_abline(slope = 1,intercept = -0.3, alpha=0.3) +
  geom_abline(slope = 1,intercept = 0.4, alpha=0.4) +
  geom_abline(slope = 1,intercept = -0.4, alpha=0.4) +
  geom_abline(slope = 1,intercept = 0.5, alpha=0.5) +
  geom_abline(slope = 1,intercept = -0.5, alpha=0.5) +
  geom_abline(slope = 1,intercept = 0.6, alpha=0.6) +
  geom_abline(slope = 1,intercept = -0.6, alpha=0.6) +
  geom_abline(slope = 1,intercept = 0.7, alpha=0.7) +
  geom_abline(slope = 1,intercept = -0.7, alpha=0.7) +
  geom_abline(slope = 1,intercept = 0.8, alpha=0.8) +
  geom_abline(slope = 1,intercept = -0.8, alpha=0.8) +
  geom_abline(slope = 1,intercept = 0.9, alpha=0.9) +
  geom_abline(slope = 1,intercept = -0.9, alpha=0.9) +
  geom_abline(slope = 1,intercept = 1, alpha=1) +
  geom_abline(slope = 1,intercept = -1, alpha=1) +
  geom_abline(slope = -1, intercept = 1) +
  coord_fixed(ratio = 1) +
  facet_wrap(~POP)



mean_toplot_tmp <- toplot_tmp %>%
  group_by(POP, Region, Ancestry) %>%
  summarize(mean_a = mean(a), mean_x = mean(X))




ggplot(mean_toplot_tmp, aes(x=mean_a, y=mean_x, color = Ancestry, shape = Ancestry)) + 
  geom_point(show.legend = FALSE) + 
  geom_abline() +
  geom_abline(slope = 1,intercept = 0.1, alpha=0.1) +
  geom_abline(slope = 1,intercept = -0.1, alpha=0.1) +
  geom_abline(slope = 1,intercept = 0.2, alpha=0.2) +
  geom_abline(slope = 1,intercept = -0.2, alpha=0.2) +
  geom_abline(slope = 1,intercept = 0.3, alpha=0.3) +
  geom_abline(slope = 1,intercept = -0.3, alpha=0.3) +
  geom_abline(slope = 1,intercept = 0.4, alpha=0.4) +
  geom_abline(slope = 1,intercept = -0.4, alpha=0.4) +
  geom_abline(slope = 1,intercept = 0.5, alpha=0.5) +
  geom_abline(slope = 1,intercept = -0.5, alpha=0.5) +
  geom_abline(slope = 1,intercept = 0.6, alpha=0.6) +
  geom_abline(slope = 1,intercept = -0.6, alpha=0.6) +
  geom_abline(slope = 1,intercept = 0.7, alpha=0.7) +
  geom_abline(slope = 1,intercept = -0.7, alpha=0.7) +
  geom_abline(slope = 1,intercept = 0.8, alpha=0.8) +
  geom_abline(slope = 1,intercept = -0.8, alpha=0.8) +
  geom_abline(slope = 1,intercept = 0.9, alpha=0.9) +
  geom_abline(slope = 1,intercept = -0.9, alpha=0.9) +
  geom_abline(slope = 1,intercept = 1, alpha=1) +
  geom_abline(slope = 1,intercept = -1, alpha=1) +
  geom_abline(slope = -1, intercept = 1) +
  coord_fixed(ratio = 1) +
  facet_wrap(~POP)
  


################################
# only females

a_file <- read_table("africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_autosome_femalesOnly_R8.4.meanQ", col_names = c("a1","a2","a3","a4"))
x_file <- read_table("africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_Xchr_femalesOnly_R9.4.meanQ", col_names = c("x1","x2","x3","x4"))
fam_file <- read_tsv("fam_with_meta_africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem.txt", col_names = F)

female_fam_file <- fam_file %>% 
  filter(X5 == 2)

df_pops <- bind_cols(POP = female_fam_file$X10, 
                     Region = female_fam_file$X7, 
                     a_file, 
                     x_file)


df_pops[,c(3:10)] %>% 
  as.matrix() %>%
  heatmap(labRow=df_pops$POP)


names(df_pops)[7] = "X4"
names(df_pops)[8] = "X3"
names(df_pops)[9] = "X2"
names(df_pops)[10] = "X1"


toplot_tmp <- df_pops %>%
  pivot_longer(a1:X1, 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)")



################################
# only males

a_file <- read_table("africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_autosome_malesOnly_R4.4.meanQ", col_names = c("a1","a2","a3","a4"))
x_file <- read_table("africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem_Xchr_malesOnly_R4.4.meanQ", col_names = c("x1","x2","x3","x4"))
fam_file <- read_tsv("fam_with_meta_africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem.txt", col_names = F)


male_fam_file <- fam_file %>% 
  filter(X5 == 1)


df_pops <- bind_cols(POP = male_fam_file$X10, 
                     Region = male_fam_file$X7, 
                     a_file, 
                     x_file)


df_pops %>% 
  select(starts_with(c("a","x"))) %>% 
  as.matrix() %>%
  heatmap(labRow=df_pops$POP)


names(df_pops)[7] = "X3"
names(df_pops)[8] = "X1"
names(df_pops)[9] = "X4"
names(df_pops)[10] = "X2"


toplot_tmp <- df_pops %>%
  pivot_longer(a1:X1, 
               names_to = c(".value", "Ancestry"), 
               names_pattern = "(.)(.)")


cor(df_pops$a2, df_pops$a1)

cor.test(c(1,2,3,4),c(1,1,1,1))
cor.test(c(1,2,3,4),c(1,1,1,1), method="pearson")      


x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor(df_pops$a2, df_pops$a1, method="pearson")

summary(df_pops$a2)


cor.test(df_pops$a1, df_pops$x2)

cor.test(df_pops$a2, df_pops$x4)

cor.test(df_pops$a3, df_pops$x1)

cor.test(df_pops$a4, df_pops$x3)


df_pops %>% 
  select(starts_with(c("a","x"))) %>% 
  cor()


playing_df_pops <- df_pops %>% 
  mutate(x1and3 = x1 + x3) %>% 
  select(-c(a2, x1, x3))


playing_df_pops %>% 
  select(starts_with(c("a","x"))) %>% 
  as.matrix() %>%
  heatmap(labRow=df_pops$POP)