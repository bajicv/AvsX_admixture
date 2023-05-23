################################################################################
##
## Correcting Females-only dataset to match Males-only and Both-Sexes datasets
## Bringing K=4 to K=3 by merging East Asian and Eurasian components together
## 
## Vladimir Bajic
## 26.11.2021
##
################################################################################

library(tidyverse)
library(pheatmap)

# Plotting heatplots for each meanQ file to detect which columns should be merged

fam_file <- read_tsv("Data/africa_data/fam_with_meta_africa_and_refpop_0sexRem_dupRem_MindGeno_IndPw_pruned_REMOVEDIND_YMTRem.txt", col_names = F)

a_dir_path <- "Data/africa_data/A_F_AC_sum_Han_French/"

files <- dir(path = a_dir_path, pattern = paste0("NEW.*.", "4", ".meanQ"), full.names = T, recursive = T)

POP <- fam_file %>% 
  filter(X5 == 2) %>% 
  pull(X10)

for (i in 1:length(files)){
  df <- read_table(files[i], col_names = F)
  df_pop <- bind_cols(POP = POP, df)
  p <- pheatmap(df_pop[,2:ncol(df_pop)], labels_row = POP)
  ggsave(filename = paste0("NEW_X_pheatplot_K4_i",i,".png"),plot = p, device = png)
}

################################################################################

# Make new meanQ-like files in which two columns representing East Asian and
# Westeurasian components are merged into one column

old_dir_path <- "Data/africa_data/A_F_AC/"
out_dir_path <- "Out/africa_data/A_F_AC_sum_Han_French/"

# make function to create new table 
# given the run and which columns to sum and remove
make_new_table <- function(R, Col_1, Col_2){
  r <- R
  col_1 <- Col_1  
  col_2 <- Col_2
  file <- dir(path = old_dir_path, pattern = paste0(".*.", "R", r ,".4", ".meanQ"), full.names = F, recursive = T)
  df <- read_table(paste0(old_dir_path, file), col_names = F)

  df %>% 
    mutate(sumC = .[[col_1]] + .[[col_2]]) %>% 
    select(-all_of(c(col_1, col_2))) %>% 
    write.table(file = paste0(out_dir_path,"NEW_",file), quote = F, col.names = F, row.names = F)
}

# Now call function based on what you observed from heatplots

# A_F_AC_K4 :
# r col1 col2
old_dir_path <- "Data/africa_data/A_F_AC/"
out_dir_path <- "Output/africa_Output/Data_output/A_F_AC_sum_Han_French/"
make_new_table(1, 1, 3)
make_new_table(3, 1, 3)
make_new_table(5, 1, 3)
make_new_table(6, 1, 3)
make_new_table(7, 1, 2)
make_new_table(9, 1, 4)



# X_F_K4 :
# r col1 col2
old_dir_path <- "Data/africa_data/X_F/"
out_dir_path <- "Output/africa_Output/Data_output/X_F_sum_Han_French/"
make_new_table(0, 1, 4)
make_new_table(2, 2, 4)
make_new_table(3, 1, 4)
make_new_table(4, 1, 3)
make_new_table(5, 1, 2)
make_new_table(6, 2, 4)
make_new_table(7, 1, 4)
make_new_table(8, 2, 3)
make_new_table(9, 2, 4)



################################################################################
