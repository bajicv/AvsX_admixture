################################################################################
# define variables

source(file = "Calc_Cor_AvsX_Qfiles.R")

a_dir_path <- "africa_data/A_F_AC/"
x_dir_path <- "africa_data/X_F/"
out_name_prefix <- "Africa_F"
K <- 4

setwd("africa_data/")
CorAX_table(a_dir_path, x_dir_path, K, out_name_prefix)

################################################################################