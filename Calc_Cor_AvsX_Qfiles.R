################################################################################
################################################################################
##
##  Script to check correlation between autosomal Q files and X-chr Q files and
##  output it as sorted table with best pair of files at the top
##  it works with ADMIXTURE and fastSTRUCTURE output files
## 
##  Vladimir Bajic
##  30-Sep-21
##
################################################################################
################################################################################

# load library
library(tidyverse)

################################################################################
# define function to calculate correlation between A and X files

a_x_cor <- function(aQfile, xQfile){
  
  a_file <- aQfile %>% 
    read_table(col_names = F, progress = F, col_types = cols()) %>% 
    rename_with(~gsub("X", "A", .x, fixed = TRUE))
  
  x_file <- xQfile %>% 
    read_table(col_names = F, progress = F, col_types = cols())
  
  aK <- ncol(a_file)
  xK <- ncol(x_file)
  
  aR <- aQfile %>% 
    str_match("R[0-9]+.[0-9]+") %>% 
    str_sub(2,2) %>% 
    as.numeric()
  
  xR <- xQfile %>% 
    str_match("R[0-9]+.[0-9]+") %>% 
    str_sub(2,2) %>% 
    as.numeric()
  
  a_empty <- a_file %>% 
    summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
    pivot_longer(cols = everything(), names_to = "columns", values_to = "sum") %>% 
    filter(near(sum, 0)) %>% 
    nrow()
  
  x_empty <- x_file %>% 
    summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
    pivot_longer(cols = everything(), names_to = "columns", values_to = "sum") %>% 
    filter(near(sum, 0)) %>% 
    nrow()
  
  if (aK == xK){
    result <- cor_result <- cor(a_file, x_file) %>% 
      as_tibble(rownames = "A") %>% 
      pivot_longer(!A, names_to = "X", values_to = "cor") %>% 
      slice_max(order_by = cor, n=aK) %>% 
      arrange(A) %>% 
      summarise(aK = aK,
                xK = xK,
                aR = aR,
                xR = xR,
                a_empty = a_empty,
                x_empty = x_empty,
                min_cor= round(min(cor), digits = 5),
                max_cor= round(max(cor), digits = 5),
                mean_cor = round(mean(cor), digits = 5),
                median_cor = round(median(cor), digits = 5))
    
    return(result)
  } else {
    print("Error: The number of Ks in autosomal file is not equal to the number of Ks in X-chr file")
  }
}

################################################################################
# define function to output table with correlations between A and X files

CorAX_table <- function(a_dir_path, x_dir_path, K, out_name_prefix) {
  
  filesA <- dir(path = a_dir_path, pattern = paste0(".*.", K, ".meanQ"), full.names = T, recursive = T)
  filesX <- dir(path = x_dir_path, pattern = paste0(".*.", K, ".meanQ"), full.names = T, recursive = T)
  
  datalist = list()
  r <- 1
  for (i in 1:length(filesA)) {
    for (j in 1:length(filesX)) {
      datalist[[r]] <- a_x_cor(filesA[i], filesX[j])
      r = r+1
    }
  }
  
  final_table <- do.call(rbind, datalist) %>% 
    arrange(-min_cor) %>% 
    write_delim(file = paste0(out_name_prefix,"_K",K,"_CorAX.txt"), 
                append = F, col_names = T)
  
  print(final_table)
}

################################################################################