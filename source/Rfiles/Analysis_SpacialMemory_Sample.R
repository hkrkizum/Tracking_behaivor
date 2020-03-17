## set working dir ------------------------------------------------------
setwd("./")
source("./source/Rfiles/functions_SpacialMemory.R")
library(tidylog)

## Set log -------------------------------------------------------------------------
# today <- Sys.Date()
# logname <- str_c(as.character(today), ".log", "") %>% 
#     str_c("Log/", ., "")
# sink(logname, split = TRUE, type = c("output", "message"))

## set parameters---------------------------------------------------------
setting_file <- "./Setting/FileList/Analysis_file_setting_20190716_0718.csv"
in_dir <- "./TrackingData/"
out_dir <- "./Result/"
dir.create(out_dir, showWarnings = F)

df_In_f <- read_csv(setting_file) %>% 
    dplyr::mutate(in_f = str_c(in_dir, in_f, sep = "/")) %>% 
    dplyr::mutate(json = str_c(out_dir, out_f, sep = "/") %>% str_c(., ".json", sep = "")) %>% 
    dplyr::mutate(txt  = str_c(out_dir, out_f, sep = "/") %>% str_c(., ".csv", sep = "")) %>% 
    dplyr::mutate(png  = str_c(out_dir, out_f, sep = "/") %>% str_c(., ".png", sep = ""))

head(df_In_f)
dim(df_In_f)


for(i in 1:dim(df_In_f)[1]){
    tmp <- str_c("Analysis start", df_In_f$in_f[i], sep = ": ")
    print(tmp)
    
    pol_table <- read_csv(df_In_f$Group[i])
    
    pols_total_x <- pol_table$X
    pols_total_y <- pol_table$Y
    pols_total <- list(pols_total_x, pols_total_y)
    print(pols_total)
    
    pol.x_A <- pol_table$X[1:4]
    pol.y_A <- pol_table$Y[1:4]
    
    pol.x_B <- pol_table$X[4:7]
    pol.y_B <- pol_table$Y[4:7]
    
    pol.x_C <- pol_table$X[c(7:9,1)]
    pol.y_C <- pol_table$Y[c(7:9,1)]
    
    pols <- list(pol.x_A, pol.y_A, pol.x_B, pol.y_B, pol.x_C, pol.y_C)
    print(pols)
    
    Calib_dist <- c(pols_total_x[2],pols_total_x[3], pols_total_y[2], pols_total_y[3]) 
    
    myfunction_all(df_In_f$in_f[i], 
                   df_In_f$skip_start[i], 
                   df_In_f$skip_end[i],
                   df_In_f$txt[i],
                   df_In_f$json[i], 
                   df_In_f$png[i],
                   df_In_f$Arm_start[i],
                   df_In_f$Arm_novel[i],
                   df_In_f$Arm_other[i],
                   df_In_f$Retention[i],
                   pols,
                   pols_total,
                   Calib_dist)
}


## check ----------------------------------------------------------------
df_result_summary <- myfunction_get_df_summary(out_dir)
df_result_summary
# sink()