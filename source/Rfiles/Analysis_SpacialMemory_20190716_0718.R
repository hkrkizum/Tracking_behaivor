## set working dir ------------------------------------------------------
setwd("D:/Develop/Tracking_behaivor")
source("./source/Rfiles/functions_SpacialMemory.R")
library(tidylog)

## Set log -------------------------------------------------------------------------
# today <- Sys.Date()
# logname <- str_c(as.character(today), ".log", "") %>% 
#     str_c("Log/", ., "")
# sink(logname, split = TRUE, type = c("output", "message"))

## Group 1 ----------------------------------------------------------------------

#################################################
#                                               #
#  Group 1                                      #
#                                               #
#################################################

## set parameters---------------------------------------------------------
setting_file <- "./Setting/FileList/Analysis_file_setting_20190716_0718.csv"
in_dir <- "./TrackingData/"
out_dir <- "C:/Behaivior_Analysis/Bonsai/Result/"

df_In_f <- read_csv(setting_file)

for(param_Group in unique(df_In_f$Group)){
    if (param_Group == 16) {
        pol_table <- read_csv("Setting/Polygon/20190716_poly_XY.csv")
    } else if (param_Group == 17) {
        pol_table <- read_csv("Setting/Polygon/20190716_poly_XY_2.csv")
    } else if (param_Group == 18) {
        pol_table <- read_csv("Setting/Polygon/20190718_poly_XY.csv")
    } else if (param_Group == 19) {
        pol_table <- read_csv("Setting/Polygon/20190718_poly_XY_2.csv")
    }
    
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
    
    df_In_f <- read_csv(setting_file) %>%
        dplyr::filter(Group == param_Group) %>% 
        data.frame()
    tmp <- str_c(in_dir, df_In_f$in_f, sep = "")
    df_In_f <- df_In_f %>% 
        dplyr::mutate(in_f = tmp)
    
    tmp <- str_c(df_In_f$out_f, ".json", sep = "") 
    tmp <- str_c(out_dir, tmp, sep = "")
    df_In_f <- df_In_f %>% 
        dplyr::mutate(json = tmp)

    tmp <- str_c(df_In_f$out_f, ".csv", sep = "") 
    tmp <- str_c(out_dir, tmp, sep = "")
    df_In_f <- df_In_f %>% 
        dplyr::mutate(txt = tmp)
    
    tmp <- str_c(df_In_f$out_f, ".png", sep = "") 
    tmp <- str_c(out_dir, tmp, sep = "")
    df_In_f <- df_In_f %>% 
        dplyr::mutate(png = tmp)
    
    head(df_In_f)
    dim(df_In_f)
    
    for(i in 1:dim(df_In_f)[1]){
        tmp <- str_c("Analysis start", df_In_f$in_f[i], sep = ": ")
        print(tmp)
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
    
}

## check ----------------------------------------------------------------
df_result_summary <- myfunction_get_df_summary(out_dir)
df_result_summary
sink()

# -----------------------------------------------------------------------
str(df_result_summary)
rawdata <- df_result_summary  %>% 
    dplyr::mutate(Time = if_else(str_detect(MouseID, "5min"), 5, 10)) %>% 
    dplyr::mutate(Cage = str_remove(MouseID, "_Y.*$")) %>% 
    dplyr::mutate(No   = str_remove(MouseID, "^.*_No") %>% str_remove(., "_5min")) %>% 
    dplyr::mutate(INT  = case_when(str_detect(Cage, "0216") ~ "SED",
                                   str_detect(Cage, "0217") ~ "EXE",
                                   str_detect(Cage, "0218") ~ "EXE",
                                   str_detect(Cage, "0219") ~ "SED")) %>% 
    dplyr::mutate(IRD  = case_when(str_detect(Cage, "0216") ~ "A",
                                   str_detect(Cage, "0217") ~ "A",
                                   str_detect(Cage, "0218") ~ "B",
                                   str_detect(Cage, "0219") ~ "B")) %>% 
    dplyr::mutate(INT = factor(INT, levels = c("SED", "EXE"))) %>% 
    dplyr::mutate(IRD = factor(IRD, levels = c("A", "B"))) %>% 
    dplyr::arrange(INT, IRD) %>% 
    dplyr::mutate_at(.vars = vars("Training_OtherPerOtherPlusStart",
                                  "Retention_NovelPerNovelPlusOther"), as.character) %>% 
    dplyr::mutate_at(.vars = vars("Training_OtherPerOtherPlusStart",
                                  "Retention_NovelPerNovelPlusOther"), as.numeric)  
    
# test_1 <- c(1, "NA", 2)
# test_1 <- df_result_summary$Retention_NovelPerNovelPlusOther
# test_1
# test_2 <- as.character(test_1) %>% as.numeric(.)
# test_2
# 
# colnames(rawdata)
# rawdata$Retention_NovelPerNovelPlusOther %>%
#     as.character() %>% 
#     as.numeric()

# Retention ---------------------------------------------------------
rawdata %>% 
    dplyr::filter(Time == 10) %>% 
    dplyr::filter(Retention_NovelPerNovelPlusOther != "NA") %>%
    ggplot(., 
           aes(x=IRD,
               y=Retention_NovelPerNovelPlusOther,
               fill=INT)) +
    # geom_boxplot()
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8))

rawdata %>% 
    dplyr::filter(Time == 5) %>% 
    dplyr::filter(Retention_NovelPerNovelPlusOther != "NA") %>% 
    ggplot(., 
           aes(x=IRD,
               y=Retention_NovelPerNovelPlusOther,
               fill=INT)) +
    # geom_boxplot()
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8))

# Training_OtherPerOtherPlusStart ---------------------------------------------------------
rawdata %>% 
    dplyr::filter(Time == 10) %>% 
    dplyr::filter(Training_OtherPerOtherPlusStart != "NA") %>%
    ggplot(., 
           aes(x=IRD,
               y=Training_OtherPerOtherPlusStart,
               fill=INT)) +
    # geom_boxplot()
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8))

rawdata %>% 
    dplyr::filter(Time == 5) %>% 
    dplyr::filter(Training_OtherPerOtherPlusStart != "NA") %>%
    ggplot(., 
           aes(x=IRD,
               y=Training_OtherPerOtherPlusStart,
               fill=INT)) +
    # geom_boxplot()
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8))


rawdata %>% 
    dplyr::filter(Novel.arm == "A") %>% 
    dplyr::filter(Retention_NovelPerNovelPlusOther != "NA") %>% 
    dplyr::select(MouseID, Training_OtherPerOtherPlusStart, Retention_NovelPerNovelPlusOther)
    
