## set working dir ------------------------------------------------------
setwd("D:/Develop/Tracking_behaivor")
source("./source/Rfiles/functions_OFT.R")

## set parameters---------------------------------------------------------
setting_file  <- "Setting/FileList/Analysis_file_setting_20190710_OP.csv"
Polygone_file <- "Setting/Polygon/20190710_poly_XY_OP.csv"
in_dir  <- "C:/Behaivior_Analysis/Bonsai/Tracking/"
out_dir <- "C:/Behaivior_Analysis/Bonsai/Result/"
dir.create(out_dir, recursive = T, showWarnings = F)

## set polgon A to C ---------------------
pol_table <- read_csv(Polygone_file)
pols_total_x <- pol_table$X[1:4]
pols_total_y <- pol_table$Y[1:4]
pols_total <- list(pols_total_x, pols_total_y)
print(pols_total)

# pol.x_A <- pol_table$X[5:8]
# pol.y_A <- pol_table$Y[5:8]

# pols <- list(pol.x_A, pol.y_A)
# print(pols)

Calib_dist <- c(pols_total_x[1],pols_total_x[2], pols_total_y[1], pols_total_y[2]) 

## Import parameters -----------------------------------------------------
df_In_f <- read_csv(setting_file) %>% data.frame()

tmp <- str_c(in_dir, df_In_f$in_f, sep = "")
df_In_f$in_f <- tmp

tmp <- str_c(df_In_f$out_f, ".json", sep = "") 
tmp <- str_c(out_dir, tmp, sep = "")
df_In_f$json <- tmp

tmp <- str_c(df_In_f$out_f, ".csv", sep = "") 
tmp <- str_c(out_dir, tmp, sep = "")
df_In_f$csv <- tmp

tmp <- str_c(df_In_f$out_f, ".png", sep = "") 
tmp <- str_c(out_dir, tmp, sep = "")
df_In_f$png <- tmp
head(df_In_f)
dim(df_In_f)

## extution -------------------------------------------------------------
for(i in 1:dim(df_In_f)[1]){
    tmp <- str_c("Analysis start", df_In_f$in_f[i], sep = ": ")
    print(tmp)
    myfunction_all(df_In_f$in_f[i], 
                   df_In_f$skip_start[i], 
                   df_In_f$skip_end[i],
                   df_In_f$csv[i],
                   df_In_f$json[i], 
                   df_In_f$png[i],
                   # pols,
                   pols_total,
                   Calib_dist)
}

## check ----------------------------------------------------------------
list_json <- list.files(out_dir, pattern = "\\.json")
list_json <- str_c(out_dir, list_json, sep = "")
list_json

list_name           <- c()
list_csv            <- c()
list_start_time     <- c()
list_end_time       <- c()
list_frame          <- c()
list_NAframe        <- c()
list_OutlierXYpoint <- c()
list_Center_time    <- c()
list_Center_length  <- c()
list_Moving_dist    <- c()
list_ReEntry_delay  <- c()
list_Center_number  <- c()
list_Out_number     <- c()

for(i in list_json){
    tmp <- fromJSON(i)
    filename <- str_replace(i, out_dir, "")
    filename <- str_replace(filename, ".json", "")
    list_name        <- c(list_name,        filename) 
    list_csv         <- c(list_csv,         tmp$file_name)
    list_start_time     <- c(list_start_time,     tmp$`Analyze_start_time(sec)`)
    list_end_time       <- c(list_end_time,       tmp$`Analyze_end_time(sec)`)
    list_frame          <- c(list_frame,          tmp$`Analyzed frame`)
    list_NAframe        <- c(list_NAframe,        tmp$`NA frame number`)
    list_OutlierXYpoint <- c(list_OutlierXYpoint, tmp$`Outlier XY point frame number`)
    list_Center_time    <- c(list_Center_time,    tmp$`Enter_Center_time(sec)`)
    list_Center_length  <- c(list_Center_length,  tmp$`Enter_Center_length(cm)`)
    list_Moving_dist    <- c(list_Moving_dist,    tmp$`Ajusted Moving Distance(cm)`)
    list_ReEntry_delay  <- c(list_ReEntry_delay,  tmp$`Re-entry Delay(sec)`)
    list_Center_number  <- c(list_Center_number,  tmp$`Entering number of Center, Out`[1])
    list_Out_number     <- c(list_Out_number,     tmp$`Entering number of Center, Out`[2])
}

df_result_summary <- data.frame(
    "MouseID"                 = list_name,
    "filename"                = list_csv,
    "Analyze_start_time(sec)" = list_start_time,
    "Analyze_end_time(sec)"   = list_end_time,
    "Analyzed frame"          = list_frame,
    "NA frame number"         = list_NAframe,
    "OutlierXYpoint frameN"   = list_OutlierXYpoint,
    "Enter_Center_time(sec)"  = list_Center_time,
    "Enter_Center_length(cm)" = list_Center_length,
    "Moving Distance(cm)"     = list_Moving_dist,
    "Re-entry Delay(sec)"     = list_ReEntry_delay,
    "Enter_center_number"     = list_Center_number,
    "Enter_Out_number"        = list_Out_number
)


df_result_summary
out_f <- str_c(out_dir, "df_result_summary.csv", sep = "")
write.csv(df_result_summary, out_f, row.names = F, quote = F)



# -----------------------------------------------------------------
library(data.table)
library(tidyverse)
rawdata <- df_result_summary  %>% 
    dplyr::mutate(Time = if_else(str_detect(MouseID, "5min"), 5, 10)) %>% 
    dplyr::mutate(Cage = str_remove(MouseID, "_OFT.*$")) %>% 
    dplyr::mutate(No   = str_remove(MouseID, "^.*_OFT_No") %>% str_remove(., "_5min")) %>% 
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
    dplyr::mutate(Enter_Center_time_percent = `Enter_Center_time.sec.` / (Time * 60) * 100) %>% 
    dplyr::mutate(Enter_Center_len_percent  = `Enter_Center_length.cm.`/`Moving.Distance.cm.`* 100)  
colnames(rawdata)
    

# Center - time - percent --------------------------------------------
rawdata %>% 
    dplyr::filter(Time == 10) %>% 
    ggplot(., 
           aes(x=IRD,
               y=Enter_Center_time_percent,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8)) +
    ylim(0, 50)

rawdata %>% 
    dplyr::filter(Time == 5) %>% 
    ggplot(., 
           aes(x=IRD,
               y=Enter_Center_time_percent,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8))+
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8)) +
    ylim(0, 50)

# Center - length  ----------------------------------------------------
rawdata %>% 
    dplyr::filter(Time == 10) %>% 
    ggplot(., 
           aes(x=IRD,
               y=`Enter_Center_length.cm.`,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8))


rawdata %>% 
    dplyr::filter(Time == 5) %>% 
    ggplot(., 
           aes(x=IRD,
               y=`Enter_Center_length.cm.`,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8))+
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8))

# Center - length - percent -------------------------------------------
rawdata %>% 
    dplyr::filter(Time == 10) %>% 
    ggplot(., 
           aes(x=IRD,
               y=Enter_Center_len_percent,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8))


rawdata %>% 
    dplyr::filter(Time == 5) %>% 
    ggplot(., 
           aes(x=IRD,
               y=Enter_Center_len_percent,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8))+
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8))

# Center - enter number  --------------------------------------------
rawdata %>% 
    dplyr::filter(Time == 10) %>% 
    ggplot(., 
           aes(x=IRD,
               y=Enter_center_number,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8)) 

rawdata %>% 
    dplyr::filter(Time == 5) %>% 
    ggplot(., 
           aes(x=IRD,
               y=Enter_center_number,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8))+
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8)) 


rawdata %>% 
    dplyr::filter(Time == 5) %>% 
    dplyr::select(Cage, No, INT, IRD, `Enter_Center_length.cm.`)

# Re entry delay  --------------------------------------------
rawdata %>% 
    dplyr::filter(Time == 10) %>% 
    ggplot(., 
           aes(x=IRD,
               y=`Re.entry.Delay.sec.`,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8)) +
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8)) 

rawdata %>% 
    dplyr::filter(Time == 5) %>% 
    ggplot(., 
           aes(x=IRD,
               y=`Re.entry.Delay.sec.`,
               fill=INT)) +
    geom_boxplot(position=position_dodge(0.8))+
    geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(0.8)) 
