## set library -------------------------------------------
library(sp)
library(tidyverse)
library(jsonlite)
library(stringr)
library(Rcpp)
sourceCpp('./source/Rfiles/Cpp/myfunction_movingCpp.cpp')

## make function -----------------------------------------
myfunction_jugde <- function(x, y, Ax, Ay){
    result <- point.in.polygon(x, y, Ax, Ay)
    if (result == 0) {
        return(0)
    }else{
        return(1)
    }
}

myfunction_zone <- function(a,b,c){
    if (a == 1) {
        return("A")
    }else if(b == 1){
        return("B")
    }else if(c == 1){
        return("C")
    }else {
        return("S")
    }
}

myfunction_enter <- function(pre, post) {
    if( post - pre == 1){
        return(1)
    } else if ( post - pre == 0) { 
        return(0)
    } else if (post - pre == -1) {
        return(-1)
    } else {
        return(NA)
    }
}

myfunction_moving <- function(a){
  res <- 0
  for(i in 2:dim(a)[1]){
    b <- sqrt((a[i,1] - a[i-1,1])^2 + (a[i,2] - a[i-1,2])^2)
    res <- c(res, b[1,1])
  }
  return(res)
}

myfunction_alternation <- function(alt){
  res = 0
  for(i in 3:length(alt)){
    sample <- c(alt[i-2], alt[i-1], alt[i])
    # print(sample)
    if (length(unique(sample))==3) {
      res <- res + 1
    } 
  }
  return(res/(length(alt)-2)*100)
}

myfunction_all <- function(in_f, 
                           skip_start, 
                           skip_end,
                           txt,
                           json,
                           png,
                           Arm_start,
                           Arm_novel,
                           Arm_other,
                           Ret_flag,
                           pols,
                           pols_t,
                           Calib_dist){
    ## input file ----------------------------
    in_f_name <- in_f
    
    ## set polgon A to C ---------------------
    cutoff_senter <- 0.9
    
    
    pol.x_A <- as.vector(pols[[1]]) 
    pol.y_A <- as.vector(pols[[2]])
    
    if (pol.x_A[1] > pol.x_A[2]) {
        tmp_A_1 <- pol.x_A[2] - abs(pol.x_A[1] - pol.x_A[2]) * cutoff_senter
    }else if (pol.x_A[1] < pol.x_A[2]){
        tmp_A_1 <- pol.x_A[1] + abs(pol.x_A[1] - pol.x_A[2]) * cutoff_senter 
    }else if(pol.x_A[1] == pol.x_A[2]){
        tmp_A_1 <- pol.x_A[1]
    }

    if (pol.x_A[3] > pol.x_A[4]) {
        tmp_A_2 <- pol.x_A[3] - abs(pol.x_A[3] - pol.x_A[4]) * cutoff_senter 
    }else if (pol.x_A[3] < pol.x_A[4]){
        tmp_A_2 <- pol.x_A[3] + abs(pol.x_A[3] - pol.x_A[4]) * cutoff_senter
    }else if(pol.x_A[3] == pol.x_A[4]){
        tmp_A_2 <- pol.x_A[3]
    }

    tmp_A_3 <- ( pol.y_A[1] - pol.y_A[2] ) * cutoff_senter + pol.y_A[2]
    tmp_A_4 <- ( pol.y_A[4] - pol.y_A[3] ) * cutoff_senter + pol.y_A[3]
    
    pol.x_A[1] <- tmp_A_1
    pol.x_A[4] <- tmp_A_2
    pol.y_A[1] <- tmp_A_3
    pol.y_A[4] <- tmp_A_4
    
    pol.x_B <- as.vector(pols[[3]])
    pol.y_B <- as.vector(pols[[4]])
    
    tmp_B_1 <- pol.x_B[1] + (pol.x_B[2]-pol.x_B[1])*(1 - cutoff_senter)
    tmp_B_2 <- pol.x_B[4] + (pol.x_B[3]-pol.x_B[4])*(1 - cutoff_senter)
    tmp_B_3 <- pol.y_B[1] + (pol.y_B[2]-pol.y_B[1])*(1 - cutoff_senter)
    tmp_B_4 <- pol.y_B[4] + (pol.y_B[3]-pol.y_B[4])*(1 - cutoff_senter)
    
    pol.x_B[1] <- tmp_B_1
    pol.x_B[4] <- tmp_B_2
    pol.y_B[1] <- tmp_B_3
    pol.y_B[4] <- tmp_B_4

    pol.x_C <- as.vector(pols[[5]])
    pol.y_C <- as.vector(pols[[6]])
    
    tmp_C_1 <- pol.x_C[1] - (pol.x_C[1]-pol.x_C[2])*(1 - cutoff_senter)
    tmp_C_2 <- pol.x_C[4] - (pol.x_C[4]-pol.x_C[3])*(1 - cutoff_senter)
    tmp_C_3 <- pol.y_C[1] + (pol.y_C[2]-pol.y_C[1])*(1 - cutoff_senter)
    tmp_C_4 <- pol.y_C[4] + (pol.y_C[3]-pol.y_C[4])*(1 - cutoff_senter)
    
    pol.x_C[1] <- tmp_C_1
    pol.x_C[4] <- tmp_C_2
    pol.y_C[1] <- tmp_C_3
    pol.y_C[4] <- tmp_C_4
    
    ## skip frames --------------------------
    skip_start <- skip_start * 29.97
    skip_end <- skip_end * 29.97
    
    ## get rawdata --------------------------------------------
    rawdata <- read_csv(in_f_name,
                        col_names = FALSE,
                        col_types = cols(X1 = col_double(), X2 = col_double()),
                        skip = 3)
    colnames(rawdata) <- c("x","y") # colname をセット
    rawdata$frame <- row.names(rawdata) # rawname をセット
    rawdata$frame <- as.numeric(rawdata$frame)
    rawdata <- rawdata[skip_start:skip_end,] # skip frame
    
    # ## 先頭がNAの場合に備え、NAをオミットした際の先頭を取得
    # rawdata %>%
    #   dplyr::filter(!is.na(x)) -> tmp
    

    # NAを含むレコードを抽出
    rawdata %>%
      dplyr::filter(is.na(x)) -> tmp
    
    NAframeNum <- dim(tmp)[1]
    x_list <- c(tmp$frame[1])
    y_list <- c()
    
    # フレーム番号の前後を比較し、差が1以上なら先頭及び末尾に格納
    if (dim(tmp)[1] != 0 ) {
        if (dim(tmp)[1] == 1) {
            x_list <- c(tmp$frame)
            y_list <- c(tmp$frame)
        }else {
            for (i in 2:(length(tmp$frame)[1])){
                if (tmp$frame[i] - tmp$frame[i-1] > 1){
                    x_list <- c(x_list, tmp$frame[i])
                    y_list <- c(y_list, tmp$frame[i-1])
                } else {
                }
            }
            
            # 末尾が連続してNAの場合、強制的に最終フレームを格納する
            if (length(x_list) !=  length(y_list)) {
                y_list <- c(y_list, tmp$frame[dim(tmp)[1]])
            }
        }
      

      # NAのスタート・ストップをデータフレームに格納
      tmp3 <- data.frame("x"=x_list, "y"=y_list)
      print("NA frame list")
      print(tmp3)

      for (i in 1:dim(tmp3)[1]) {
        a <- rawdata %>%
          dplyr::filter(frame >= tmp3[i, 1]-1) %>% # NAの上下を取得
          dplyr::filter(frame <= tmp3[i, 2]+1) %>% # NAの上下を取得
          dplyr::filter(!is.na(x)) %>% # NAをオミット
          dplyr::summarise(x_ave = mean(x), y_ave = mean(y)) %>% # 上下で平均値を算出
          data.frame()
        rawdata[(rawdata$frame >= tmp3[i, 1] & rawdata$frame <= tmp3[i, 2]),]$x <- a[1,1] # NAのframeに平均値を格納
        rawdata[(rawdata$frame >= tmp3[i, 1] & rawdata$frame <= tmp3[i, 2]),]$y <- a[1,2]
      }
    }
    
    ## mutate ---------------------
    rawdata %>% 
        mutate(x_smooth = x) %>% 
        mutate(y_smooth = y) %>% 
        dplyr::select(frame, x, y, x_smooth, y_smooth)-> rawdata 
    
    # rawdata$dist <- myfunction_moving(rawdata[,2:3])
    rawdata$dist <- myfunction_movingCpp(rawdata[,2:3])
    
    # 外れ値を含むレコードを抽出
    rawdata %>% 
        dplyr::filter(dist > 100) -> tmp
    print(tmp)
    
    OutframeNum <- dim(tmp)[1]
    x_list <- c(tmp$frame[1])
    y_list <- c()
    
    # フレーム番号の前後を比較し、差が1以上なら先頭及び末尾に格納
    if (dim(tmp)[1] != 0 ) {
        if (dim(tmp)[1] == 1) {
            x_list <- c(tmp$frame)
            y_list <- c(tmp$frame)
        } else {
            for (i in 2:(length(tmp$frame)[1])){
                if (tmp$frame[i] - tmp$frame[i-1] > 1){
                    x_list <- c(x_list, tmp$frame[i])
                    y_list <- c(y_list, tmp$frame[i-1])
                } else {
                }
            }

            # 末尾に最終フレームを格納する
            if (length(x_list) !=  length(y_list)) {
                y_list <- c(y_list, tmp$frame[dim(tmp)[1]])
            }
        }

        # 外れ値のスタート・ストップをデータフレームに格納
        tmp3 <- data.frame("x"=x_list, "y"=y_list)
        print("Outlier XY point frame list")
        print(tmp3)

        for (i in 1:dim(tmp3)[1]) {
            rawdata[(rawdata$frame >= tmp3[i, 1] & rawdata$frame <= tmp3[i, 2]),]$x <- NA # 外れ値をNA化
            rawdata[(rawdata$frame >= tmp3[i, 1] & rawdata$frame <= tmp3[i, 2]),]$y <- NA # 外れ値をNA化
            a <- rawdata %>%
                dplyr::filter(frame >= tmp3[i, 1]-1) %>% # 上下を取得
                dplyr::filter(frame <= tmp3[i, 2]+1) %>% # 上下を取得 
                dplyr::filter(!is.na(x)) %>%             # NAをオミット
                dplyr::summarise(x_ave = mean(x), y_ave = mean(y)) %>% # 上下で平均値を算出
                data.frame()
            
            print(a)
            rawdata[(rawdata$frame >= tmp3[i, 1] & rawdata$frame <= tmp3[i, 2]),]$x <- a[1,1] # NAのframeに平均値を格納
            rawdata[(rawdata$frame >= tmp3[i, 1] & rawdata$frame <= tmp3[i, 2]),]$y <- a[1,2]
        }
    }
    
    # 5フレームの平均値を修正済みXY座標に格納する
    rawdata$x_smooth[2] <- mean(rawdata$x[1:3])
    rawdata$y_smooth[2] <- mean(rawdata$y[1:3])
    
    for (i in 3:(dim(rawdata)[1]-2) ) {
        slice_start <- i-2
        slice_end   <- i+2
        rawdata$x_smooth[i] <- mean(rawdata$x[slice_start:slice_end])
        rawdata$y_smooth[i] <- mean(rawdata$y[slice_start:slice_end])
    }
    
    slice_start <- dim(rawdata)[1]-3
    slice_end   <- dim(rawdata)[1]
    rawdata$x_smooth[dim(rawdata)[1]-1] <- mean(rawdata$x[slice_start:slice_end])
    rawdata$y_smooth[dim(rawdata)[1]-1] <- mean(rawdata$y[slice_start:slice_end])
    
    rawdata$x_smooth[dim(rawdata)[1]] <- mean(rawdata$x[slice_start:slice_end])
    rawdata$y_smooth[dim(rawdata)[1]] <- mean(rawdata$y[slice_start:slice_end])
    
    # rawdata$dist <- myfunction_moving(rawdata[,2:3])
    # rawdata$dist_smooth <- myfunction_moving(rawdata[,7:8])
    rawdata$dist <- myfunction_movingCpp(rawdata[,c("x", "y")])
    rawdata$dist_smooth <- myfunction_movingCpp(rawdata[,c("x_smooth", "y_smooth")])
    
    rawdata %>% 
        dplyr::filter( (dist > 100) | (dist_smooth > 50) ) -> tmp
    if (dim(tmp)[1] > 0) {
        print("XY point contain outliers")
        print(tmp)
    }
    
    print("Adjusted rawdata")
    print(head(rawdata))

    ## jugde Arm A ----------------
    pol.x <- pol.x_A
    pol.y <- pol.y_A
    
    in_list <- c()
    for(i in 1:dim(rawdata)[1]){
        zone <- myfunction_jugde(rawdata$x_smooth[i],rawdata$y_smooth[i], pol.x, pol.y)
        in_list <- c(in_list, zone)
    }
    # result_list_A <- in_list # 結果の格納
    rawdata$Arm_A <- in_list    
    
    ## jugde Arm B ---------------
    pol.x <- pol.x_B
    pol.y <- pol.y_B
    
    in_list <- c()
    for(i in 1:dim(rawdata)[1]){
        zone <- myfunction_jugde(rawdata$x_smooth[i],rawdata$y_smooth[i], pol.x, pol.y)
        in_list <- c(in_list, zone)
    }
    rawdata$Arm_B <- in_list # 結果の格納

    ## jugde Arm C ----------------
    pol.x <- pol.x_C
    pol.y <- pol.y_C
    
    in_list <- c()
    for(i in 1:dim(rawdata)[1]){
        zone <- myfunction_jugde(rawdata$x_smooth[i],rawdata$y_smooth[i], pol.x, pol.y)
        in_list <- c(in_list, zone)
    }
    rawdata$Arm_C <- in_list
    
    ## ゾーン識別カラム -----------------------------------
    in_list<- c()
    for(i in 1:dim(rawdata)[1]){
        zone <- myfunction_zone(rawdata$Arm_A[i], rawdata$Arm_B[i], rawdata$Arm_C[i])
        in_list <- c(in_list, zone)
    }
    rawdata$zone <- in_list
    
    in_f <- c(rawdata$zone[1])
    for(i in 2:length(rawdata$zone)){
        if (rawdata$zone[i] != rawdata$zone[i-1]) {
            in_f <- c(in_f, rawdata$zone[i])
        }
    }
    
    Alternation_S <- in_f
    Alternation <- in_f[-which(in_f == "S")]
    
    if (length(Alternation) == 0) {
        Alternation <- "Null"
        Alternation_table <- "Null"
    }else if(length(Alternation) == 1){
        Alternation_table <- as.vector(table(Alternation))
    }else{
        in_f <- c(Alternation[1])
        for(i in 2:length(Alternation)){
            # print(Alternation[i])
            # print(Alternation[i-1])
            if (Alternation[i] != Alternation[i-1]) {
                in_f <- c(in_f, Alternation[i])
            }
        }
        Alternation <- in_f
        Alternation_table <- as.vector(table(Alternation))
    }
    
    # print(Alternation)
    print(table(Alternation))
    
    ## Calc Arm stay time-----------------------------------------------------
    rawdata %>% 
        dplyr::filter(Arm_A == 1) %>%
        dim() -> tmp
    Enter_A_time <- tmp[1] * (1 / 29.97)
    
    rawdata %>% 
        dplyr::filter(Arm_B == 1) %>%
        dim() -> tmp
    Enter_B_time <- tmp[1] * (1 / 29.97)
    
    rawdata %>% 
        dplyr::filter(Arm_C == 1) %>%
        dim() -> tmp
    Enter_C_time <- tmp[1] * (1 / 29.97)
    
    ## Calc moving distance
    Unit_length <- 128 / sqrt((Calib_dist[1]-Calib_dist[2])^2 + (Calib_dist[3]-Calib_dist[4])^2)
    total_dist <- sum(rawdata$dist_smooth) * Unit_length
    
    # ## Calc Alternation
    alt <- myfunction_alternation(Alternation)
    
    ## Calc preference
    if (Ret_flag == TRUE) {
        ## Calc 3way %
        if (Arm_novel == "A") {
            Novel_Arm <- Enter_A_time
        } else if (Arm_novel == "B") {
            Novel_Arm <- Enter_B_time
        } else if (Arm_novel == "C") {
            Novel_Arm <- Enter_C_time
        } else{
            Starting_Arm <- 0.0000001
        }
        
        if (Arm_other == "A") {
            Other_Arm <- Enter_A_time
        } else if (Arm_other == "B") {
            Other_Arm <- Enter_B_time
        } else if (Arm_other == "C") {
            Other_Arm <- Enter_C_time
        } 
        Retention_rate <- (Novel_Arm/(Novel_Arm + Other_Arm)) * 100
        Training_rate <- "NA"
    } else{
        ## Calc 2way %
        if (Arm_start == "A") {
            Starting_Arm <- Enter_A_time
        } else if (Arm_start == "B") {
            Starting_Arm <- Enter_B_time
        } else if (Arm_start == "C") {
            Starting_Arm <- Enter_C_time
        } else{
            Starting_Arm <- 0.0000001
        } 
        
        if (Arm_other == "A") {
            Other_Arm <- Enter_A_time
        } else if (Arm_other == "B") {
            Other_Arm <- Enter_B_time
        } else if (Arm_other == "C") {
            Other_Arm <- Enter_C_time
        } else{
            Other_Arm <- 0.0000001
        } 
        Training_rate <- (Other_Arm/(Other_Arm +Starting_Arm)) * 100
        Retention_rate <- "NA"
    }
    
    ## make result.json
    x <- list(in_f_name,
              skip_start/29.97,
              skip_end/29.97,
              dim(rawdata)[1],
              NAframeNum,
              OutframeNum,
              Enter_A_time, 
              Enter_B_time, 
              Enter_C_time,
              total_dist,
              Arm_start,
              Arm_novel,
              Arm_other,
              Training_rate,
              Retention_rate,
              # Alternation_S,
              Alternation,
              Alternation_table,
              alt)
    y <- c("file_name",
           "Analyze_start_time(sec)",
           "Analyze_end_time(sec)",
           "Analyzed frame",
           "NA frame number",
           "Outlier XY point frame number",
           "Enter_A_time(sec)",
           "Enter_B_time(sec)",
           "Enter_C_time(sec)",
           "Moving Distance(mm)",
           "Strat arm",
           "Novel arm",
           "Other arm",
           "Training(Other/(Other + Start))",
           "Retention(Novel/(Novel + Other))",
           # "Alternation_with_center",
           "Alternation",
           "Entering number of A, B, C",
           "Spontaneous alternation %")
    
    result <- rbind(y,x)
    result
    colnames(result) <- result[1,]
    result <- result[-1, ]
    tmp <- toJSON(result, pretty = TRUE, auto_unbox = TRUE)
    write(tmp, file = json)
    
    # write.table(rawdata, txt, sep = "\t",row.names = F)
    write.csv(rawdata, txt, row.names = F, quote = F)

    #moving line
    
    ## Y-maze
    polygon <- data.frame("x" = pols_t[[1]], "y" = pols_t[[2]])
    polygon$y <- polygon$y * -1
    
    ## Zone
    polygon_A <- data.frame("x" = pol.x_A, "y" = pol.y_A * -1)
    polygon_B <- data.frame("x" = pol.x_B, "y" = pol.y_B * -1)
    polygon_C <- data.frame("x" = pol.x_C, "y" = pol.y_C * -1)
     
    
    rawdata %>% 
        dplyr::select(x_smooth,y_smooth) %>% 
        dplyr::mutate(x = x_smooth) %>% 
        dplyr::mutate(y = y_smooth) %>%
        dplyr::mutate(y = y * -1) -> tmp
    g <- ggplot(tmp, aes(x,y)) + 
      geom_path() +
      xlim(c(0, 1920)) +
      ylim(c(-1080,0)) +
      theme_void()+ 
      theme(panel.background = element_rect(fill = "White", colour = "Black")) +
      geom_polygon(data = polygon, aes(x,y), color = "blue", fill = NA, size = 2) +
      geom_polygon(data = polygon_A, aes(x,y), color = "Red", fill = NA, size = 2) +
      geom_polygon(data = polygon_B, aes(x,y), color = "Red", fill = NA, size = 2) +
      geom_polygon(data = polygon_C, aes(x,y), color = "Red", fill = NA, size = 2)
    ggsave(filename = png, plot = g, dpi = 100, width = 19.2, height = 10.8)
    plot(g)
    
    
}

myfunction_get_df_summary <- function(out_dir){
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
    list_Enter_A_time   <- c()  
    list_Enter_B_time   <- c() 
    list_Enter_C_time   <- c()
    list_total_dist     <- c()
    list_Arm_start      <- c()
    list_Arm_novel      <- c()
    list_Arm_other      <- c()
    list_Training_rate  <- c()
    list_Retention_rate <- c()
    list_Alternation        <- c()
    list_Alternation_table  <- c()
    list_alt                <- c()
    
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
        list_Enter_A_time   <- c(list_Enter_A_time,   tmp$`Enter_A_time(sec)`)  
        list_Enter_B_time   <- c(list_Enter_B_time,   tmp$`Enter_B_time(sec)`) 
        list_Enter_C_time   <- c(list_Enter_C_time,   tmp$`Enter_C_time(sec)`)
        list_total_dist     <- c(list_total_dist,     tmp$`Moving Distance(mm)`)
        list_Arm_start      <- c(list_Arm_start,      tmp$`Strat arm`)
        list_Arm_novel      <- c(list_Arm_novel,      tmp$`Novel arm`)
        list_Arm_other      <- c(list_Arm_other,      tmp$`Other arm`)
        list_Training_rate  <- c(list_Training_rate,  tmp$`Training(Other/(Other + Start))`)
        list_Retention_rate <- c(list_Retention_rate, tmp$`Retention(Novel/(Novel + Other))`)
        list_Alternation        <- c(list_Alternation,       
                                     paste(tmp$Alternation, collapse = "_"))
        list_Alternation_table  <- c(list_Alternation_table, 
                                     paste(tmp$`Entering number of A, B, C`, collapse = "_") )
        list_alt                <- c(list_alt,               tmp$`Spontaneous alternation %`)
    }
    
    df_result_summary <- data.frame(
        "MouseID"                 = list_name,
        "filename"                = list_csv,
        "Analyze_start_time(sec)" = list_start_time,
        "Analyze_end_time(sec)"   = list_end_time,
        "Analyzed frame"          = list_frame,
        "NA frame number"         = list_NAframe,
        "OutlierXYpoint frameN"   = list_OutlierXYpoint,
        "Enter_A_time(sec)"       = list_Enter_A_time,
        "Enter_B_time(sec)"       = list_Enter_B_time,
        "Enter_C_time(sec)"       = list_Enter_C_time,
        "Moving Distance(mm)"     = list_total_dist,
        "Strat arm"               = list_Arm_start,
        "Novel arm"               = list_Arm_novel,
        "Other arm"               = list_Arm_other,
        "Training_OtherPerOtherPlusStart"   = list_Training_rate,
        "Retention_NovelPerNovelPlusOther"  = list_Retention_rate,
        "Alternation"                       = list_Alternation,
        "Entering number of A, B, C"        = list_Alternation_table,
        "Spontaneous alternation percent"   = list_alt
    )
    
    out_f <- str_c(out_dir, "df_result_summary.csv", sep = "")
    write.csv(df_result_summary, out_f, row.names = F, quote = F)
    
    return(df_result_summary)
}