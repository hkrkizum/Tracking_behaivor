## set library -------------------------------------------
library(sp)
library(tidyverse)
library(jsonlite)
library(Rcpp)
sourceCpp('Cpp/myfunction_movingCpp.cpp')


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
                           pols_t,
                           Calib_dist){
    ## input file --------------------------------------------------------------
    in_f_name <- in_f
    
    ## Calc Center Area --------------------------------------------------------
    pol.x_A <- as.vector(pols_t[[1]])
    pol.y_A <- as.vector(pols_t[[2]])

    tmp_1 <- ((pol.x_A[2] - pol.x_A[1]) * 0.4 / 2)
    pol.x_A[1] <- pol.x_A[1] + tmp_1
    pol.x_A[2] <- pol.x_A[2] - tmp_1
    
    tmp_1 <- ((pol.x_A[3] - pol.x_A[4]) * 0.4 / 2)
    pol.x_A[4] <- pol.x_A[4] + tmp_1
    pol.x_A[3] <- pol.x_A[3] - tmp_1
    
    tmp_2 <- ((pol.y_A[2] - pol.y_A[3]) * 0.4 / 2)
    pol.y_A[2] <- pol.y_A[2] + tmp_1
    pol.y_A[3] <- pol.y_A[3] - tmp_1

    tmp_2 <- ((pol.y_A[1] - pol.y_A[4]) * 0.4 / 2)
    pol.y_A[1] <- pol.y_A[1] + tmp_1
    pol.y_A[4] <- pol.y_A[4] - tmp_1
    
    ## skip frames -----------------------------------------------------------
    skip_start <- skip_start * 29.97
    skip_end <- skip_end * 29.97
    
    ## get rawdata -----------------------------------------------------------
    rawdata <- read_csv(in_f_name,
                        col_names = FALSE,
                        col_types = cols(X1 = col_double(), X2 = col_double()),
                        skip = 3)
    colnames(rawdata) <- c("x","y") # colname をセット
    rawdata$frame <- row.names(rawdata) # rawname をセット
    rawdata$frame <- as.numeric(rawdata$frame)
    rawdata <- rawdata[skip_start:skip_end,] # skip frame
    
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
        } else {
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
            a <- rawdata %>%
                dplyr::filter((frame = tmp3[i, 1]-1) | (frame = tmp3[i, 2]+1)) %>% # NAの上下を取得
                dplyr::summarise(x_ave = mean(x), y_ave = mean(y)) %>% # 上下で平均値を算出
                data.frame()
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
    # rawdata$dist_smooth <- myfunction_moving(rawdata[,5:6])
    
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
    result_list_A <- in_list # 結果の格納
    rawdata$Arm_A <- result_list_A
    
    ## Entry number -------------------------------------------------------------
    rawdata <- rawdata %>% 
      dplyr::mutate(zone = if_else(Arm_A == 1, "Center", "Out"))
    
    in_f <- c(rawdata$zone[1])
    for(i in 2:length(rawdata$zone)){
      if (rawdata$zone[i] != rawdata$zone[i-1]) {
        in_f <- c(in_f, rawdata$zone[i])
      }
    }
    
    Alternation <- in_f
    Alternation_table <- as.vector(table(Alternation)) 
    # print(Alternation)
    print(table(Alternation))
    
    ## Re-entry delay -----------------------------------------------------------
    tmp1 <- rawdata %>% 
      tibble::rowid_to_column() %>% 
      dplyr::mutate(ChangeAria = lead(Arm_A, n = 1, default = NA)) %>% 
      dplyr::mutate(flag_ChAra = Arm_A - ChangeAria) %>% 
      dplyr::filter(flag_ChAra == -1)
    print(tmp1)
    Entry_delay <- tmp1$rowid[1]  * (1 / 29.97)
    print("********* Entry Delay *************")
    print(Entry_delay)
    
    
    ## Calc Cenrer stay time -----------------------------------------------------
    rawdata %>% 
        dplyr::filter(Arm_A == 1) %>%
        dim() -> tmp
    Enter_A_time <- tmp[1] * (1 / 29.97)
    
    ## Calc moving distance ------------------------------------------------------
    Unit_length <- 50 / sqrt((Calib_dist[1]-Calib_dist[2])^2 + (Calib_dist[3]-Calib_dist[4])^2)
    total_dist <- sum(rawdata$dist) * Unit_length
    total_dist_smooth <- sum(rawdata$dist_smooth) * Unit_length
    

    ## Calc Center stay length ----------------------------------------------------
    rawdata %>% 
        dplyr::filter(Arm_A == 1) -> tmp
    Enter_A_length <- sum(tmp$dist_smooth) * Unit_length
    
    ## make result.json ----------------------------------------------------------
    x <- list(in_f_name,
              skip_start/29.97,
              skip_end/29.97,
              dim(rawdata)[1],
              NAframeNum,
              OutframeNum,
              Enter_A_time, 
              Enter_A_length,
              total_dist,
              total_dist_smooth,
              Entry_delay,
              Alternation_table)
    
    y <- c("file_name",
           "Analyze_start_time(sec)",
           "Analyze_end_time(sec)",
           "Analyzed frame",
           "NA frame number",
           "Outlier XY point frame number",
           "Enter_Center_time(sec)",
           "Enter_Center_length(cm)",
           "Moving Distance(cm)",
           "Ajusted Moving Distance(cm)",
           "Re-entry Delay(sec)",
           "Entering number of Center, Out")
    
    result <- rbind(y,x)
    result
    colnames(result) <- result[1,]
    result <- result[-1, ]
    tmp <- toJSON(result, pretty = TRUE, auto_unbox = TRUE)
    write(tmp, file = json)
    print(tmp)
    
    # write.table(rawdata, txt, sep = "\t",row.names = F, quote = F)
    write.csv(rawdata, txt, row.names = F, quote = F)
    
    #moving line
    polygon_A <- data.frame("x" = pol.x_A, "y" = pol.y_A)
    polygon_A$y <- polygon_A$y * -1
    
    polygon_box <- data.frame("x" = pols_t[[1]], "y" = pols_t[[2]])
    polygon_box$y <- polygon_box$y * -1
    
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
      geom_polygon(data = polygon_A, aes(x,y), color = "blue", fill = NA, size = 2) +
      geom_polygon(data = polygon_box, aes(x,y), color = "Red", fill = NA, size = 2)
    ggsave(filename = png, plot = g, dpi = 100, width = 19.2, height = 10.8)
    plot(g)
}