## set library -------------------------------------------
library(sp)
library(tidyverse)
library(jsonlite)
library(stringr)


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
                           pols,
                           pols_t,
                           Calib_dist){
    ## input file ----------------------------
    in_f_name <- in_f
    
    ## set polgon A to C ---------------------
    pol.x_A <- as.vector(pols[[1]]) 
    pol.y_A <- as.vector(pols[[2]])
    
    pol.x_B <- as.vector(pols[[3]])
    pol.y_B <- as.vector(pols[[4]])
    
    pol.x_C <- as.vector(pols[[5]])
    pol.y_C <- as.vector(pols[[6]])
    
    ## skip frames --------------------------
    skip_start <- skip_start * 29.97
    skip_end <- skip_end * 29.97
    
    ## get rawdata --------------------------------------------
    rawdata <- read_csv(in_f_name,
                        col_names = FALSE)
    colnames(rawdata) <- c("x","y")
    rawdata$frame <- rownames(rawdata)
    rawdata <- rawdata[skip_start:skip_end,]
    rawdata <- rawdata %>% na.omit(.$x) %>% na.omit(.$y) 
    print(head(rawdata))
    rawdata <- rawdata %>%
        dplyr::filter(x != -1) %>%
        dplyr::filter(y != -1)
    
    # print(head(rawdata))
    ## jugde Arm A ----------------
    pol.x <- pol.x_A
    pol.y <- pol.y_A
    
    in_list <- c()
    for(i in 1:dim(rawdata)[1]){
        zone <- myfunction_jugde(rawdata$x[i],rawdata$y[i], pol.x, pol.y)
        in_list <- c(in_list, zone)
    }
    result_list_A <- in_list # 結果の格納
    
    ## jugde Arm B ---------------
    pol.x <- pol.x_B
    pol.y <- pol.y_B
    
    in_list <- c()
    for(i in 1:dim(rawdata)[1]){
        zone <- myfunction_jugde(rawdata$x[i],rawdata$y[i], pol.x, pol.y)
        in_list <- c(in_list, zone)
    }
    result_list_B <- in_list # 結果の格納
    ## jugde Arm C ----------------
    pol.x <- pol.x_C
    pol.y <- pol.y_C
    
    in_list <- c()
    for(i in 1:dim(rawdata)[1]){
        zone <- myfunction_jugde(rawdata$x[i],rawdata$y[i], pol.x, pol.y)
        in_list <- c(in_list, zone)
    }
    result_list_C <- in_list
    ## mutate ---------------------
    rawdata %>% 
        mutate(Arm_A = result_list_A) %>% 
        mutate(Arm_B = result_list_B) %>% 
        mutate(Arm_C = result_list_C) %>% 
        dplyr::select(frame, x, y, Arm_A, Arm_B, Arm_C)-> rawdata 
    
    rawdata$dist <- myfunction_moving(rawdata[,2:3])
    
    print(head(rawdata))
    
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
    
    in_f <- c(Alternation[1])
    for(i in 2:length(Alternation)){
        if (Alternation[i] != Alternation[i-1]) {
            in_f <- c(in_f, Alternation[i])
        }
    }
    Alternation <- in_f
    Alternation_table <- as.vector(table(Alternation)) 
    print(Alternation)
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
    total_dist <- sum(rawdata$dist) * Unit_length
    
    ## Calc Alternation
    alt <- myfunction_alternation(Alternation)
    
    ## make result.json
    x <- list(in_f_name,
              skip_start/29.97,
              skip_end/29.97,
              Enter_A_time, 
              Enter_B_time, 
              Enter_C_time,
              total_dist,
              Alternation_S,
              Alternation,
              Alternation_table,
              alt)
    y <- c("file_name",
           "Analyze_start_time(sec)",
           "Analyze_end_time(sec)",
           "Enter_A_time(sec)",
           "Enter_B_time(sec)",
           "Enter_C_time(sec)",
           "Moving Distance(mm)",
           "Alternation_with_center",
           "Alternation",
           "Entering number of A, B, C",
           "Spontaneous alternation %")
    
    result <- rbind(y,x)
    result
    colnames(result) <- result[1,]
    result <- result[-1, ]
    tmp <- toJSON(result, pretty = TRUE, auto_unbox = TRUE)
    write(tmp, file = json)
    
    write.table(rawdata, txt, sep = "\t",row.names = F)
    
    #moving line
    polygon <- data.frame("x" = pols_t[[1]], "y" = pols_t[[2]])
    polygon$y <- polygon$y * -1
    
    rawdata %>% 
      dplyr::select(x,y) %>% 
      dplyr::mutate(y = y * -1) -> tmp
    g <- ggplot(tmp, aes(x,y)) + 
      geom_path() +
      xlim(c(0, 1920)) +
      ylim(c(-1080,0)) +
      theme_void()+ 
      theme(panel.background = element_rect(fill = "White", colour = "Black")) +
      geom_polygon(data = polygon, aes(x,y), color = "blue", fill = NA, size = 2)
    ggsave(filename = png, plot = g, dpi = 100, width = 19.2, height = 10.8)
    plot(g)
    
}