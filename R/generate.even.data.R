generate.even.data <- function(data,time.step=10){
    #need to write code to ensure the time step is less than the frequency
    #of data

    names(data)[1] <- "datetime"
    data.name <- names(data)[2]
    names(data)[2] <- "data.obs"
    data <- data %>% drop_na()
    datetime = seq(from=floor_date(min(data$datetime),unit="hour"),
                                                 to=ceiling_date(max(data$datetime),unit="hour"),
                                                 by="1 min")
    datetime_matrix <- data.frame(datetime = datetime[-1])
    data <- datetime_matrix %>% left_join(data) %>%
        mutate(data.even = zoo::rollapply(data.obs,
                                          width=time.step,
                                          partial=TRUE,
                                          fill=NA,
                                          align="right",
                                          FUN=function(x) mean(x,na.rm=T)))
    datetime_matrix <- data.frame(datetime = seq(from=floor_date(min(data$datetime),unit="hour"),
                   to=ceiling_date(max(data$datetime),unit="hour"),
                   by=paste0(time.step," min")))
    data <- datetime_matrix %>% left_join(data) %>%
        select(datetime,data.even)
    names(data)[2] <- data.name
    return(data)
}
