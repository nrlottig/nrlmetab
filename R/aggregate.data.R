aggregate.data <- function(data,time.step=60) {
    names(data)[1] <- "datetime"
    data.name <- names(data)[2]
    names(data)[2] <- "data.obs"
    data <- data %>% drop_na()
    datetime_matrix <- data.frame(datetime = seq(from=floor_date(min(data$datetime),unit="hour"),
                   to=ceiling_date(max(data$datetime),unit="hour"),
                   by="1 min"))
    data <- datetime_matrix %>% left_join(data)
    if((time.step %% 2) == 0) time.step <- time.step + 1
    data <- data %>% mutate(data.avg = zoo::rollapply(data.obs,
                                      width=time.step,
                                      partial=TRUE,
                                      fill=NA,
                                      align="center",
                                      FUN=function(x) mean(x,na.rm=T)))
    datetime_matrix <- data.frame(datetime = seq(from=floor_date(min(data$datetime),unit="hour"),
                                                 to=ceiling_date(max(data$datetime),unit="hour"),
                                                 by=paste0(time.step," min")))
    data <- datetime_matrix %>% left_join(data) %>% select(datetime,data.avg)
    names(data)[2] <- data.name
    return(data)
}
