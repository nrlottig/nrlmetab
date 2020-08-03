extract.do.data <- function(data,time.step=60,lat,lon,tz) {
    names(data)[1] <- "datetime"
    data.name <- names(data)[2]
    names(data)[2] <- "data.obs"
    data <- data %>% drop_na() #%>%
    # mutate(year = year(datetime),
    #        yday = yday(datetime),
    #        hour = hour(datetime)) #%>%
    # group_by(year,yday) %>%
    # mutate(daily.avg = mean(data.obs,na.rm=TRUE)) %>%
    # ungroup()
    datetime_matrix <- data.frame(datetime = seq(from=floor_date(min(data$datetime),unit="hour"),
                                                 to=ceiling_date(max(data$datetime),unit="hour"),
                                                 by="1 min"))
    data <- datetime_matrix %>% left_join(data)
    if((time.step %% 2) == 1) time.step.roll <- time.step - 1 else time.step.roll <- time.step
    data <- data %>% mutate(data.avg = NA)
    solardata <- getSunlightTimes(date = as_date(data$datetime),
                                  lat = lat,
                                  lon = lon,
                                  tz=tz)[c(8,13)]
    solardata$sunriseEnd <- force_tz(solardata$sunriseEnd,"UTC")
    solardata$nauticalDusk <- force_tz(solardata$nauticalDusk,"UTC")
    data <- data %>% mutate(sunriseEnd = solardata$sunriseEnd+7200,nauticalDusk = solardata$nauticalDusk)
    for(i in 1:nrow(data)){
        start.at <- max(i-time.step.roll/2, 1)
        if(data$datetime[i] > data$sunriseEnd[i] & data$datetime[i] < data$nauticalDusk[i]) {
            data$data.avg[i] <- max(data$data.obs[start.at:(i+time.step.roll/2)],na.rm=TRUE)
        } else {
            data$data.avg[i] <- min(data$data.obs[start.at:(i+time.step.roll/2)],na.rm=TRUE)
        }
    }
    datetime_matrix <- data.frame(datetime = seq(from=floor_date(min(data$datetime),unit="hour"),
                                                 to=ceiling_date(max(data$datetime),unit="hour"),
                                                 by=paste0(time.step," min")))
    data <- datetime_matrix %>% left_join(data) %>% select(datetime,data.avg) %>%
        mutate(data.avg = ifelse(is.finite(data.avg)==FALSE,NA,data.avg))
    names(data)[2] <- data.name
    return(data)
}
