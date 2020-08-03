trim.outliers <- function(data,width=7,sd.dev=2.5) {
    #data is two column data.frame
        #column one is datetime
        #column two is data
        #width: number of days to smooth data variance
        #sd.dev: number of standard deviations to compare value when
        #deterimining if it is an outlier on any given day
    names(data)[1] <- "datetime"
    data.name <- names(data)[2]
    names(data)[2] <- "data.obs"
    data <- data %>% mutate(doy = yday(datetime),year=year(datetime))
    sds <- data %>%
        group_by(year,doy) %>%
        summarise(sd_data = sd(data.obs,na.rm=TRUE)) %>%
        ungroup() %>%
        full_join(tibble(doy = seq(min(data$doy),max(data$doy),by=1))) %>%
        arrange(year,doy) %>%
        mutate(avg_sd = rollapply(data = sd_data,
                                  width = width,
                                  FUN = mean,
                                  align = "center",
                                  fill= NA,
                                  na.rm = T,
                                  partial = TRUE))
dat_clean = data %>%
        left_join(sds) %>%
        group_by(year,doy) %>%
        mutate(data.obs = ifelse(abs(data.obs - mean(data.obs, na.rm=T)) < 2.5*avg_sd,data.obs,NA)) %>%
        ungroup() %>%
        select(datetime,data.obs)
names(dat_clean)[2] = data.name
return(dat_clean)
}
