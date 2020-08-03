describe.ts <- function(data) {
    names(data)[1] <- "datetime"
    names(data)[2] <- "data.obs"
    data <- data %>% drop_na()
    return.minutes <- table(minute(data$datetime))
    return.seconds <- table(second(data$datetime))
    data.freq <- 1440/calc.freq(data$datetime)
    repeated.values <- data %>%
        group_by(datetime) %>%
        summarize(n=n()) %>%
        filter(n>1)
    return(list(minutes=return.minutes,
                seconds=return.seconds,
                freq=data.freq,
                repeated.obs = nrow(repeated.values)))
}
