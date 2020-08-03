drift.correction <- function(dat,dat_lter,var_dat,var_lter) {
    data <- dat %>% left_join(dat_lter) %>% 
        select_("datetime",var_dat,var_lter) %>%
        mutate(diff = !!as.name(var_lter)-!!as.name(var_dat))
    data <- as.data.frame(data)
    comp.data <- data[,c(1,3,4)] %>% drop_na()

#-----------------------------------------------------------------------
#Plot to determine correction factors
good <- "N"
while(good == "N")
{
    correction.data = data.frame(x = comp.data[,1], y = comp.data[,3])
    correction.data = correction.data[complete.cases(correction.data),]
    
    #Plot the different corrections 
    #Plot linear regressions and export 
    #Red dots should match color 
    par(mfrow=c(4,1),mar=c(1.3,2.6,0,0) + 0.1,oma=c(.25,.25,.25,.25),mgp=c(2,.5,0))
    fit1 = lm(y~x,data=correction.data)
    fit1.adj = predict(fit1,newdata=data.frame(x=data$datetime))
    y.range = range(data[,2]+fit1.adj,data[,2],na.rm=TRUE)
    plot(data$datetime,data[,2]+fit1.adj,type="l",col="green",ylim=y.range,ylab=names(data)[2])
    lines(data$datetime,data[,2],col="grey")
    points(comp.data$datetime,comp.data[,2],pch=16,col="red",cex=2)
    
    fit2 = loess(y~as.numeric(x),data = correction.data,control = loess.control(surface = "direct"),span=1)
    fit2.adj= predict(fit2,newdata=as.numeric(data$datetime))
    y.range = range(data[,2]+fit2.adj,data[,2],na.rm=TRUE)
    plot(data$datetime,data[,2]+fit2.adj,type="l",col="blue",ylim=y.range)
    lines(data$datetime,data[,2],col="grey")
    points(comp.data$datetime,comp.data[,2],pch=16,col="red",cex=2)
    
    fit3.adj = mean(correction.data$y,na.rm=TRUE)
    y.range = range(data[,2]+fit3.adj,data[,2],na.rm=TRUE)
    plot(data$datetime,data[,2]+fit3.adj,type="l",col="tan",ylim=y.range)
    lines(data$datetime,data[,2],col="grey")
    points(comp.data$datetime,comp.data[,2],pch=16,col="red",cex=2)
    adj.range =range(c(fit1.adj,fit2.adj,fit3.adj,comp.data[,3]),na.rm=TRUE)
    
    plot(correction.data$x,correction.data$y,ylim=adj.range,xlim=range(dat$datetime),pch=16,col="red",cex=2)
    abline(fit1,col="green")
    lines(data$datetime,fit2.adj,col="blue")
    abline(h=fit3.adj,col="tan")
    
    #This section of code helps identify bad points in LTER data
    good = readline("Are you happy with the drift correction (Y/N): ")
    if(good=="N"){
        remove.choice = readline(prompt="Do you want to remove any points? (Y/N): ")
        if (remove.choice == "Y"){
            par(mfrow=c(1,1),mar=c(1.3,2.6,0,0) + 0.1,oma=c(.25,.25,.25,.25),mgp=c(2,.5,0))
            plot(comp.data$datetime,comp.data[,3],xlim=range(data$datetime,na.rm=TRUE))
            exclude = identifyPch(x=comp.data$datetime,y=comp.data[,3],n=5)
            comp.data= comp.data[-exclude,]
            points(comp.data$datetime,comp.data[,3],pch=16,col="red")
        }
    }
    
}
#Pick best fitting line
model.choice = as.integer(readline("Enter 1 for linear (green), 2 for loess (blue), 3 for constant  (tan), 4 (none): "))

#Create new data frame of Sequence numbers and apply correction to original DO data based on choice
if(model.choice==1) {
    drift.adj = fit1.adj
} 
if(model.choice==2) {
    drift.adj = fit2.adj
}

if(model.choice==3) {
    drift.adj = fit3.adj
}

if(model.choice==4) {
    drift.adj = 0  
}

data[,2] <- data[,2] + drift.adj
return(data[,c(1:2)])
} # end function

