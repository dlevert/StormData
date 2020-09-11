get.data <- function(){
# FUNCTION TO IMPORT DATA AND FORMAT CERTAIN VARIABLES=========================
        require(lubridate)
        wd <- read.csv("repdata-data-StormData.csv.bz2")
        wd$EVTYPE <- trimws(wd$EVTYPE)
#FORMAT DATE
        wd$BGN_DATE <- sub(" .*", "", wd$BGN_DATE)
        wd$BGN_DATE <- mdy(wd$BGN_DATE)
#FORMAT TIME
        d <- paste(wd$BGN_DATE,wd$BGN_TIME, wd$TIME_ZONE)
        d <- parse_date_time(d,"%Y-%m-%d %H:%M:%S", truncated = TRUE)
        wd$BGN_TIME <- d
#FORMAT FACTORS
        wd$COUNTYNAME <- as.factor(wd$COUNTYNAME)
        wd$STATE <- as.factor(wd$STATE)
        wd$EVTYPE <- as.factor(wd$EVTYPE)
        wd <<- wd
}

ana <- function(){
        require(dplyr)
        require(lattice)
# SUBSET DATA FOR PLOT
        sub <- wd %>% group_by(EVTYPE) %>%
                summarise(fatal = sum(FATALITIES),
                          inj = sum(INJURIES),
                          totinj = sum(FATALITIES) + sum(INJURIES))
        sub <- filter(sub, fatal > 1000 | inj > 1000)
        sub <<- arrange(sub, desc(fatal), desc(inj))
# OBTAIN STATS
        a <- max(sub$totinj)
        a <- subset(sub, sub$totinj == a)
        a <- paste("The most harmful event type is", a$EVTYPE , " with", 
                   a$fatal , "fatalities and,", a$inj, "injuries")
# PLOT RESULTS
        xyplot(sub$EVTYPE ~ sub$totinj, sub, 
               col = c("blue"), pch = 19, cex = 1.5, grid = TRUE,
               main = "Total Fatalities & Injuries by Event Type",
               xlab = "Total injuries & deaths",
               ylab = "Event Type")
        print(a)
}

ana2 <- function(){
# SUBSET DATA FOR PLOT
        require(dplyr)
        require(lattice)
        sub2 <- wd %>% group_by(EVTYPE) %>%
                summarise(pd = sum(PROPDMG), cd = sum(CROPDMG),
                          td = sum(PROPDMG)+sum(CROPDMG))
        sub2 <- filter(sub2,td > 250000)
# OBTAIN STATS
        b <- max(sub2$td)
        b <- subset(sub2, sub2$td == b)
        b <- paste("The most economic impactful weather event is ", b$EVTYPE,
                          " with a property damage of $", 
                   format(b$pd, nsmall = 0, big.mark = ","),
                   " a crop damage of $", 
                   format(b$cd, nsmall = 0, big.mark = ","),
                   " and a total of $",
                   format(b$td, nsmall = 0, big.mark = ","), sep = "")
# PLOT RESULTS
        xyplot(sub2$EVTYPE ~ sub2$td/1000000, sub2, 
               grid = TRUE, pch = 19, cex = 1.5, col = "red",
               main = "Property and Crop Damage vs. Event Type",
               xlab = "Damage Value $M",
               ylab = "Event Type")
}


