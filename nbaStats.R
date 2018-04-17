install.packages("xlsx")
install.packages("plotly")
library(xlsx)
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)

stats <- read.xlsx("nbastats.xlsx", 
                sheetIndex = 1)

contracts <- read.xlsx("contracts.xlsx", 
                   sheetIndex = 1)

teams <- read.xlsx("teams.xlsx", 
                       sheetIndex = 1)

# RK (ranking) column has few 'NA':
stats$RK <- 1:100

# Use substr function combined with regexpr:
# the regexpr function returns the position in which the comma
# is found in the Player columns. Subtracting -1 to that, return
# the position BEFORE the comma is found, hence the position
# of the last character of the player's name. I use that as the
# 'stop' parameter of the substr function.
stats$names <- substr(stats$PLAYER, 1, regexpr(",", stats$PLAYER)-1)
stats$team <- substr(stats$PLAYER, regexpr(",", stats$PLAYER)+1, length(stats$PLAYER)) 

names(contracts)[names(contracts) == "Player"] <- "names"

# fixing few differences in names typing:

stats$names <- replace(stats$names, stats$names == "CJ McCollum", 
                       "C.J. McCollum") 
stats$names <- replace(stats$names, stats$names == "Larry Nance Jr.", 
                       "Larry Nance") 
stats$names <- replace(stats$names, stats$names == "Otto Porter Jr.", 
                       "Otto Porter") 
stats$names <- replace(stats$names, stats$names == "TJ Warren", 
                       "T.J. Warren") 

# # I will not use the substr until the length of the full string (length(merged$team)),
# # because I will also solve the problem of mutliple teams for one 
# # player, considering only the  first team
# merged$team1 <- substr(merged$team, 2, length(merged$team))
merged <- merge(x = stats, 
                y = contracts[ , c("names", "X2017.18", "Tm")], 
                by = "names", all.x = TRUE)


# Check if there are missing in RK:
merged[!complete.cases(merged$RK), ]


# Removing Greg Monroe who appears twice
merged <- merged[!(merged$names == "Greg Monroe" & 
               merged$X2017.18 == "$5,000,000"), ]

str(merged)


merged <- merge(x = merged, y = teams[ , c("team", "conference")],
                 by.x = "Tm",
                 by.y = "team",
                 all.x = TRUE)

# Check if there are missing values:
merged[!complete.cases(merged$conference), ]


# Changing X2017.18 var name:
names(merged)[names(merged) == "X2017.18"] <- "salary"

# and type, using the parse function in readr:
merged$salary <- parse_number(merged$salary)

# Converting PER from factor to numeric:
merged$PER <- as.numeric(levels(merged$PER)[merged$PER])

merged$salaryMln <- round(merged$salary / 1000000, 2)


plot_ly(merged, x = merged$PER, y = merged$salaryMln, 
        type = "scatter", mode = "markers", 
        size = merged$GP, alpha = 0.6,
        color = merged$conference,
        colors = c("red", "blue"),
        text = ~paste(merged$names, merged$Tm)) %>%
  layout(title = "Top 100 NBA Players per Player Efficiency Rating (PER)",
         titlefont = list(size = 16, 
                          color = "blue"),
    xaxis = list(title = "Player Efficiency Rating (PER)",
                 nticks = 10,
                 tickfont = list(size = 11),
                 titlefont = list(size = 11)),
    yaxis = list(title = "Salary (Millions of Dollar)",
                 nticks = 10,
                 tickfont = list(size = 11),
                 titlefont = list(size = 11)))
    
      
