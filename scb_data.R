library(pxweb)
Sys.setenv(LANG = "en_US.UTF-8")
library(ggplot2)
library(gridExtra)
library(tidyr)
library(writexl)
library(readxl)

d2 <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel")            # Interactive user interface. For automatization, use json queries.

df2 <- subset(d2$data)
View(df2)
df2$månad <- gsub("M", "-", df2$månad)                     #Transform into proper dates
df2$månad <- paste(df2$månad, "01", sep = "-")
df2$månad <- as.Date(df2$månad, format = "%Y-%m-%d")
df2
#write_xlsx(df2, "LOCAL FILE PATH")               #Saving df for next time. Local filepath.
#file <- "LOCAL FILE PATH"
df2 <- read_excel(file)
par(mfrow=c(1,1))
df2$bilar <- df2$`Nyregistrerade personbilar`
ggplot(df2, aes(x = månad, y = bilar, col = drivmedel)) +
  geom_point(size = 3) +
  labs(title = "New cars by fuel type",
       x = "Date",
       y= "New cars (by month)") + 
  #scale_color_manual(values = c("bensin" = "red", "diesel" = "blue", el = "yellow")) +
  theme_dark()
