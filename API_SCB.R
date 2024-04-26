install.packages("pxweb")
install.packages("writexl")

library(pxweb)
library(writexl)

# Interact with SCB API:
d <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel")

View(d[["data"]])

my_download <- d[["data"]]

#Download data locally and use later.
write_xlsx(my_download, "C:/Users/Filip/Desktop/DS23/R/Kunskapskontroll/SCB_bilar.xlsx")
