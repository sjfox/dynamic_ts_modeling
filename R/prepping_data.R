library(cdcfluview)
library(tidyverse)


ili <- get_flu_data(region = "national", data_source = "ilinet", years = 1997:2017)

flu_ts <- as.numeric(ili$`% WEIGHTED ILI`) 

flu_ts[35:53] <- approx(c(34,54), c(flu_ts[34], flu_ts[54]), xout = 35:53)$y
flu_ts[87:105] <- approx(c(86,106), c(flu_ts[86], flu_ts[106]), xout = 87:105)$y
flu_ts[139:157] <- approx(c(138,158), c(flu_ts[138], flu_ts[158]), xout = 139:157)$y
flu_ts[191:209] <- approx(c(190,210), c(flu_ts[190], flu_ts[210]), xout = 191:209)$y
flu_ts[243:261] <- approx(c(242,262), c(flu_ts[242], flu_ts[262]), xout = 243:261)$y

plot(flu_ts, type ="l")

write_csv(x = data_frame(ili = flu_ts), "data/flu_ili.csv")

Iquitos<-read.csv('/Users/aliciakraay/Dropbox/TiseanData/Iquitos_Testing_Data.csv')
SanJuan<-read.csv('/Users/aliciakraay/Dropbox/TiseanData/San_Juan_Testing_Data.csv')

dengue_ts<-SanJuan$total_cases
plot(dengue_ts, type='l')

write.csv(dengue_ts, '/Users/aliciakraay/Dropbox/TiseanData/DengueTS.csv')

