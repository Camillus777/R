library(tidyverse)

data = read_csv2("PriceTest.csv", col_types = cols(.default = "c"))
data = data[1:30,]
data$APR = gsub(",",".",data$APR)
data$APR = gsub("%","",data$APR)
data$APR = as.numeric(data$APR)/100
data$ticket_size = as.numeric(gsub(" ","",data$ticket_size))
data$`APR*ticket_size` = as.numeric(gsub(" ","",data$`APR*ticket_size`))
data$lt_iNI = as.numeric(gsub(" ","",data$lt_iNI))

str(data)

#data pivot summary
data %>% 
  group_by(test_grp_flg) %>% 
  summarize(
    count=n(), 
    avg_apr = mean(APR), 
    sd_apr = sd(APR),
    wavg_apr = weighted.mean(APR,ticket_size),
    ticket=mean(ticket_size), 
    mean(`APR*ticket_size`), 
    mean(lt_iNI)
      )

ggplot(data, aes(x=APR)) + geom_histogram() + facet_wrap(~test_grp_flg)#geom_histogram(bins=10)
ggplot(data, aes(x=APR,y=test_grp_flg)) + geom_point()
