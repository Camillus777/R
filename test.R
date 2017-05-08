library(tidyverse)
library(tabplot)
library(GGally)

# DATA -------------------------------------------------
#data = read_csv2("PriceTest.csv", col_types = cols(.default = "c"))
data = read_csv2("PriceTest.csv")

#data = data[1:30,]
#data$APR = gsub(",",".",data$APR)
#data$APR = gsub("%","",data$APR)
#data$APR = as.numeric(data$APR)/100
#data$ticket_size = as.numeric(gsub(" ","",data$ticket_size))
#data$`APR*ticket_size` = as.numeric(gsub(" ","",data$`APR*ticket_size`))
#data$lt_iNI = as.numeric(gsub(" ","",data$lt_iNI))
data$test_grp_flg = as.factor(data$test_grp_flg)
data$vps_flg = as.factor(data$vps_flg)

# EXPLORE -------------------------------------------------------
str(data)
glimpse(data)
head(data)

summary(data)
cor(as.data.frame(select(data, apr, ticket_size, lt_iNI)))
ggscatmat(as.data.frame(data))
ggpairs(select(data,-application_id, -rnd_apr), mapping = ggplot2::aes(color =test_grp_flg, alpha = 0.8))
ggduo(test, 1,2:3, mapping = ggplot2::aes(color =test_grp_flg, alpha = 0.8))    
tableplot(data,sortCol = APR)
#tableplot(data, select = c(APR, ticket_size),sortCol = APR)

# ANALYSIS ----------------------------------------
#summary pivot with difference row
data %>% 
  group_by(test_grp_flg) %>% 
  summarize(
    count=n(), 
    avg_apr = mean(apr), 
    sd_apr = sd(apr),
    wavg_apr = weighted.mean(apr,ticket_size),
    ticket=mean(ticket_size), 
    mean(APRxTicket), 
    mean(lt_iNI)
  )  %>% 
  # adding difference row
  bind_rows(summarise_each(., 'diff', -test_grp_flg))


# old school hypothesis testing
t.test(apr ~ test_grp_flg, data=data)
t.test(APRxTicket ~ test_grp_flg, data=data)
t.test(ticket_size ~ test_grp_flg, data=data)

sd(data$apr)/sqrt(sum(!is.na(data$apr))) + sd(data$apr)/sqrt(sum(!is.na(data$apr)))

# permutation test - zatim rucni ale pak predelat na permutaci celeho data frame 
test_group = sample(data$test_grp_flg, replace = TRUE)
#new_test = data$test_grp_flg
mean(data$apr[test_group==1]) - mean(data$apr[test_group==0])

apr_shuffle = replicate(10000,{test_group = sample(data$test_grp_flg, replace = TRUE)
                            apr = mean(data$apr[test_group==1]) - mean(data$apr[test_group==0])
                            apr_wa = weighted.mean(data$apr[test_group==1], data$ticket_size[test_group==1]) - weighted.mean(data$apr[test_group==0], data$ticket_size[test_group==0])                             
                            data.frame(apr,apr_wa) 
                                
                            }
                        )



permutated = replicate(10000,{test_group = sample(data$test_grp_flg, replace = TRUE)
              data %>%
              summarize(dif_apr_simple = mean(apr[test_group==1]) - mean(apr[test_group==0]),
                        dif_apr_wa = weighted.mean(apr[test_group==1], ticket_size[test_group==1]) - weighted.mean(apr[test_group==0], ticket_size[test_group==0])                       
                        )
              }, simplify=FALSE) %>%
              bind_rows

quantile(permutated$dif_apr_wa, 1-0.180)


#test44 = replicate(1000, sample_n(data, 30), simplify = FALSE) - takhle muzu replikovat cely sample ale pro shuffle test neni potreba


# VISUALIZE RESULT --------------------------
ggplot(permutated, aes(x=dif_apr_wa)) + geom_histogram(aes(y=..density..),binwidth=0.001) + 
            #stat_function(fun=dnorm, args=list(mean=mean(permutated$děif_apr_wa), sd=sd(permutated$dif_apr_wa)))+
            geom_vline(xintercept = 0.006115932, color="blue", size = 2)+
            geom_vline(xintercept = c(quantile(permutated$dif_apr_wa, 1-0.025),quantile(permutated$dif_apr_wa, 0.025)), color="red")
  #geom_density()

gplot(permutated, aes(x=dif_apr_simple)) + geom_histogram()


ggplot(data, aes(x=APR)) + geom_histogram() + facet_wrap(~test_grp_flg)#geom_histogram(bins=10)
ggplot(data, aes(x=APR,y=test_grp_flg)) + geom_point()

