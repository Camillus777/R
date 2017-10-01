## conversion hypothesis evaluation
library(tidyverse)
library(scales)
## option one reshuffling (permutation test)
S1_n = 1000
S1_1 = 20
S2_n = 1000
S2_1 = 30

S1 = c(replicate(S1_1,1), replicate (S1_n-S1_1,0))
S2 = c(replicate(S2_1,1), replicate (S2_n-S2_1,0))
S_full = c(S1,S2)

diff = data.frame(value = sum(S_full[1:S1_n])/S1_n - sum(S_full[(S1_n+1):length(S_full)])/S2_n)
permutated_diff = replicate(n=10000, {
                  temp = sample(S_full, replace = FALSE)
                  
                sum(temp[1:S1_n])/S1_n - sum(temp[(S1_n+1):length(temp)])/S2_n
                }) 

permutated_diff = data.frame(value  = permutated_diff)
quantiles = permutated_diff  %>% summarize(lower=quantile(value, 0.025), higher = quantile(value, 1-0.025))

bootstrappped_diff = replicate(n=10000, {
  tempS1 = sample(S1, replace = TRUE)
  tempS2 = sample(S2, replace = TRUE)
  sum(tempS1)/S1_n - sum(tempS2)/S2_n
}) 
bootstrappped_diff = data.frame(value  = bootstrappped_diff)
quantiles = bootstrappped_diff  %>% summarize(lower=quantile(value, 0.025), higher = quantile(value, 1-0.025))

ggplot(permutated_diff, aes(x=value)) + 
  #geom_histogram(alpha=0.4, fill="blue", binwidth = 0.002 ) +
  geom_histogram(data =bootstrappped_diff, alpha=0.4, fill="red", binwidth = 0.002 ) +
  #geom_density(alpha=0.4, fill= "blue", color = "white")+
  #geom_density(data = permutated_diff %>% filter(value<=quantiles$lower),alpha=0.4, fill= "blue", color = "white")
  scale_x_continuous(labels=scales::percent, breaks = seq(-0.03, 0.03, by= 0.01))+
  #stat_summary(aes(y=..count..),fun.y = sum, fill="blue",geom ="area") +
  #layer(mapping = aes(x=ifelse(x < quantiles$lower,x,quantiles$lower), y=y), geom = "area", geom_params=list(fill="red",alpha=.3))+
  geom_vline(data = quantiles, aes(xintercept = lower), color="white", alpha=0.5, size=1) +
  geom_vline(data = quantiles, aes(xintercept = higher), color="white",alpha=0.5,size=1) +
  geom_vline(data = diff, aes(xintercept = value), color="blue")+
  labs(x= "Conversion difference", title="No difference between conversion rates was proved")+
  theme_minimal() 
  
