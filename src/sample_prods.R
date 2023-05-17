# Building out a simple model
library(tidyverse)

# specs around stores and products
days <- c(1:20) #what day is this sold
hours <- c(1:12) #hours of sellable merchandise
products <- c(1:1000) #how many sku does this store have
shelf_amnt <- c(10) #how many products can fit on a shelf... this is effectively what the starting stock is.

# Accuracy Stuff ---
# based off the TP=5, FN = 5, FP =1, TN=89 confusion matrix
# Given we know the state of the facing... what's the probability we alert or not.....
corrOOS <- function(){sample(x = c("OOS", "Stocked"), size = 1, prob = c(0.5, 0.5), replace=T)}
corrStock <- function(){sample(x = c("OOS", "Stocked"), size = 1, prob = c(0.01, 0.99), replace=T)}
# -----------------


base_prods <-
  expand.grid(
    prodID = products, day = days
  ) %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(quant_start = sample(c(1:10),1, prob = c(0,0.05, 0.05, 0.05, 0.05, .1, .1 ,0.1, 0.4,0.1) ,replace = T)) %>% 
  ungroup() %>% 
  crossing(., hour = hours)

# dist_prod_removal <- function() {round(rexp(1, rate = 2),0)}
# 

# base_prods %>%
#   group_by(prodID) %>%
#   summarise(MinStart = min(quant_start),
#             MaxStart = max(quant_start),
#             counts = n())


base_prd_removal <-
  base_prods %>% 
  rowwise() %>% 
  # mutate(prod_pur = round(rexp(1, rate = 1.7),0)) %>% #assume products are purchased at roughly an exponential rate (this produces a sale through rate ~50-70%)
  mutate(prod_pur = actuar::rlogarithmic(n=1, prob=0.45)-1) %>% #trying with a logarithmic distribution, center on 0 instead of 1
  ungroup() %>% 
  group_by(prodID, day, quant_start) %>% 
  mutate(cumProds = cumsum(prod_pur),
         ProdStatus = quant_start - cumProds,
         OOS = ifelse(ProdStatus <= 0, "OOS", "Stocked")) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(Alert = ifelse(OOS == "OOS", corrOOS(), corrStock())) %>% 
  ungroup() %>% 
  mutate(CorrFlag = ifelse(OOS == Alert, "Correct", "Missed"))
  
# -------------
# 
# Confus <-
#   base_prd_removal %>% 
#   count(OOS, Alert) %>%
#   mutate(Per = n/sum(n)) %>% 
#   select(-n) %>% 
#   pivot_wider(values_from = Per, names_from = Alert)


# -------------

TotalProds <-
  base_prd_removal %>% 
  group_by(prodID, day) %>% 
  summarise(TotalProds = sum(prod_pur),
            Inventory = mean(quant_start)) %>% 
  mutate(CorrSold = ifelse(TotalProds > Inventory, Inventory, TotalProds),
         LossSales = ifelse(TotalProds > Inventory, TotalProds-Inventory, 0))


sum(TotalProds$CorrSold)/sum(TotalProds$Inventory)
sum(TotalProds$LossSales)



TimeFirstRunOut <-
  base_prd_removal %>% 
  filter(ProdStatus >= 0) %>% 
  group_by(prodID, day, quant_start) %>% 
  filter(hour == max(hour)) %>% 
  ungroup() %>% 
  mutate(TimeEmpty = 12- hour)



# visual of the sell through rate, each point represents a product
theme_set(theme_bw())

TotalProds %>% 
  mutate(SellThru = CorrSold/Inventory) %>% 
  group_by(day) %>% 
  mutate(AvgSell = mean(SellThru)) %>% 
  ungroup() %>% 
  ggplot(aes(day, SellThru, group = day))+
    geom_point(position = position_jitter(width = 0.2, height = 0.01), aes(color =Inventory ))+
    geom_violin(alpha = 0.5)+
    geom_point(aes(day, AvgSell), color = "red", size = 2)+
    geom_hline(yintercept = c(0.4, 0.7), linetype = "dashed")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.1), expand = c(0,0))+
  labs(title = "Sell-Through Percentage by Day",
       x= "Simulation Day", y = "Sell-Through (%)",
       subtitle = "Assuming a Logarithmic Sell Rate of Product Throughout the Day \nAnd that shelves are fully restocked at the start of each day")
  
TotalProds %>% 
  filter(LossSales > 0) %>% 
  group_by(day) %>% 
  summarise(Dist_SKU = n_distinct(prodID),
            totalMissed = sum(LossSales)) %>% 
  ungroup() %>% 
  ggplot(aes(x =1 ,totalMissed))+
    geom_point(aes(size = Dist_SKU))+
    geom_violin(alpha = 0.3)+
  labs(title = glue::glue("Total Sales Lost Due to Intraday Unavailability (Out of {max(products)} possible SKU)"),
       y = "Total Products of Lost Sales", x = NULL)+
  theme(axis.text.x = element_blank())





TimeFirstRunOut %>% 
  group_by(day) %>% 
  mutate(AvgEmpty = mean(TimeEmpty),
         MedEmpty = median(TimeEmpty)) %>% 
  ungroup() %>% 
  ggplot(aes(day, TimeEmpty, group = day))+
  geom_point(position = position_jitter(width = 0.2, height = 0.01), aes(color =quant_start ), 
             alpha= 0.5, size = 0.9)+
  geom_violin(alpha = 0.5)+
  geom_point(aes(day, AvgEmpty), color = "red", size = 2.5)+
  # geom_hline(yintercept = c(0.4, 0.7), linetype = "dashed")+
  # scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.1), expand = c(0,0))+
  labs(title = "Time of Facing Empty",
       subtitle = "Red Dot: Avg time a Facing is OOS Per Simulation Day",
       x= "Simulation Day", y = "Facing Empty Time")


TimeFirstRunOut %>% 
  group_by(day, TimeEmpty) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(day) %>% 
  mutate(PercentDay = n/sum(n)) %>% 
  ungroup() %>% 
  filter(TimeEmpty == 0) %>% 
  mutate(Great1Hour = 1-PercentDay) %>% 
  ggplot(aes(day, Great1Hour)) +
    geom_bar(stat = "identity", alpha = 0.6)+
    scale_y_continuous(labels = scales::percent_format(), expand = c(0,0), limits = c(0,0.5))+
    labs(title = "Percent of Facings OOS for 1hr or Longer",
         y = NULL, x= "Simulation Day")




base_prd_removal %>% 
  count(day, hour, OOS) %>% 
  group_by(day, hour) %>% 
  mutate(PerOOS = n/sum(n)) %>% 
  ungroup() %>% 
  filter(OOS == "OOS") %>% 
  mutate(hour = factor(hour)) %>% 
  ggplot(aes(hour, PerOOS, group = day))+
    geom_line(color= "grey", alpha=0.7)+
    geom_point()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Percent OOS of Facings by Hour of Day",
       subtitle = "Grouped by Simulation Day",
       y= "Percent of Facings OOS")


# sample(c(0,1), 100, replace = TRUE)

## Visual of Funnel Caught ------
base_prd_removal %>% 
  count(day, hour, OOS, CorrFlag) %>% 
  group_by(day, hour, OOS) %>%
  mutate(CountStock = sum(n)) %>%
  ungroup() %>% 
  group_by(day, hour) %>% 
  mutate(TotalProd = sum(n)) %>% 
  ungroup() %>% 
  mutate(OSS_Corr = n/TotalProd,
         OSS_Stock = CountStock/TotalProd) %>% 
  filter(OOS == "OOS",
         CorrFlag == "Correct") %>% 
  filter(day == 1) %>% 
  mutate(hour = factor(hour)) %>% 
  ggplot(aes(group = day))+
    geom_line(aes(hour, OSS_Corr), color = "blue")+
    geom_point(aes(hour, OSS_Corr), color = "blue")+
    geom_line(aes(hour, OSS_Stock), color = "grey")+
    geom_point(aes(hour, OSS_Stock), color = "grey")+
  labs(title = "Savings with Variable Accuracy")  




base_prd_removal %>% 
  count(day, hour, OOS, CorrFlag) %>% 
  group_by(day, hour, OOS) %>%
  mutate(CountStock = sum(n)) %>%
  ungroup() %>% 
  group_by(day, hour) %>% 
  mutate(TotalProd = sum(n)) %>% 
  ungroup() %>% 
  mutate(OSS_Corr = n/TotalProd,
         OSS_Stock = CountStock/TotalProd) %>% 
  filter(OOS == "OOS",
         CorrFlag == "Correct") %>% 
  # filter(day == 1) %>% 
  mutate(hour = factor(hour)) %>% 
  ggplot(aes(group = day))+
  geom_line(aes(hour, OSS_Corr), color = "blue")+
  geom_point(aes(hour, OSS_Corr), color = "blue")+
  geom_line(aes(hour, OSS_Stock), color = "grey")+
  geom_point(aes(hour, OSS_Stock), color = "grey")+
  geom_ribbon(aes(hour, ymin = OSS_Corr, ymax = OSS_Stock), alpha = 0.2, fill = "#028A0F") +
  geom_ribbon(aes(hour, ymin = 0, ymax = OSS_Corr), alpha = 0.2, fill = "#FFD300") +
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  labs(title = "Savings with Variable Accuracy",
       subtitle = "Green: Savings with Current Model Accuracy, \nYellow: Additional Savings with Perfect Accuracy") 




