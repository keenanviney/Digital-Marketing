##-----------------------------
#  Games Company Retail Analysis
##-----------------------------

require(tidyverse)
require(arules)

### Retail Metrics ----------------------------------------------------------------------------------------------------------------------

data <- read_csv("~/Box/Keenan.Viney/Desktop Backup/Games Company/Retail Analysis Pitch/aug-sept-sales.csv", col_names = TRUE) %>% 
  mutate(Key = paste0(entity_id, increment_id, customer_id, sku, price))

## Create a dataframe with the sized items
remove_sized_items <- data %>%
  filter(lag(product_type == "configurable") & product_type == "simple") %>% 
  mutate(Key = paste0(entity_id, increment_id, customer_id, sku, price))

## Anti-join to remove the sized items and leave only the generic item with the price
metrics <- anti_join(data, remove_sized_items, by = "Key")

### Calculating Metrics
Items_sold <- metrics %>% group_by(name) %>% 
  count(Total_item_sales = sum(price)) %>% 
  rename(Item_velocity = n)

Baskets <- metrics %>% group_by(increment_id) %>% count(sum(price)) %>% rename(Items_in_basket = n, Basket_Price = `sum(price)`)

Merged_Metrics <- left_join(metrics, Items_sold, by = "name") %>% left_join(Baskets, by = "increment_id")

Calculated_Metrics <- Merged_Metrics %>% mutate(Percent_of_Total_Sale = Total_item_sales/sum(Total_item_sales), Basket_Cost_x_Target = Basket_Price - price)

Item_Names <- unique(Calculated_Metrics$name)

Increment_fnc <- function(target_item) {
  Target_baskets <- filter(Calculated_Metrics, name == target_item) %>% select(increment_id, name, Basket_Cost_x_Target)
  Other_baskets <- anti_join(Calculated_Metrics, Target_baskets, by = "increment_id") %>% select(increment_id, name, Basket_Cost_x_Target)
  Alpha_Target <- mean(Target_baskets$Basket_Cost_x_Target)
  Beta_Other <- mean(Other_baskets$Basket_Cost_x_Target)
  Incremental_Basket_Contribution <- Alpha_Target - Beta_Other
  print(Incremental_Basket_Contribution)
}

output <- vector("numeric", length = length(Item_Names))
for (i in seq_along(Item_Names)) {
  output[[i]] <- Increment_fnc(Item_Names[[i]])
}

Increment_List <- data_frame(Item_Names, output) %>% arrange(desc(output))

Average_Price <- Calculated_Metrics %>% 

Increment_List_Price <- left_join(Increment_List, Calculated_Metrics, by = c("Item_Names" = "name")) %>%
  select(Item_Names, output, price) %>% rename(Incremental_Metric = output) 

#df <- mutate(Calculated_Metrics, Test = Increment_fnc(Calculated_Metrics$name))

Target_Item <- "Game Wrecking Ball Shirt - Men's"
#Target_baskets <- filter(Calculated_Metrics, name == Target_item) %>% select(increment_id, name, Basket_Cost_x_Target)
#Other_baskets <- anti_join(Calculated_Metrics, Target_baskets, by = "increment_id") %>% select(increment_id, name, Basket_Cost_x_Target)
#Alpha_Target <- mean(Target_baskets$Basket_Cost_x_Target)
#Beta_Other <- mean(Other_baskets$Basket_Cost_x_Target)
#Incremental_Basket_Contribution <- A-B


### Association Rules -----------------------------------------------------------------------------------------------------------
trans <- read.transactions("~/Box Sync/Keenan.Viney/Desktop Backup/Games Company/Retail Analysis Pitch/Transactions_Cleaned.csv",
                           rm.duplicates = TRUE, sep = ",", format = "single", cols = c(2,7)) 

rules <- apriori(data = trans, parameter = list(supp = 0.0001, conf = 0.0001, minlen = 2), control = list (verbose=F))

rules_lift <- sort(rules, by =  "lift", decreasing = TRUE)

df_rules <- as(rules_lift, "data.frame")

inspect(head(rules_lift))

Zen_rules <- apriori(data = trans, parameter = list(supp = 0.00001, conf = 0.0001, minlen = 2, maxlen = 2), appearance = list(default="lhs", rhs="Game Ultimate Zen Hoodie"), control = list (verbose=F))

Zen_rules_lift <- sort(Zenyatta_rules, by =  "lift", decreasing = TRUE)

inspect(Zen_rules_lift)
