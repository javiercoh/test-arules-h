library(arules)
library(dplyr)

.GlobalEnv$dataframe1 <- read.csv("df1.csv")
colnames(dataframe1) <- c("customer_id", "customer_size", "sku_nbr", "avg_ROS", "sku_id", "category_id", "sku_amount", "sku_qty", "top_skus", "city_id")

.GlobalEnv$sup_level_agg = 0.10
.GlobalEnv$conf_level_agg = .70

city<-'3'

.GlobalEnv$df <- dataframe1[dataframe1$city_id == city,]

.GlobalEnv$aggregate_categories <- T
.GlobalEnv$base_str <- paste('categories city ',city)
params <- list(support=sup_level_agg, conf=conf_level_agg, minlen=3, maxlen=15)

sku_id_vec <-as.character(.GlobalEnv$df$sku_id)
customer_id_vec <-as.character(.GlobalEnv$df$customer_id)
.GlobalEnv$dataset <- as(split(sku_id_vec, customer_id_vec), "transactions")

# print('******* GOING TO MINE AGGREGATES +++++++++++++++++')
category <- .GlobalEnv$df %>% select(sku_id, category_id) %>% distinct()  # https://stackoverflow.com/questions/55998277/r-how-to-incorporate-categories-of-item-set-in-transactions-data
.GlobalEnv$dataset@itemInfo <- data.frame(labels = category$sku_id, category_id = category$category_id)
.GlobalEnv$dataset <- addAggregate(.GlobalEnv$dataset, "category_id")
.GlobalEnv$dataset <- aggregate(.GlobalEnv$dataset, by = "category_id")

rules <- apriori(.GlobalEnv$dataset, parameter = params)
rules <- subset(rules, subset = confidence < 1 & lift > 1) #https://www.rdocumentation.org/packages/arules/versions/1.6-7/topics/subset
rules <- sort(rules, decreasing = FALSE, by="confidence")
rules <- head(rules, 3500)

superlhs <- is.superset(dataset, lhs(rules))
superrhs <- is.superset(dataset, rhs(rules))
rhs_colnames <- colnames(superrhs)


# now going to check case rhs = "IP0402-LEUDANTES CO" and customer_id <- "C1020395696"
customer <- c("C1020395696")
customer_index <- dataset@itemsetInfo$transactionID==customer
inspect(dataset[customer_index])
df_customer <- df[df$customer_id == "C1020395696",]

#this seems fine, so we can see that for two items of category_id "IP0402-LEUDANTES CO"
# the category df containes
category[category$sku_id == "AD01020018",]
category[category$sku_id == "AD01020009",]

#so far df_customer should throw empty  at this:
df_customer[df_customer$sku_id == "AD01020018",]

# So here  is the thing: according to next line, customer should contain aggregate item/category_id "IP0402-LEUDANTES CO"
df_customer[df_customer$sku_id == "AD01020009",]

#however when checking the transformed dataset, the transaction for customer C1020395696 does not contain IP0402-LEUDANTES CO
inspect(dataset[customer_index])

