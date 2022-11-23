library(tidyverse)
library(lubridate)
#import datasets
#datasets appear to be clean, don't require pre-processing 
df_addsToCart <- read_csv('DataAnalyst_Ecom_data_addsToCart.csv')
glimpse(df_addsToCart)
view(df_addsToCart)
str(df_addsToCart)

#check dataset for missing values
df_addsToCart %>%  summarise(across(everything(), 
                                    list(NA_count = ~sum(is.na(.)), 
                                         UNQ_count = ~length(unique(.)))))

#convert date character to date format
df_sessionCounts <- read_csv('DataAnalyst_Ecom_data_sessionCounts.csv')
df_sessionCounts <- mutate(df_sessionCounts, dim_date = mdy(dim_date))
glimpse(df_sessionCounts)
view(df_sessionCounts)
str(df_sessionCounts)

#check dataset for missing values
df_sessionCounts %>%  summarise(across(everything(), list(NA_count = ~sum(is.na(.)))))
df_sessionCounts %>%  summarise(across(everything(), list(UNQ_count = ~length(unique(.)))))


# Create Month*Device aggregate on sessions, txns, qty, and ECR
df_sessionAgg <- df_sessionCounts %>% 
  group_by(dim_deviceCategory, dim_month = year(dim_date)*100 + month(dim_date))  %>%
  summarise(sessions = sum(sessions),
            transactions = sum(transactions),
            QTY = sum(QTY),
            ECR = sum(transactions)/sum(sessions),
            .groups = 'drop')

view(df_sessionAgg)
head(df_sessionAgg)

# Create Month*Device*Browser aggregate on sessions, txns, qty, and ECR
#top 5 browsers to filter on
browser_filter <- df_sessionCounts %>% group_by(dim_browser)  %>%
  summarise(sessions = sum(sessions),
            .groups = 'drop') %>%
  arrange(desc(sessions)) %>%
  slice(1:5) %>%
  pull(dim_browser)

df_sessionCountsBr <- filter(df_sessionCounts, dim_browser %in% browser_filter)
view(df_sessionCountsBr)

#browser/category aggregate table
df_sessionAggBr <- df_sessionCountsBr %>% group_by(dim_browser, dim_deviceCategory, dim_month = year(dim_date)*100 + month(dim_date))  %>%
  summarise(sessions = sum(sessions),
            transactions = sum(transactions),
            QTY = sum(QTY),
            ECR = sum(transactions)/sum(sessions),
            .groups = 'drop') 

view(df_sessionAggBr)
head(df_sessionAggBr)

# Create month aggregate on sessions, txns, qty, and ECR
df_sessionTemp = df_sessionCounts %>% group_by(dim_month = year(dim_date)*100 + month(dim_date))  %>%
  summarise(sessions = sum(sessions),
            transactions = sum(transactions),
            QTY = sum(QTY),
            ECR = sum(transactions)/sum(sessions),
            .groups = 'drop')
head(df_sessionTemp)

# Convert addsToCart date to YYYYmm for joining to sessions table
df_addsToCart = mutate(df_addsToCart, dim_month = dim_year*100 + dim_month)
df_addsToCart = select(df_addsToCart, -dim_year)
head(df_addsToCart)

# Join session and adds datasets
df_metricsJoin = df_sessionTemp %>% inner_join(df_addsToCart, by = 'dim_month')
view(df_metricsJoin)

# Create MoM metrics, absolute + relative difference to previous month
df_metricsAgg =  mutate(df_metricsJoin, 
                        prev_sessions = lag(sessions,order_by = dim_month),
                        prev_transactions = lag(transactions,order_by = dim_month),
                        prev_QTY = lag(QTY,order_by = dim_month),
                        prev_ECR = lag(ECR,order_by = dim_month),
                        prev_addsToCart = lag(addsToCart,order_by = dim_month))
df_metricsAgg =  mutate(df_metricsAgg, 
                        absdiff_sessions = sessions-prev_sessions,
                        absdiff_transactions = transactions-prev_transactions,
                        absdiff_QTY = QTY-prev_QTY,
                        absdiff_ECR = ECR-prev_ECR,
                        absdiff_addsToCart = addsToCart-prev_addsToCart,
                        reldiff_sessions = (sessions-prev_sessions)/prev_sessions,
                        reldiff_transactions = (transactions-prev_transactions)/prev_transactions,
                        reldiff_QTY = (QTY-prev_QTY)/prev_QTY,
                        reldiff_ECR = (ECR-prev_ECR)/prev_ECR,
                        reldiff_addsToCart = (addsToCart-prev_addsToCart)/prev_addsToCart)
view(df_metricsAgg)

library(openxlsx)
# Write tables to xlsx
write.xlsx(list(df_sessionAgg, df_metricsAgg, df_sessionAggBr), file='IXISDigitalTechTables.xlsx', sheetName = list('Sessions Agg', 'MoM Metrics Agg', 'Sess+Brows Agg'))