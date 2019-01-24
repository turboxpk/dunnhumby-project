#Dunnhumby Case Study Carbo_loading#
#Created by PK Xia on January 2019#
#load libary#
library(sqldf)
library(dplyr)

#read the product csv file#
dh_product<-read.csv('dh_product_lookup.csv', header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
product<-data.frame(dh_product, row.names = NULL, check.rows = FALSE,
           check.names = TRUE, fix.empty.names = FALSE,
           stringsAsFactors = FALSE)
#find space and na#
summary(is.na(product))
#found 3 NA cells in product_size#

#read the transactions csv file#
dh_transaction<-read.csv('dh_transactions.csv', header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
trans<-data.frame(dh_transaction, row.names = NULL, check.rows = FALSE,
                    check.names = TRUE, fix.empty.names = FALSE,
                    stringsAsFactors = FALSE)
#find space and na#
summary(is.na(trans))
#no NA in transaction table#

#check if the NA in product table contribute to sales#
sqldf("SELECT p.upc, p.commodity, sum(t.dollar_sales*t.units) AS Revenue FROM product p JOIN trans t ON p.upc=t.upc 
WHERE p.product_size IS NULL
GROUP BY p.upc")
#Revenue not null, decided to keep the upc#

#read the store csv file#
dh_store<-read.csv('dh_store_lookup.csv', header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
stores<-data.frame(dh_store, row.names = NULL, check.rows = FALSE,
                  check.names = TRUE, fix.empty.names = FALSE,
                  stringsAsFactors = FALSE)
#find space and na#
summary(is.na(stores))


#Top 5 products in each commodity#
#pasta#
pasta<-sqldf("SELECT p.commodity, p.upc, p.product_description, sum(t.dollar_sales*t.units) AS Revenue
FROM product p JOIN trans t ON p.upc=t.upc
WHERE p.commodity='pasta'
GROUP BY p.commodity, p.upc, p.product_description
ORDER BY Revenue DESC LIMIT 5")

#pasta sauce#
pasta_sauce<-sqldf("SELECT p.commodity, p.upc, p.product_description, sum(t.dollar_sales*t.units) AS Revenue
FROM product p JOIN trans t ON p.upc=t.upc
      WHERE p.commodity='pasta sauce'
      GROUP BY p.commodity, p.upc, p.product_description
      ORDER BY Revenue DESC LIMIT 5")

#pancake mix#
pancake<-sqldf("SELECT p.commodity, p.upc, p.product_description, sum(t.dollar_sales*t.units) AS Revenue
FROM product p JOIN trans t ON p.upc=t.upc
      WHERE p.commodity='pancake mixes'
      GROUP BY p.commodity, p.upc, p.product_description
      ORDER BY Revenue DESC LIMIT 5")
#syrup#
syrup<-sqldf("SELECT p.commodity, p.upc, p.product_description, sum(t.dollar_sales*t.units) AS Revenue
FROM product p JOIN trans t ON p.upc=t.upc
             WHERE p.commodity='syrups'
             GROUP BY p.commodity, p.upc, p.product_description
             ORDER BY Revenue DESC LIMIT 5")

#Top 5 brands in each commodity#
pasta_brands<-sqldf("SELECT p.commodity, p.brand, sum(t.dollar_sales*t.units) AS Revenue
FROM product p JOIN trans t ON p.upc=t.upc
WHERE p.commodity='pasta'
GROUP BY p.commodity, p.brand
ORDER BY Revenue DESC LIMIT 5")

pasta_sauce_brands<-sqldf("SELECT p.commodity, p.brand, sum(t.dollar_sales*t.units) AS Revenue
FROM product p JOIN trans t ON p.upc=t.upc
                          WHERE p.commodity='pasta sauce'
                          GROUP BY p.commodity, p.brand
                          ORDER BY Revenue DESC LIMIT 5")

pancake_brands<-sqldf("SELECT p.commodity, p.brand, sum(t.dollar_sales*t.units) AS Revenue
FROM product p JOIN trans t ON p.upc=t.upc
                      WHERE p.commodity='pancake mixes'
                      GROUP BY p.commodity, p.brand
                      ORDER BY Revenue DESC LIMIT 5")

syrup_brands<-sqldf("SELECT p.commodity, p.brand, sum(t.dollar_sales*t.units) AS Revenue
FROM product p JOIN trans t ON p.upc=t.upc
                    WHERE p.commodity='syrups'
                    GROUP BY p.commodity, p.brand
                    ORDER BY Revenue DESC LIMIT 5")


#What is driving sales# 

#Commodity Sales for each year#
Revenue_by_commodity<-sqldf("SELECT p.commodity, sum(CASE WHEN t.week>=1 AND t.week<=52 THEN t.dollar_sales*t.units END) AS Year1, sum(CASE WHEN t.week>52 AND t.week<=104 THEN t.dollar_sales*t.units END) AS Year2,
                            ((sum(CASE WHEN t.week>=1 AND t.week<=52 THEN t.dollar_sales*t.units END)/sum(CASE WHEN t.week>52 AND t.week<=104 THEN t.dollar_sales*t.units END))-1)*100 AS YOY_pct
                            FROM product p JOIN trans t ON p.upc=t.upc
                            GROUP BY p.commodity
                            ")
Revenue_by_commodity<-mutate(Revenue_by_commodity, Year1Share= Year1*100/sum(Year1), Year2Share=Year2*100/sum(Year2))
Revenue_by_commodity_1<-sqldf("SELECT commodity, Year1, Year2, YOY_pct, (Year1Share-Year2Share) AS Share_Change FROM Revenue_by_commodity
                              ORDER BY Share_Change DESC")

#Pasta Brand Sales for each year#
Revenue_by_brand<-sqldf("SELECT p.brand, p.commodity, sum(CASE WHEN t.week>=1 AND t.week<=52 THEN t.dollar_sales*t.units END) AS Year1, sum(CASE WHEN t.week>52 AND t.week<=104 THEN t.dollar_sales*t.units END) AS Year2,
                        ((sum(CASE WHEN t.week>=1 AND t.week<=52 THEN t.dollar_sales*t.units END)/sum(CASE WHEN t.week>52 AND t.week<=104 THEN t.dollar_sales*t.units END))-1)*100 AS YOY_pct
                        FROM product p JOIN trans t ON p.upc=t.upc
                        WHERE p.commodity='pasta'
                        GROUP BY p.brand
                        HAVING YOY_pct IS NOT NULL
                        ORDER BY Year1 DESC, Year2, YOY_pct
                        ")
#replace the NA sales with 0
Revenue_by_brand$Year1[is.na(Revenue_by_brand$Year1)]<-0
Revenue_by_brand$Year2[is.na(Revenue_by_brand$Year2)]<-0

#Add share calculation for each brand
#table ranked by most recent year sales
Revenue_by_brand<-mutate(Revenue_by_brand, Year1Share= Year1*100/sum(Year1), Year2Share=Year2*100/sum(Year2))
#calculate top 10 brands with the largest share gain
brand_share<-sqldf("SELECT brand, commodity, Year1Share, Year2Share, YOY_pct, (Year1Share-Year2Share) AS Share_change FROM Revenue_by_brand
                   ORDER BY Share_change DESC LIMIT 10")



#household with consistent spending in these commodities over the last two years#
Revenue_by_household<-sqldf("SELECT t.household, p.commodity, s.store_zip_code, t.geography, sum(CASE WHEN t.week>=1 AND t.week<=52 THEN t.dollar_sales*t.units END) AS Year1, sum(CASE WHEN t.week>52 AND t.week<=104 THEN t.dollar_sales*t.units END) AS Year2,
                            ((sum(CASE WHEN t.week>=1 AND t.week<=52 THEN t.dollar_sales*t.units END)/sum(CASE WHEN t.week>52 AND t.week<=104 THEN t.dollar_sales*t.units END))-1)*100 AS YOY_pct
                            FROM trans t JOIN stores s ON t.store=s.store JOIN product p ON p.upc=t.upc
                            WHERE p.commodity='pasta'
                            GROUP BY t.household
                            HAVING YOY_pct IS NOT NULL
                            ORDER BY Year1 DESC, Year2, YOY_pct")

#replace the NA sales with 0#
Revenue_by_household$Year1[is.na(Revenue_by_household$Year1)]<-0
Revenue_by_household$Year2[is.na(Revenue_by_household$Year2)]<-0

#Add share calculation for each household#
#table ranked by most recent year sales#
Revenue_by_household<-mutate(Revenue_by_household, Year1Share= Year1*100/sum(Year1), Year2Share=Year2*100/sum(Year2))
#calculate top 10 households with the largest share gain
household_share<-sqldf("SELECT household, commodity, store_zip_code, geography, Year1Share, Year2Share, YOY_pct, (Year1Share-Year2Share) AS Share_change 
                       FROM Revenue_by_household
                       ORDER BY Share_change DESC LIMIT 10")


#Repurchase Rate Calculation#
#RP for pasta for the first 30 days

re30_pasta<-sqldf("SELECT p.commodity, t.household, count(*)
                  FROM product p JOIN trans t ON p.upc=t.upc
                  WHERE p.commodity='pasta' AND t.day<=30
                  GROUP BY p.commodity, t.household
                  HAVING count(*)>1")

hh30_pasta<-sqldf("SELECT p.commodity, t.household, t.units
                  FROM product p JOIN trans t ON p.upc=t.upc
                  WHERE p.commodity='pasta' AND t.day<=30
                  GROUP BY t.household
                  HAVING t.units>0 ")
#how many household purchase pasta again in 30 days/total number of households purchased pasta in 30 days
rp30_pasta<-length(unique(re30_pasta$household))/length(unique(hh30_pasta$household))
#CLV repurchase rate
rp_clv_30_pasta<-sum(re30_pasta$`count(*)`)/length(unique(hh30_pasta$household))

#RP for pasta sauce for the first 30 days

re30_ps<-sqldf("SELECT p.commodity, t.household, count(*)
               FROM product p JOIN trans t ON p.upc=t.upc
               WHERE p.commodity='pasta sauce' AND t.day<=30
               GROUP BY p.commodity, t.household
               HAVING count(*)>1")

hh30_ps<-sqldf("SELECT p.commodity, t.household, t.units
               FROM product p JOIN trans t ON p.upc=t.upc
               WHERE p.commodity='pasta sauce' AND t.day<=30
               GROUP BY t.household
               HAVING t.units>0 ")
#how many household purchase pasta again in 30 days/total number of households purchased pasta sauce in 30 days
rp30_ps<-length(unique(re30_ps$household))/length(unique(hh30_ps$household))
#CLV repurchase rate
rp_clv_30_ps<-sum(re30_ps$`count(*)`)/length(unique(hh30_ps$household))


#RP for pancake mix for the first 30 days

re30_pm<-sqldf("SELECT p.commodity, t.household, count(*)
               FROM product p JOIN trans t ON p.upc=t.upc
               WHERE p.commodity='pancake mixes' AND t.day<=30
               GROUP BY p.commodity, t.household
               HAVING count(*)>1")

hh30_pm<-sqldf("SELECT p.commodity, t.household, t.units
               FROM product p JOIN trans t ON p.upc=t.upc
               WHERE p.commodity='pancake mixes' AND t.day<=30
               GROUP BY t.household
               HAVING t.units>0 ")
#how many household purchase pasta again in 30 days/total number of households purchased pancake mix in 30 days
rp30_pm<-length(unique(re30_pm$household))/length(unique(hh30_pm$household))
#CLV repurchase rate
rp_clv_30_pm<-sum(re30_pm$`count(*)`)/length(unique(hh30_pm$household))

#RP for syrup for the first 30 days

re30_sp<-sqldf("SELECT p.commodity, t.household, count(*)
               FROM product p JOIN trans t ON p.upc=t.upc
               WHERE p.commodity='syrups' AND t.day<=30
               GROUP BY p.commodity, t.household
               HAVING count(*)>1")

hh30_sp<-sqldf("SELECT p.commodity, t.household, t.units
               FROM product p JOIN trans t ON p.upc=t.upc
               WHERE p.commodity='syrups' AND t.day<=30
               GROUP BY t.household
               HAVING t.units>0 ")
#how many household purchase pasta again in 30 days/total number of households purchased syrup in 30 days
rp30_sp<-length(unique(re30_sp$household))/length(unique(hh30_sp$household))
#CLV repurchase rate
rp_clv_30_sp<-sum(re30_sp$`count(*)`)/length(unique(hh30_sp$household))


#coupon usage by commodity

coupon_usage<-sqldf("SELECT p.commodity, t.coupon, count(p.commodity) AS NumberOfUse FROM product p JOIN trans t ON p.upc=t.upc
                    GROUP BY p.commodity,t.coupon")





