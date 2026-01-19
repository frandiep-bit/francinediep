install.packages("DBI")
install.packages("RMySQL") 
install.packages("tidyverse") 
install.packages("RODBC")
install.packages("odbc")

library(DBI)
library(RMySQL)
library(RODBC)
library(odbc)

#connecting sql
connectmysqlproj <- dbConnect(
  RMySQL::MySQL(),
  dbname = "PROJECT",     
  host = "localhost",      
  port = 3306,           
  user = "root", 
  password = "DELETED!" 
)

#service requests by prop (dont use)

service_requests1 <- dbGetQuery(con, "
  SELECT 
      p.PROPERTY_ID,
      p.ADDRESS,
      COUNT(sr.SERVICE_ID) AS NUM_SERVICE_REQUESTS
  FROM 
      PROPERTY p
  LEFT JOIN 
      SERVICE_REQUEST sr
  ON 
      p.PROPERTY_ID = sr.PROPERTY_ID
  GROUP BY 
      p.PROPERTY_ID, p.ADDRESS;
")

ggplot(service_requests, aes(x = reorder(ADDRESS, -NUM_SERVICE_REQUESTS), y = NUM_SERVICE_REQUESTS)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Service Requests Per Property",
    x = "Property Address",
    y = "Number of Service Requests"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

service_request_cat <- dbGetQuery(con, "
  SELECT 
      sc.CATEGORY_DESCRIPTION,
      COUNT(sr.SERVICE_ID) AS NUM_SERVICE_REQUESTS
  FROM 
      SERVICE_REQUEST sr
  JOIN 
      SERVICE_CATEGORY sc
  ON 
      sr.CATEGORY_NUMBER = sc.CATEGORY_NUM
  GROUP BY 
      sc.CATEGORY_DESCRIPTION;
")

#PIE CHART FOR CATEGORY & COUNT SERV REQ (use this)
ggplot(service_request_categories, aes(x = "", y = NUM_SERVICE_REQUESTS, fill = CATEGORY_DESCRIPTION)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(NUM_SERVICE_REQUESTS)), 
    position = position_stack(vjust = 0.5), 
    size = 4
  ) +
  labs(
    title = "Service Request Categories",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(legend.title = element_blank())


###total serv by cat (use)


ggplot(service_data, aes(x = reorder(CATEGORY_DESCRIPTION, Total_Requests), y = Total_Requests)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total Service Requests by Category", x = "Service Category", y = "Total Requests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####Look at the service category by types and hours spent (use both)
query1 <- "
SELECT 
    sr.CATEGORY_NUMBER,
    sc.CATEGORY_DESCRIPTION,
    COUNT(*) AS Total_Requests,
    SUM(sr.SPENT_HOURS) AS Total_Hours,
FROM SERVICE_REQUEST sr
JOIN SERVICE_CATEGORY sc
    ON sr.CATEGORY_NUMBER = sc.CATEGORY_NUM
GROUP BY sr.CATEGORY_NUMBER, sc.CATEGORY_DESCRIPTION
ORDER BY Avg_Hours DESC;
"


service_data1 <- dbGetQuery(con, query1)

ggplot(service_data, aes(x = reorder(CATEGORY_DESCRIPTION, Total_Requests), y = Total_Requests)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Total Service Requests by Category", 
       x = "Service Category", 
       y = "Total Requests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#hours spent by cat

ggplot(service_data, aes(x = reorder(CATEGORY_DESCRIPTION, Total_Hours), y = Total_Hours)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Total Hours Spent by Category", 
       x = "Service Category", 
       y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#part two

#PULL IN SERVICE REQUESTS BY MONTH
query2 <- "
SELECT 
    sr.CATEGORY_NUMBER,
    sc.CATEGORY_DESCRIPTION,
    MONTH(sr.NEXT_SERVICE_DATE) AS Service_Month,
    YEAR(sr.NEXT_SERVICE_DATE) AS Service_Year,
    COUNT(*) AS Total_Requests,
    SUM(sr.SPENT_HOURS) AS Total_Hours,
    ROUND(AVG(sr.SPENT_HOURS), 2) AS Avg_Hours
FROM SERVICE_REQUEST sr
JOIN SERVICE_CATEGORY sc
    ON sr.CATEGORY_NUMBER = sc.CATEGORY_NUM
WHERE sr.NEXT_SERVICE_DATE IS NOT NULL
GROUP BY sr.CATEGORY_NUMBER, sc.CATEGORY_DESCRIPTION, Service_Month, Service_Year
ORDER BY Service_Year, Service_Month, Avg_Hours DESC;
"

service_data <- dbGetQuery(con, query2)

library(ggplot2)

library(DBI)
library(RMySQL)

connect <- dbConnect(
  RMySQL::MySQL(),
  dbname = "PROJECT",     
  host = "localhost",      
  port = 3306,           
  user = "root", 
  password = "DELETED!" 
)

projquery <- "
SELECT 
    o.OWNER_NUM,
    p.PROPERTY_ID,
    CONCAT(o.FIRST_NAME, ' ', o.LAST_NAME) AS OWNER_NAME,
    COUNT(sr.SERVICE_ID) AS Total_Requests
FROM OWNER o
JOIN PROPERTY p ON o.OWNER_NUM = p.OWNER_NUM
JOIN SERVICE_REQUEST sr ON p.PROPERTY_ID = sr.PROPERTY_ID
GROUP BY o.OWNER_NUM, OWNER_NAME, p.PROPERTY_ID
ORDER BY Total_Requests DESC;
"

result1 <- dbGetQuery(connect, projquery)

#total serv req by owner (use)

ggplot(data = result, aes(x = reorder(OWNER_NAME, -Total_Requests), y = Total_Requests)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Total Service Requests by Owner",
    x = "Owner Name",
    y = "Total Requests"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) #rotate x-axis labels 
  )


#TOTAL EST HOURS OF CATEGORIES SCHEDULED OR OPEN REQUESTS BAR CHART

con <- dbConnect(
  RMySQL::MySQL(),
  dbname = "PROJECT",     
  host = "localhost",      
  port = 3306,           
  user = "root", 
  password = "DELETED!" 
)

query <- "
SELECT 
    sr.CATEGORY_NUMBER,
    sc.CATEGORY_DESCRIPTION,
    sr.STATUS,
    COUNT(*) AS Total_Requests,
    SUM(sr.EST_HOURS) AS Total_EST_Hours
FROM SERVICE_REQUEST sr
JOIN SERVICE_CATEGORY sc
    ON sr.CATEGORY_NUMBER = sc.CATEGORY_NUM
    WHERE SPENT_HOURS = 0
GROUP BY sr.CATEGORY_NUMBER, sc.CATEGORY_DESCRIPTION, STATUS
ORDER BY TOTAL_EST_HOURS DESC;
"

result3 <- dbGetQuery(con, query)

library(ggplot2)

ggplot(result3, aes(x = reorder(CATEGORY_DESCRIPTION, Total_EST_Hours), 
                 y = Total_EST_Hours, fill = STATUS)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Total Estimated Hours by Category and Status",
       x = "Category Description",
       y = "Total Estimated Hours") +
  theme_minimal()

