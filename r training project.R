# Write a code to get your data into a dataframe from a csv

# Point to the correct directory:

getwd()
setwd("a:\\Training\\R Programming\\Adventure Works Data")
getwd()





# Import Sales data - Transactional Data

AdventureWorks_Sales_2015 = read.csv("AdventureWorks_Sales_2015.csv", header = TRUE, sep = ",")

AdventureWorks_Sales_2016 = read.csv("AdventureWorks_Sales_2016.csv", header = TRUE, sep = ",")

AdventureWorks_Sales_2017 = read.csv("AdventureWorks_Sales_2017.csv", header = TRUE, sep = ",")



print(head(AdventureWorks_Sales_2015))
print(head(AdventureWorks_Sales_2016))
print(head(AdventureWorks_Sales_2017))

# import dplyr package
library(dplyr)

# Create a Single Sales Dataframe

Sales_DF_partial = union(AdventureWorks_Sales_2015, AdventureWorks_Sales_2016)
print(head(Sales_DF_partial))

Sales_DF = union(Sales_DF_partial, AdventureWorks_Sales_2017)



# Export Data to Check no Data Has been lost

print(colnames(Sales_DF))

write.csv(Sales_DF,"a:\\Training\\R Programming\\Adventure Works Data\\Sales_DF.csv")

write.csv(Sales_DF,"a:\\Training\\R Programming\\Adventure Works Data\\Sales_DF.csv", row.names = FALSE)





# Import Dimensional Data - AdventureWorks_Products, AdventureWorks_Product_Subcategories,
# AdventureWorks_Product_Categories,  AdventureWorks_Customers, AdventureWorks_Territories

# Products Data
Products_DF = read.csv("AdventureWorks_Products.csv", header = TRUE, sep = ",")

# Product Sub Category - Hierarchy 1
Sub_Cat_Products_DF = read.csv("AdventureWorks_Product_Subcategories.csv", header = TRUE, sep = ",")

# Product Category - Hierarchy 2
Cat_Products_DF = read.csv("AdventureWorks_Product_Categories.csv", header = TRUE, sep = ",")

# AdventureWorks_Customers
Customers_DF = read.csv("AdventureWorks_Customers.csv", header = TRUE, sep = ",")

# AdventureWorks_Territories
Territories_DF = read.csv("AdventureWorks_Territories.csv", header = TRUE, sep = ",")








# Add Product Attributes to the Dataframe
Sales_Products = merge(Sales_DF,Products_DF,by.Sales_DF = "ProductKey", by.Products_DF = "ProductKey")

# Add Product Sub Category Attrbutes to the Dataframe
Sales_Product_Sub_Cat = merge(Sales_Products, Sub_Cat_Products_DF, by.Sales_Products = "ProductSubcategoryKey", by.Sub_Cat_Products_DF = "ProductSubcategoryKey" )

# Add Prouct Category Attributes to the Dataframe
Sales_Product_Cat = merge(Sales_Product_Sub_Cat, Cat_Products_DF, by.Sales_Product_Sub_Cat = "ProductCategoryKey", by.Cat_Products_DF =  "ProductCategoryKey"  )

# Add Customer Attributes to the DataFrame
Sales_Customer = merge(Sales_Product_Cat, Customers_DF, by.Sales_Product_Cat = "CustomerKey", by.Customers_DF = "CustomerKey")

# Add Territory Attributes to the DataFrame
Sales = merge(Sales_Customer, Territories_DF, by.Sales_Customer = "TerritoryKey", by.Territories_DF = "SalesTerritoryKey")

# Check if we have received all the columns
print(colnames(Sales))

# Calculate Sales Amount
Sales$SalesAmount = Sales$OrderQuantity * Sales$ProductPrice


print(colnames(Sales))

# Check Data
head(Sales)





# Dump Data
write.csv(Sales,"a:\\Training\\R Programming\\Adventure Works Data\\Sales.csv", row.names = FALSE)





# Some Single Column Aggregations

country_wise_sales = aggregate( SalesAmount ~ Country, Sales, FUN = sum )
print(country_wise_sales)







gender_wise_sales = aggregate(SalesAmount~Gender, Sales, FUN = sum )
product_category_wise_sales = aggregate(SalesAmount~CategoryName, Sales, FUN = sum)

print(country_wise_sales)
print(gender_wise_sales)
print(product_category_wise_sales)




# Multiple Column Aggregations

product_Cat_Wise_Gender_Wise_Sales = aggregate(SalesAmount~Gender+CategoryName, Sales, FUN = sum)
print(product_Cat_Wise_Gender_Wise_Sales)






# Rbind and Cbind and why we don't use them anymore??

x1 = c(1,2,3)
x2 = c(5,6,7)
c_bind = cbind(x1,x2)
print(c_bind)
r_bind = rbind(x1,x2)
print(r_bind)




print(product_Cat_Wise_Gender_Wise_Sales)

# Creating Pivots

pivot_gender = reshape(product_Cat_Wise_Gender_Wise_Sales, 
                       direction = "wide", idvar = "CategoryName", timevar = "Gender")
print(pivot_gender)

#charts in r 

#install.packages("ggplot2")
#install.packages("ggalt")

# Change Working Directory to point to Sales Data Directory

getwd()
setwd("a:\\Training\\R Programming\\Adventure Works Data")
getwd()


# Import Adventure Works Sales Data Set

Sales = read.csv("Sales.csv", header = TRUE, sep = ",")
print(head(Sales))

colnames(Sales)




# Calculate Sales, Profit, Sales Per Unit Sold
# Caluclated Columns / Derived Columns / Measures

# Sales Amount
Sales$SalesAmount = Sales$OrderQuantity * Sales$ProductPrice

# Profit

Sales$Profit = Sales$OrderQuantity * (Sales$ProductPrice - Sales$ProductCost)

# Profit Per Unit Sold

Sales$Profit_Per_Unit = Sales$Profit / Sales$OrderQuantity

# Sales Return

Sales$SalesReturn = Sales$Profit *100 / Sales$SalesAmount


print(head(Sales))

# Calculate Dimensions -> Month-Year, Salary Bracket

#Year

Sales$Year = substring(Sales$OrderDate,7,length(Sales$OrderDate))
print(Sales)

# Month - Year

Sales$Month_Year = paste(substring(Sales$OrderDate,7,length(Sales$OrderDate)),
                         substring(Sales$OrderDate,4,5), sep = "-")
print(Sales)

# Salary Bracket

# Clean the String
Sales$SalaryBracket = gsub(" ", "", substring(Sales$AnnualIncome,2,length(Sales$AnnualIncome) ))

print(head(Sales))


# Convert string to numeric
Sales$SalaryBracket = as.integer(Sales$SalaryBracket)
print(head(Sales))
# Add if else condition

Sales$SalaryBracket = ifelse( ((Sales$SalaryBracket>=10000) & (Sales$SalaryBracket<=40000)), "Low Income Bracket", ifelse( ((Sales$SalaryBracket>40000) & (Sales$SalaryBracket<=110000)), "Mid Income Bracket", "High Income Bracket"  ) )

print(head(Sales))
# Create Aggregated Tables to be consumed into Visuals

# Graphical Library - ggplot2

library("ggplot2")
library("ggalt")
# Bar Chart


# Create Aggregated Data Set - Product Category wise Sales

product_cat_wise_sales = aggregate(SalesAmount~CategoryName, Sales, FUN = sum)
print(product_cat_wise_sales)


# Layer 1 - Set Theme
theme_set(theme_grey())

# Layer 2 - Add Data 
bar_chart = 
  ggplot(product_cat_wise_sales, aes(x=CategoryName, y=SalesAmount)) + 
  
  # Layer 3 - Type of Visualization
  geom_bar(stat="identity", width=0.5, fill='purple') + 
  
  # Layer 4 - Chart Labels
  
  labs(title="Bar Chart", 
       subtitle="Sales per Product Category", 
       caption="Data Source - Adventure Works") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

print(bar_chart)
ggsave("bar_chart.png")


# Draw Line Chart


# Create Aggregated Data Set - Month-Year wise Sales

month_year_wise_sales = aggregate(SalesAmount~Month_Year, Sales, FUN = sum)
print(month_year_wise_sales)


# Layer 1 - Set Theme
theme_set(theme_dark())

# Layer 2 - Add Data 
line_chart = 
  ggplot(month_year_wise_sales, aes(x=Month_Year, y= SalesAmount, group = 1)) + 
  
  # Layer 3 - Type of Visualization
  geom_line() + 
  geom_point() +
  
  # Layer 4 - Chart Labels
  
  labs(title="Line Chart - Time Series (Basic)", 
       subtitle="Sales over Time", 
       caption="Data Source - Adventure Works")

print(line_chart)
ggsave("line_chart.png")



# Pie - Income Bracket by sales

# Create Aggregated Data Set - Income Bracket wise Sales

income_bracket_wise_sales = aggregate(SalesAmount~SalaryBracket, Sales, FUN = sum)
print(income_bracket_wise_sales)

colnames(income_bracket_wise_sales) <- c("class", "freq")
print(income_bracket_wise_sales)

pie_chart = ggplot(income_bracket_wise_sales, aes(x = "", y=freq, fill = factor(class))) + 
  
  geom_bar(width = 1, stat = "identity") +
  
  theme(axis.line = element_blank(), 
        
        plot.title = element_text(hjust=0.5)) + 
  
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie_chart = pie_chart + coord_polar(theta = "y", start=0)
print(pie_chart)
ggsave("pie_chart.png")



# Dumbbell Charts - Income Bracket by sales
# Compare % Sales Returns over the Years: 2016 and 2017 for Product Sub Categories

year_wise_sales_returns = aggregate(SalesReturn~Year+SubcategoryName, Sales, FUN = mean)
print(year_wise_sales_returns)
year_wise_sales_returns = year_wise_sales_returns[((year_wise_sales_returns$Year == 2016) |
                                                     (year_wise_sales_returns$Year == 2017)),]
print(year_wise_sales_returns)


pivot_year = reshape(year_wise_sales_returns, direction = "wide", idvar = "SubcategoryName", timevar = "Year")
print(pivot_year)
colnames(pivot_year)[colnames(pivot_year) == "SalesReturn.2016"] = "FY2016"
colnames(pivot_year)[colnames(pivot_year) == "SalesReturn.2017"] = "FY2017"
print(pivot_year)




dumbbell_plot = 
  ggplot(pivot_year, aes(y=SubcategoryName, x=FY2016, xend=FY2017)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x="Sales Return %", y="Product SubCategory", title="Dumbbell Plot to Determine Yearly Shift") +
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))

print(dumbbell_plot)
ggsave("dumbbell_plot.png")


# Histogram - Stacked Chart
#  Salary Bracket wise Gender wise Sales


salary_bracket_wise_gender_wise_sales = aggregate(SalesAmount~Gender+SalaryBracket, Sales, FUN = sum)
print(salary_bracket_wise_gender_wise_sales)


theme_set(theme_classic())


stacked_histogram =  ggplot(salary_bracket_wise_gender_wise_sales, aes(reorder(SalaryBracket, -SalesAmount, sum), SalesAmount, fill = Gender)) +
  geom_col() +
  geom_text(
    aes(label = after_stat(y), group = SalaryBracket), 
    stat = 'summary', fun = sum, vjust = -1
  )

print(stacked_histogram)
ggsave("stacked_histogram.png")

