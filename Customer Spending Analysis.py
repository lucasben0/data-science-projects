#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 26 09:47:44 2024

@author: lucasben
"""
### PREPARING NECESSARY LIBRARIES ###
import pandas as pd # data processing
import os
for dirname, _, filenames in os.walk('/kaggle/input') :
    for filename in filenames:
        print(os.path.join(dirname, filename))
  
os.chdir('/Users/lucasben/Desktop/working directory') # set directory

df = pd.read_csv("/Users/lucasben/Desktop/working directory/SalesForCourse_quizz_table.csv")
df.head()
### UNDERSTANDING THE DATASET ###
df.shape #understanding the shape of the table
print(f'Number of rows: {df.shape[0]} \nNumber of columns: {df.shape[1]}')
df.dtypes # check data types of each columns
df.info() # summary of dataframe
df.columns # column names
# check for unique values among categorical columns
for column in ['Country', 'Product Category', 'Sub Category']:
    value = df[column].dropna().unique()
    print(f'{value.size} unique vales in {column}:\n', value, '\n')
# there are several issues 1) incompleteness of data 
# 2) a column with no name 'Column 1' 3) wrong 'Date' type of data
### CLEAN & TRANSFORM DATA ###
df['Date'] = pd.to_datetime(df['Date']) # convert 'Date' from object to datetime in Pandas
# filters df to include only the rows where 'Column1' does not have missing values
df[df['Column1'].notna()] #clean/transform 'Column1'
print(f'''{df['Column1'].notna().sum()} cells contain values and {df['Column1'].isna().sum()} cells are null over a total of {df['Column1'].size} cells''')
df.drop('Column1', axis = 1, inplace = True) # removing 'Column1' 
# creating two new columns 'Unit Margin' and 'Profit'
df['Unit Margin'] = df['Unit Price'] - df['Unit Cost']
df['Profit'] = df['Revenue'] - df['Cost']
df.head()
### EXPLORATORY DATA ANALYSIS ###
df.describe(include = 'all') # summary for numerical and categorical data
import matplotlib.pyplot as plt
# nationality breakdown
lst_feat = ['Country', 'Quantity', 'Cost', 'Revenue', 'Unit Margin', 'Profit']
country = df.loc[:,lst_feat].groupby('Country').sum('Profit').sort_values('Profit', ascending = False)
country
# bar graph
ax = country.plot(kind = 'bar', figsize = (10,5))
ax.set_ylabel('Amount in USD ($10,000,000')
ax.set_title('Sales and Revenue in 4 Countries')
plt.show()
# The US generates more than double the revenue of each country yet it is second in profit behind Germany
# how much is the profit/loss of each product in total?
product = df.groupby(['Sub Category'], as_index = False).sum(numeric_only = True) # grouping and aggregating data
product = product[['Sub Category', 'Cost', 'Revenue', 'Profit']].set_index('Sub Category').sort_values('Profit') # filtering and organizing columns
product # df where rows are ordered based on the profitability of each 'Sub Category' from lowest to highest profit
# horizontal bar graph
product.plot(kind = 'barh', figsize = (10,6))
plt.title('Cost - Revenue - Profit across different product')
plt.xlabel('Amount in USD ($1,000,000')
plt.show()
# how much is the profit/loss of each product by country?
product_div = df.groupby(['Country', 'Product Category', 'Sub Category'], as_index = False).sum(numeric_only = True)
product_div = product_div.loc[:, ['Country', 'Product Category', 'Sub Category', 'Quantity', 'Cost', 'Revenue', 'Unit Margin', 'Profit']].sort_values(['Country', 'Product Category', 'Sub Category'])
product_div
# four unique horizontal bar graphs
fig, axs = plt.subplots(2,2,figsize=(12,8))
countries = product_div['Country'].dropna().unique()
for i, country in enumerate(countries):
    country_data = product_div[product_div['Country'] == country].loc[:,['Sub Category', 'Profit']].sort_values('Profit')
    axs[i // 2, i % 2].barh(country_data['Sub Category'], country_data['Profit'], color=country_data['Profit'].apply(lambda x: 'grey' if x >= 0 else 'red'))
    axs[i // 2, i % 2].set_title(country)
    product_div_sorted = product_div[product_div['Country'] == country].sort_values('Profit', ascending=False).reset_index()
    product_div_sorted['Cumulative Profit %'] = product_div_sorted['Profit'].cumsum() / product_div_sorted['Profit'].sum()
    axs[i // 2, i % 2].axhline(y=product_div_sorted[product_div_sorted['Cumulative Profit %'] >= 0.8].index[0], color = 'blue', linestyle = '--', linewidth = 2)
fig.suptitle('Bar Charts with 80% Line for Different Countries', fontsize=16)
plt.tight_layout()
plt.show()
# breakdown to country level, US and UK are losing money on every type of bicycle
# time series analysis of profit
time = df.groupby(['Country', 'Date'], as_index = False).sum(numeric_only = True).sort_values('Date')
time = time[['Date', 'Country', 'Profit']].set_index('Date')
fig, ax = plt.subplots(figsize=(20,6))
grouped_data = time.groupby('Country')
for country, group in grouped_data:
    ax.plot(group.index, group['Profit'], label=country)
ax.legend()
fig.suptitle('Profit generated from different country by day', fontsize = 16)
plt.show()
grouped_data = time.groupby(['Country', time.index.to_period('M')]).sum()
# demographic
df[['Country','Customer Age']].groupby('Country').mean() # age
df[['Country', 'index']].groupby('Country').count() # number of sales
df[['Country', 'Customer Gender', 'index']].groupby(['Country', 'Customer Gender']).count() # distribution of genders


















