import pandas as pd 
import matplotlib.pyplot as plt
import numpy as np


data = pd.read_csv("/Users/vivekkumar/Desktop/finalproject_ait/foodbank.csv")

print(data.head()) 

null_counts = data.isnull().sum()

print(null_counts)

#cleaning dataset 

Updated_data = data.drop('Virginia City/County Boundaries', axis=1)

#print(Updated_data.head()) 
Updated_datanull_counts = Updated_data.isnull().sum()

Updated_data["Locality"].fillna("miscellaneous")

mean_A = int(Updated_data['FIPS'].mean())  # Calculate the mean and convert to an integer
print(mean_A)
Updated_data['FIPS'] = Updated_data['FIPS'].apply(lambda x: mean_A if pd.isnull(x) else x)  # Replace null values

mean_house = int( Updated_data['Households Served'].mean())
print(mean_house)
Updated_data['Households Served'] = Updated_data['Households Served'].apply(lambda x: mean_house if pd.isnull(x) else x)

mean_individual = int(Updated_data['Individuals Served'].mean())
Updated_data['Individuals Served'] = Updated_data['Individuals Served'].apply(lambda x: mean_individual if pd.isnull(x) else x)

mean_pounds = int(Updated_data['Pounds of Food Distributed'].mean())
Updated_data['Pounds of Food Distributed'] = Updated_data['Pounds of Food Distributed'].apply(lambda x: mean_pounds if pd.isnull(x) else x)

Updated_data['Children Served via non-federal child nutrition programs'] = Updated_data["Children Served via non-federal child nutrition programs"].fillna(0) 

Updated_data['Pounds of food distributed via non-federal child nutrition prog'] = Updated_data["Pounds of food distributed via non-federal child nutrition prog"].fillna(0) 

#null values
Updated_datanull_counts = Updated_data.isnull().sum()
print(Updated_datanull_counts)

print(Updated_data)

# File path to save the updated dataset
updated_file_path = "/Users/vivekkumar/Desktop/updated_foodbank.csv"

# Save the updated dataset as a CSV file
Updated_data.to_csv(updated_file_path, index=False)

#downloading dataset
print("Updated dataset saved at: {updated_file_path}")

# Change row names 
Updated_data = Updated_data.rename(columns={
    '_id': 'Idno',
    'Children Served via non-federal child nutrition programs': 'childserevedNFNP',
    'Pounds of food distributed via non-federal child nutrition prog': 'PoundsfoodNFNP',
    'LAT': 'Latitude',
    'LON': 'Longitude'
})

Updated_data = Updated_data.rename(columns={
    'Households Served': 'HouseholdsServed',
    'Individuals Served': 'IndividualsServed',
    'Pounds of Food Distributed': 'PoundsFoodDistributed',
    'MTW Status': 'MTWStatus'
})

#print(Updated_data)

Updated_datanull_counts = Updated_data.isnull().sum()
print(Updated_datanull_counts)

#File path to save the updated dataset
updated_file_path = "/Users/vivekkumar/Desktop/updated_foodbankk.csv"

# Save the updated dataset as a CSV file
Updated_data.to_csv(updated_file_path, index=False)

#downloading dataset
print("Updated dataset saved at: {updated_file_path}")

#checking duplicates
duplicates = Updated_data.duplicated()

# Print the duplicate rows
print(Updated_data[duplicates])

unique_names = Updated_data['Locality'].unique()
unique_count = Updated_data['Locality'].nunique()
#print(unique_count)

# Assuming you have a DataFrame called 'df'
summary = Updated_data.describe()

