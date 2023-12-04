import numpy as np
import pandas as pd
import json
import csv
import statistics

#business.json
# Select categories including bars and nightlife in Nashville
with open('business_bars_nightlife_nashville.json', 'w', encoding='utf-8') as output_file:
    for line in open('business.json', 'r', encoding='utf-8'):
        data = json.loads(line)
        # Check if "categories" and "city" exist and are not None
        if 'categories' in data and data['categories'] is not None and 'city' in data and data['city'] is not None:
            categories = data['categories']
            city = data['city'].lower()  # Convert city name to lowercase
            # Check if "Nightlife" or "bars" are in categories and city is "nashville"
            if ('Nightlife' in categories or 'bars' in categories) and city == 'nashville':
                # Write the data that meets the conditions to the output file
                output_file.write(json.dumps(data) + '\n')

state_counts = {}
with open('business_bars_nightlife.json', 'r',encoding='utf-8') as json_file:
    for line in json_file:
        data = json.loads(line)
        state = data.get('state', 'Unknown')
        state_counts[state] = state_counts.get(state, 0) + 1
with open('business_bars_nightlife_state_counts.csv', 'w', newline='') as csv_file:
    writer = csv.writer(csv_file)
    
    writer.writerow(['State', 'Store Count'])
    
    for state, count in state_counts.items():
        writer.writerow([state, count])
      
#calculate total data number in business.json
data_count = 0
with open('business.json', 'r', encoding='utf-8') as json_file:
    for line in json_file:
        data = json.loads(line)
        data_count += 1
print("business.json include %d data。" % data_count)

# calculate median and mean of "stars" in each state
stars_by_state = {}
with open('business_bars_nightlife.json', 'r', encoding='utf-8') as json_file:
    for line in json_file:
        data = json.loads(line)
        state = data['state']
        stars = data['stars']
        
        if state not in stars_by_state:
            stars_by_state[state] = []
        
        stars_by_state[state].append(stars)

with open('stars_median_by_state.csv', 'w', newline='') as csv_file:
    csv_writer = csv.writer(csv_file)
    csv_writer.writerow(['State', 'Median Stars'])
    for state, stars_list in stars_by_state.items():
        median_stars = statistics.median(stars_list)
        csv_writer.writerow([state, median_stars])
with open('stars_mean_by_state.csv', 'w', newline='') as csv_file:
    csv_writer = csv.writer(csv_file)
    csv_writer.writerow(['State', 'Mean Stars'])
    for state, stars_list in stars_by_state.items():
        mean_stars = statistics.mean(stars_list)
        csv_writer.writerow([state, mean_stars])
for state, stars_list in stars_by_state.items():
    mean_stars = statistics.mean(stars_list)
    print(f"State {state}  'stars' mean is {mean_stars}")

with open('business_bars_nightlife_nashville.json', 'r', encoding='utf-8') as json_file:
    for line in json_file:
        data = json.loads(line)
        city = data['city']
        postal_code = data['postal_code']
        review_count = data['review_count']
        stars = data['stars']

        if (city, postal_code) not in data_dict:
            data_dict[(city, postal_code)] = {
                'count': 0,
                'review_count': 0,
                'stars': []
            }
        
        data_dict[(city, postal_code)]['count'] += 1
        data_dict[(city, postal_code)]['review_count'] += review_count
        data_dict[(city, postal_code)]['stars'].append(stars)

with open('business_nashcille_description.csv', 'w', newline='',encoding='utf-8') as csv_file:
    csv_writer = csv.writer(csv_file)
    csv_writer.writerow(['City', 'Postal_code', 'Count', 'Review Count', 'Stars Median', 'Stars Mean', 'Stars Variance'])
    for (city, postal_code), data in data_dict.items():
        count = data['count']
        review_count = data['review_count']
        stars = data['stars']
        stars_median = statistics.median(stars)
        stars_mean = statistics.mean(stars)
        stars_variance = statistics.variance(stars) if len(stars) > 1 else 0
        csv_writer.writerow([city, postal_code, count, review_count, stars_median, stars_mean, stars_variance])
print("write the result to business_nashcille_description.csv ")

#get data_census
# Read all lines in JSON file
with open('business_bars_nightlife_nashville.json', 'r', encoding='utf-8') as file:
    lines = file.readlines()
data_list = []
for line in lines:
    try:
        data_entry = json.loads(line)
        data_list.append(data_entry)
    except json.JSONDecodeError as e:
        print(f"JSONDecodeError: {e}")
      
postal_code_data = {}

# Traverse the JSON data and calculate the star sum and count for each zip code
for entry in data_list:
    postal_code = entry['postal_code']
    stars = entry['stars'] 
    if postal_code in postal_code_data:
        postal_code_data[postal_code]['sum_stars'] += stars
        postal_code_data[postal_code]['count'] += 1
    else:
        postal_code_data[postal_code] = {'sum_stars': stars, 'count': 1}
result_data = []

# Calculate the average star rating for each zip code and add the results to the results list
for postal_code, values in postal_code_data.items():
    average_stars = values['sum_stars'] / values['count']
    result_data.append({'postal_code': postal_code, 'average_stars': average_stars})
df = pd.DataFrame(result_data)
df.to_csv('average_stars_by_postal_code.csv', index=False)

# read"average_stars_by_postal_code.csv"and"census_postalcodes.csv"
average_stars_df = pd.read_csv('average_stars_by_postal_code.csv')
census_df = pd.read_csv('census_postalcodes.csv')
merged_df = average_stars_df.merge(census_df, on='postal_code', how='inner')
merged_df.to_csv('data_census.csv', index=False)
