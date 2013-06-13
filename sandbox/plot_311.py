### PLOT DATA FROM 311 DATABASES ON A TEMPORAL SCALE ###

import json
import matplotlib.pyplot as plt
from itertools import groupby

# Open the json data file
data_file = '311-potholes.json'
f =  open('Data/'+data_file, 'r')

# Read in the json database (returns a dictionary)
req_data = json.load(f)

# Retrive the year-month as a string
dates_s = [e[8] for e in req_data['data'][:-1]]
# Convert to integers and reverse the order (lower to higher)
dates_i = [int(e[0:4]+e[5:7]) for e in dates_s][::-1]

# Count the number of reports per month
counts = [len(list(g)) for (k, g) in groupby(dates_i)]

# Plot the counts
plt.figure()
plt.plot(counts)
plt.show()


