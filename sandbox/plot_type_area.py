# This script plots the number of 311 request on a temporal scale,
# aggregated by month and day of the week.

import matplotlib.pyplot as plt
from itertools import groupby
import datetime
import numpy as np
import csv

# Useful lists
month_names = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
  'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
dayofweeks_names = ['MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN']

# Open the 311 call data file
data_file = 'comm-area-call-volume.csv'
f =  open('../data/'+data_file, 'r')

# Read in the csv file
calls_csv_reader = csv.reader(f, delimiter='\t')
# Create a data structure with the info (dictionary)
calls_type_area = {}
calls_csv_reader.next() # Consume headers
for row in calls_csv_reader:
  calls_type_area[row[0]] = [float(e) for e in row[1:]]
f.close()

# Open and read the area info file
f = open('../data/chicago-community-areas.csv', 'r')
f.readline()
areas_info = {}
l = f.readline().split(',')
areas_info['names'] = l[1:]
l = f.readline().split(',')
areas_info['population'] = [int(e) for e in l[1:]]
l = f.readline().split(',')
areas_info['income'] = [int(e) for e in l[1:]]
l = f.readline().split(',')
areas_info['latinos'] = [float(e) for e in l[1:]]

plt.figure()
titles = [  'Graffiti Removal',
            'Sanitation Code COmplaints',
            'Vacand and Abandoned Buildings',
            'Street Light Out']
titles = ['A', 'B', 'C', 'D']
for (i, k) in enumerate(['graffiti-removal','sanitation-code-complaints',
  'vacant-abandoned-buildings','street-lights-one-out']):
  print (i,k)
  plt.clf()
  #plt.subplot(2, 2, i+1)
  plt.scatter(areas_info['income'], calls_type_area[k])
  plt.ylabel("# Requests / 10,000 ppl")
  plt.xlabel("Median household income")
  plt.title(titles[i], size=30)
  img_name = 'blog_post/' + titles[i] + '-' + k + '.png'
  plt.savefig(img_name)

plt.show()



