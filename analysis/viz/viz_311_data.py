#! /usr/bin/env python

""" Set of functions for visualizing 311 request data from the city data portal.
"""
import json
import matplotlib.pyplot as plt
from itertools import groupby, izip
from datetime import date
import numpy as np
import sys
import csv
import pickle
import scipy

__author__ = "Alessandro Panella (apanel2@uic.edu)"

def plot_monthly_requests(request_type, save_fig=True, \
  data_folder='/mnt/data1/Indices/portal_311', filter_same_day=False):
  """ Plot the 311 data of a particular aggregated by month and day of the
  week, starting in January 2011. """

  # Useful lists
  month_names = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
    'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
  dayofweeks_names = ['MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN']

  # Open the json data file
  data_file = '311-' + request_type + '.json'
  f =  open(data_folder + '/' + data_file, 'r')

  # Read in the json database (returns a dictionary)
  req_data = json.load(f)
  f.close()

  # Retrieve the creation and completion date, and SRN column indexes
  columns = [e['fieldName'] for e in req_data['meta']['view']['columns']]
  cr_col = columns.index('creation_date')
  co_col = columns.index('completion_date')
  srn_col = columns.index('service_request_number')

  # Create a cuter data structure from the data
  data = []
  for e in req_data['data'][:-1][::-1]:
    cr = e[cr_col]
    co = e[co_col]
    cr_date = date(int(cr[0:4]), int(cr[5:7]), int(cr[8:10]))
    co_date = date(int(co[0:4]), int(co[5:7]), int(co[8:10]))\
      if co != None else None
    if cr_date.year > 2010:
      data.append({
        'srs': e[srn_col],
        'cr_date': cr_date,
        'co_date': co_date
      })
  # If needed, filter out the city-issued requests
  if filter_same_day:
    data = [e for e in data if e['co_date'] != None and e['cr_date'] < e['co_date']]
  # Retrieve the year-month-day creation and completion dates as a string
  #crea_dates_s = [e[cr_col] for e in req_data['data'][:-1]\
  #  if int(e[co_col][0:4]) > 2010 and e[co_col] != None]
  #comp_dates_s = [e[completed_col] for e in req_data['data'][:-1]\
  #  if int(e[cr_col][0:4]) > 2010 and e[co_col] != None]

  # Aggregate data by month
  # Convert year-month to integers and reverse the order (lower to higher)
  cr_dates_i = [int('{0}{1:02d}'.format(e['cr_date'].year, e['cr_date'].month)) for\
    e in data]

  # Count the number of reports per month, filtering out the city-issued
  # requests, if needed
  counts = [(k, len(list(g))) for (k, g) in groupby(cr_dates_i)]

  # Plot the counts
  plt.figure()
  # Create the shading rectangles for readabiliy
  for i in range(1, len(counts), 2):
    plt.axvspan(i-0.5, i+0.5, color='#DDDDDD', alpha=0.7)
  plt.plot([e[1] for e in counts], 'o-', linewidth=2, color='#3399CC')
  # Lambda function that returns "motnh labels" (the year is attached if
  # the month is January
  m_label = lambda ym: str(ym/100) + ' ' + month_names[ym%100-1]\
    if ym%100 == 1 else month_names[ym%100-1]
  month_labels = [m_label(e[0]) for e in counts] 
  plt.xticks(range(len(counts)), month_labels, rotation='vertical')
  plt.xlim(-0.5, len(counts)-0.5)
  if save_fig:
    plt.savefig('../plots/' + request_type + '-monthly.png')

  # Aggregate data by weekday
  weekdays_i = [e['cr_date'].weekday() for e in data\
    if e['co_date'] is None or e['cr_date'] < e['co_date']]

  # Plot 
  plt.figure()
  plt.hist(weekdays_i, range(8), rwidth=0.7, color='#3399CC')
  plt.xticks(np.array(range(8))+0.5, dayofweeks_names)
  if save_fig:
    plt.savefig('../plots/' + request_type + '-dayofweek.png')
  
  plt.show()

  return


def plot_vs_income_by_area(data_folder='/mnt/data1/Indices/portal_311'):
  """ Plot the amount of 311 requests (normalized by population count) for each
  community area, and each type of request. The x-axis correspond to the median
  household income """

  # Open the 311 call data file
  data_file = 'comm-area-call-volume.csv'
  f =  open(data_folder + '/' + data_file, 'r')

  # Read in the csv file
  calls_csv_reader = csv.reader(f, delimiter=',')
  # Create a data structure with the info (dictionary)
  calls_type_area = {}
  calls_csv_reader.next() # Consume headers
  for row in calls_csv_reader:
    calls_type_area[row[0]] = [float(e) for e in row[1:]]
  f.close()

  # Open and read the area info file
  f = open(data_folder + '/chicago-community-areas.csv', 'r')
  f.readline()
  areas_info = {}
  l = f.readline().split(',')
  areas_info['names'] = l[1:]
  l = f.readline().split(',')
  areas_info['population'] = [int(e) for e in l[1:]]
  l = f.readline().split(',')
  areas_info['income'] = [int(e) for e in l[1:]]

  # Plot
  for (i, k) in enumerate(calls_type_area.keys()):
    plt.subplot(4, 3, i)
    plt.scatter(areas_info['income'], calls_type_area[k])
    plt.title(k)

  plt.suptitle("Requests per 10,000 citizen vs. median income")
  #plt.show()


def plot_pothole_locations(year, daily=True, data_folder='/mnt/data1/Indices/portal_311'):
  """ Plot pothole locations in the specified year.
  If the argument "daily" is set to True, then generate a snapshot for each day
  of the year, with a red dot representing  an open pothole, and a blue one
  representing a pothole filled in the past 5 days. """

  # Only have data for 2011-2013
  if year not in [2011, 2012, 2013]:
    print " No data for " + year
    return

  # Open the json data file
  data_file = '311-potholes.json'
  f =  open(data_folder + '/' + data_file, 'r')

  # Read in the json database (returns a dictionary)
  req_data = json.load(f)
  f.close()

  # Eliminate duplicates (only consider "completed" and "open" requests)
  # Save the X-Y coordinates, and for each day, the index of the requests
  n_days = 365 if year == 2011 else 366
  open_requests_on_day = [[] for i in xrange(n_days)]
  closed_requests_on_day = [[] for i in xrange(n_days)]
  x_coords = []
  y_coords = []
  dates = []

  # Retrive the data ordered by date
  data = req_data['data'][::-1][1:]
  for (idx, e) in enumerate([ee for ee in data if year-1 < int(ee[8][0:4]) <
    year+1]):
    if idx == 0:
      prev_date = e[8]
      dates.append(prev_date)
    if e[9] in ['Open', 'Completed'] and e[10] is not None:
      date_opened = datetime.date(int(e[8][0:4]), int(e[8][5:7]), int(e[8][8:10]))
      date_closed = datetime.date(int(e[10][0:4]), int(e[10][5:7]), int(e[10][8:10]))
      if date_opened.timetuple().tm_yday-1 <= date_closed.timetuple().tm_yday-1:
        open_requests_on_day[date_opened.timetuple().tm_yday-1].append(idx)
        closed_requests_on_day[date_closed.timetuple().tm_yday-1].append(idx)
    x_coords.append(float(e[18]) if e[18] != None else x_coords[-1])
    y_coords.append(float(e[19]) if e[18] != None else y_coords[-1])
    if e[8] != prev_date:
      dates.append(e[8])
    prev_date = e[8]
    
  # Generate PNG images for every day, if required
  if daily:
    plt.figure(figsize=(8,10))
    min_x = min(x_coords) - 300
    max_x = max(x_coords) + 300
    min_y = min(y_coords) - 300
    max_y = max(y_coords) + 300
    opened = []
    fixed = []
    for i in xrange(n_days):
      opened += [ii for ii in open_requests_on_day[i]]
      for ii in closed_requests_on_day[i]:
        opened.remove(ii)
      fixed += closed_requests_on_day[i]
      if i > 5:
        for ii in closed_requests_on_day[i-5]:
          fixed.remove(ii)
      x_open = [x_coords[ii] for ii in opened]
      y_open = [y_coords[ii] for ii in opened]
     
      x_fixed = [x_coords[ii] for ii in fixed]
      y_fixed = [y_coords[ii] for ii in fixed]

      plt.clf()
      plt.scatter(x_open, y_open, color='r', alpha=0.2)
      plt.scatter(x_fixed, y_fixed, color='b', alpha=0.2)
      plt.axis('equal')
      plt.axis('off')
      plt.title(dates[i][0:10])
      plt.xlim(min_x, max_x)
      plt.ylim(min_y, max_y)
      png_name = str(year) + '/' + str(i) + '.png'
      plt.savefig(png_name)

def plot_vs_latinos(request_type, data_folder='/mnt/data1/Indices/portal_311'):
  """ This function plots a type of request against percentage of latinos, for
  any of the 77 community areas """


  # Open the 311 call data file
  data_file = 'comm-area-call-volume.csv'
  f =  open(data_folder + '/' + data_file, 'r')

  # Read in the csv file
  calls_csv_reader = csv.reader(f, delimiter=',')
  # Create a data structure with the info (dictionary)
  calls_type_area = {}
  calls_csv_reader.next() # Consume headers
  for row in calls_csv_reader:
    calls_type_area[row[0]] = [float(e) for e in row[1:]]
  f.close()

  # Open and read the area info file
  f = open(data_folder + '/chicago-community-areas.csv', 'r')
  f.readline()
  areas_info = {}
  l = f.readline().split(',')
  areas_info['names'] = l[1:]
  l = f.readline().split(',')
  areas_info['population'] = [int(e) for e in l[1:]]
  l = f.readline().split(',')
  areas_info['income'] = [int(e) for e in l[1:]]
  l = f.readline().split(',')
  areas_info['tot_calls'] = [float(e) for e in l[1:]]
  l = f.readline().split(',')
  areas_info['latinos'] = [float(e) for e in l[1:]]
  f.close()

  plt.figure()
  plt.scatter(areas_info['latinos'], calls_type_area[request_type], \
    s=[(float(e)/3000.0)**2 for e in areas_info['income']], alpha=0.6)

  plt.show()


def generate_request_histograms(data_folder='/mnt/data1/Indices/portal_311'):
  data = pickle.load(open(data_folder + '/dat.pkl'))
  data = scipy.delete(data, 2, 1)
  data = scipy.delete(data, 0, 1)
  #now we have our data!

  f = open(data_folder + '/request_types.csv', 'r')
  headers = f.readline().split(',')
  headers = headers[3:]

  new_data = data[:, 1:]

def generate_request_histograms():
  data = pickle.load(open("../data/dat.pkl"))
  data = scipy.delete(data, 2, 1)
  data = scipy.delete(data, 0, 1)
  #now we have our data!

  f = open('../data/request_types.pkl', 'r')
  headers = f.readline().split(',')
  headers = headers[3:]

  new_data = data[:, 1:]

  for i in xrange(len(headers)-1):
    # Generate the histograms
    plt.cla()
    plt.hist(new_data[:,i], bins = 300)
    plt.title(headers[i])
    filename = "../plots/hist_by_type/" + str(i+1) + ".png"
    plt.savefig(filename)

