import numpy as np
import scipy
import cPickle as pickle
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from time import time
from sklearn import metrics
from sklearn import preprocessing as pp
import csv
from operator import itemgetter
import json

def plot_clusters(clusterer):
  headers = [     'abandoned-vehicles',
                  'vacant-abandoned-buildings',
                  'graffiti-removal',
                  'potholes',
                  'sanitation-code-complaints',
                  'garbage-carts',
                  'rodent-baiting',
                  'street-lights-one-out',
                  'street-lights-all-out',
                  'alley-lights-out',  
                  'tree-trims',
                  'tree-debris'
                  ]
  plt.figure()
  k = clusterer.n_clusters
  centers = clusterer.cluster_centers_
  max_y = centers.max() + 0.1
  out_f = open('../../data/6_clusters.txt', 'w')
  for i in range(k):
    idx_val = [(ii, count) for (ii, count) in enumerate(centers[i])]
    sorted_idx_val = sorted(idx_val, key=itemgetter(1), reverse=True)
    #sorted_idx_val = sorted([(i, count) for (i, count) in enumerate(centers[i])],\
    #  key=itemgetter(1))
    #print sorted_idx_val
    out_f.write("\nCLUSTER " + str(i) + '\n')
    #for ii in xrange(centers[i].shape[0]):
    #  if centers[i,ii] > 100:
    #    print ii, headers[ii], centers[i,ii]
    out_f.write("Mean overall number of calls: " + str(sum(centers[i])) + '\n')
    for ii in xrange(len(headers)):
      out_f.write(str(sorted_idx_val[ii][0]) + ', ' +\
                  str(headers[sorted_idx_val[ii][0]]) + '\n' +\
                  str(sorted_idx_val[ii][1]) + '\n')

    plt.subplot(k/2+np.mod(k,2), 2, i+1)
    plt.bar(range(len(headers)), centers[i])
    plt.xticks(range(len(headers)), headers, rotation=30)
    plt.ylim([-max_y, max_y])

  plt.show()
 
  out_f.close()


def add_clusters_to_tracts(clusterer, data_folder='../data'):
    clusters = clusterer.labels_

    # Open the geojson file
    geojson_in = open(data_folder + '/requests_by_tract.geojson', 'r')
    tracts_geojson = json.load(geojson_in)
    geojson_in.close()

    # For each record in the geojson file, add the cluster information
    for i, r in enumerate(tracts_geojson['features']):
        r['properties']['cluster'] = str(clusters[i])

    geojson_out = open(data_folder + '/clustered_tracts.geojson', 'w')
    geojson_out.write(json.dumps(tracts_geojson));
    geojson_out.close()

