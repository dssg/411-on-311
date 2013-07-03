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


def plot_clusters(clusterer):
  # Load headers
  with open('../data/request_types.csv', 'r') as headers_f:
    h_reader = csv.reader(headers_f, delimiter=',', quotechar='"')
    headers = h_reader.next()[3:]
  
  plt.figure()
  k = clusterer.n_clusters
  centers = clusterer.cluster_centers_
  max_y = centers.max() + 50
  out_f = open('../data/top_10_6_clusters.txt', 'w')
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
    for ii in xrange(10):
      out_f.write(str(sorted_idx_val[ii][0]) + ', ' +\
                  str(headers[sorted_idx_val[ii][0]]) + '\n' +\
                  str(sorted_idx_val[ii][1]) + '\n')

    plt.subplot(k/2+np.mod(k,2), 2, i+1)
    plt.plot(centers[i])
    plt.ylim([0, max_y])

  plt.show()
 
  out_f.close()
