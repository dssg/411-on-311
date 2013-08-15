import numpy as np
import scipy
import cPickle as pickle
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from time import time
from sklearn import metrics
from sklearn import preprocessing as pp
#data = np.genfromtxt("CallTypeBeTractPerThousandResidents.csv", delimiter = ",")
#pickle.dump(data, open("dat.pkl", 'wb'))
#removed 409 and  794 by hand

def bench_k_means(estimator, name, data, labels):
  t0 = time()
  sample_size = len(data)
  estimator.fit(data)
  print '% 9s   %.2fs    %i   %.3f   %.3f   %.3f   %.3f   %.3f    %.3f' % (
    name, (time() - t0), estimator.inertia_,
    metrics.homogeneity_score(labels, estimator.labels_),
    metrics.completeness_score(labels, estimator.labels_),
    metrics.v_measure_score(labels, estimator.labels_),
    metrics.adjusted_rand_score(labels, estimator.labels_),
    metrics.adjusted_mutual_info_score(labels,  estimator.labels_),
    metrics.silhouette_score(data, estimator.labels_,
                             metric='euclidean',
                             sample_size=sample_size),)


if __name__ == "__main__":
  data = pickle.load(open("../../data/dat.pkl"))
  print data.shape
  data = scipy.delete(data, 2, 1)
  data = scipy.delete(data, 0, 1)
  #now we have our data!

  #pca = PCA(n_components = 2)
  #new_data = pca.fit_transform(data[:, 1:])
  labels = data[:, 0]
  new_data = data[:, 1:]
  print labels
  # Scale the data
  #scaler =  pp.StandardScaler()
  #new_data = scaler.fit_transform(new_data)
  
  if False:
     print(79 * '_')
     print('% 9s' % 'init'
           '    time  inertia    homo   compl  v-meas     ARI AMI  silhouette')

     bench_k_means(KMeans(init='k-means++', n_clusters=2, n_init=10),
                   name="2 clusters", data=new_data, labels = labels)
     bench_k_means(KMeans(init='k-means++', n_clusters=5, n_init=10),
                   name="5 clusters", data=new_data, labels = labels)
     bench_k_means(KMeans(init='k-means++', n_clusters=10, n_init=10),
                   name="10 clusters", data=new_data, labels = labels)

  n_clusters = range(2,15)
  n_clusters = [6]
  performance = []
  for k in n_clusters:
    clusterer = KMeans(init="k-means++", n_clusters = k, n_init = 10)
    clusterer.fit(new_data)
    preds = clusterer.predict(new_data)
    performance.append(clusterer.inertia_)
    #for c in range(n_clusters):
    #  print "\nCluster %d:" %c
    #  cluster = labels[preds == c]
    #  for label in cluster:
    #    print "\t", label 
    
    
