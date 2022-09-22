#!/usr/bin/env python
# coding: utf-8


import numpy as np
from sklearn.metrics.pairwise import euclidean_distances
from scipy.stats import rankdata
import math


def iFairscore(y, X):
    x_pair = euclidean_distances(X, X)
    r = rankdata(y)
    y_mat = np.zeros((len(y),len(y)))
    for i in range(len(y)):
        for j in range(len(y)):
            y_mat[i][j] = abs(r[i] - r[j])
    diff_mat = x_pair - y_mat
    score = sum(sum(abs(diff_mat)))/(len(y)**2)
    return score


def iFScore(y, X):
    XX = X.to_numpy()
    summ=0
    dist = np.zeros(XX.shape[0])
    r = rankdata(y)
    for i in range(XX.shape[0]):
        for j in range(XX.shape[0]):
            d_diff = math.dist(XX[i], XX[j])
            r_diff = abs(r[i] - r[j])
            d_r = abs(d_diff - r_diff)
            summ = summ + d_r
        dist[i] = (summ/(XX.shape[0]))
        summ=0
    ifs=sum(dist)/XX.shape[0]
    return ifs
