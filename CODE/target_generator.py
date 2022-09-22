#!/usr/bin/env python
# coding: utf-8


import numpy as np
import pandas as pd
from pyod.models.combination import average
from pyod.models.combination import maximization
from pyod.models.combination import aom
from sklearn.utils.extmath import weighted_mode


def t_average(A):
    avrg=average(A.to_numpy())
    return avrg


def t_maximization(Y):
    maxi=maximization(Y.to_numpy())
    return maxi


def t_aom(Y, n_bucket):
    t4=aom(Y.to_numpy(), n_buckets=n_bucket)
    return t4


def t_majority_vote(Y, n_classes=2, weights=None):
    scores=Y.to_numpy()
    n_samples, n_estimators = scores.shape[0], scores.shape[1]
    vote_results = np.zeros([n_samples, ])
    weights = np.ones([1, n_estimators])

    for i in range(n_samples):
        vote_results[i] = weighted_mode(scores[i, :], weights)[0][0]

    return vote_results.ravel()