#!/usr/bin/env python
# coding: utf-8



import numpy as np
import pandas as pd
from individual_fairness import iFairscore
from individual_fairness import iFScore
from target_generator import t_average
from scipy.stats import rankdata
from pyod.utils.data import evaluate_print



class iFselect:
    
    def __init__(self):
        self.selected = None
        self.score_set = None
        self.f_score = None
        self.if_ensemble = None
        self.members = None
        self.T = None
    
    def score_to_rank(self, Y):
        ra = np.empty(shape=Y.shape, dtype='int')
        for i in range(Y.shape[0]):
            ra[i]=rankdata(Y[i])
        r=ra.transpose()
        return r
    
    def main_function(self, Y, X):
        y_updated = Y
        selected = []
        r = self.score_to_rank(Y)
        self.score_set = pd.DataFrame()
        if_result = np.zeros(r.shape[1])
        members = self.members

        #for first selection
        for j in range(Y.shape[0]):
            if_result[j] = iFairscore(Y[j], X)
        if_ensemble = min(if_result)
        idx = np.argmin(if_result)
        selected.append(members[idx])
        self.score_set[members[idx]] = Y[idx]
        y_updated = np.delete(y_updated, (idx), axis=0)
        del members[idx]
        print("Initial iFairScore: ", if_ensemble)

        #for remaining selection
        for i in range(y_updated.shape[0]):
            if_result = np.zeros(y_updated.shape[0])
            for j in range(y_updated.shape[0]):
                int_score_set = self.score_set.copy(deep=True)
                int_score_set['new'] = y_updated[j]
                intermediate_score = t_average(int_score_set)
                if_result[j] = iFairscore(intermediate_score, X)
            if_result_min = min(if_result)
            if if_result_min < if_ensemble:
                if_ensemble = if_result_min
                idx = np.argmin(if_result)
                selected.append(members[idx])
                self.score_set[members[idx]] = y_updated[idx]
                y_updated = np.delete(y_updated, (idx), axis=0)
                del members[idx]
                print("Updated iFairScore: ", if_ensemble)
            else:
                break
        print("Final iFairScore: ", if_ensemble)
        self.selected = selected
        self.if_ensemble = if_ensemble
        self.f_score = t_average(self.score_set)
        
    def fit(self, data, df_score):
        print("Fitting...")
        d_t = df_score.to_numpy()
        self.members = list(df_score.columns)
        Y = d_t.transpose()
        X=data.drop(['Class'], axis=1)
        self.T=data['Class'].replace(['inlier', 'outlier'], [0, 1], inplace=False)
        self.main_function(Y, X)
        print("Success")
    
    def selected_members(self):
        return self.selected
    def score_df(self):
        return self.score_set
    def final_score(self):
        return self.f_score
    def print_results(self):
        evaluate_print("iFselect", self.T, self.f_score)

