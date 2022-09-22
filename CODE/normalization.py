#!/usr/bin/env python
# coding: utf-8


import pandas as pd


def linear_scaling(Y):
    cols = list(Y.columns)
    Y_norm=pd.DataFrame()
    for col in cols:
        col_norm = col + '_norm'
        Y_norm[col_norm] = (Y[col] - Y[col].min())/(Y[col].max()-Y[col].min())
    return Y_norm

