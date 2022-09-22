#!/usr/bin/env python
# coding: utf-8


from __future__ import division
import numpy as np
import pandas as pd
from scipy.stats import rankdata


def gFairscore(y,S):
    H_all=[]
    row, col = S.shape
    for i in range(col):
        a=S[:,i]
        H_all.append(kw_test(y,a))
    return sum(H_all)/len(H_all)


def kw_test(y, a):
    N=len(a)
    n=[]
    f=[]
    df=pd.DataFrame()
    df['a']=a
    df['y']=y
    df['rank']=rankdata(y)
    grps = pd.unique(df.a.values)
    d_data = {grp:df['rank'][df.a == grp] for grp in grps}
    for i in range(len(grps)):
        n.append(len(d_data[i]))
    for i in range(len(n)):
        Ri_sq = (np.sum(np.array(d_data[i])))**2
        f.append(Ri_sq/n[i])
    ff=np.sum(np.array(f))
    H = (12/(N*(N+1))) * ff-3*(N+1)
    return H
