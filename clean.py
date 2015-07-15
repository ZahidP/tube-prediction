import json
import zipfile
import matplotlib.pyplot as plt
import re
import numpy as np
import pandas as pd

mypath = "/Users/zahidpanjwani/Desktop/Code/Kaggle/tube-prediction/competition_data/"
compfiles = ['bill_of_materials.csv','comp_adaptor.csv', 'comp_boss.csv', 'comp_elbow.csv', 'comp_float.csv', 'comp_hfl.csv', 'comp_nut.csv', 'comp_other.csv', 'comp_sleeve.csv', 'comp_straight.csv', 'comp_tee.csv', 'comp_threaded.csv','train_set.csv','tube.csv','tube_end_form.csv']

arr = []
i = 0
for f in compfiles:
    r_file = mypath + f
    arr.append(pd.read_csv(r_file))
    i = i + 1

merged = arr[12].merge(arr[0],how="inner",on="tube_assembly_id")
merged = merged.merge(arr[13],how="inner",on="tube_assembly_id")
merged = merged.merge(arr[14],how="inner",on="end_a")

# save it as csv just in case we need it somewhere else
merged.to_csv("/Users/zahidpanjwani/Desktop/Code/Kaggle/tube-prediction/competition_data/merged.csv")



cols = merged.columns.values.tolist()
regexp = re.compile(r'id')
for col in cols:
    # check if column name contains 'id'
    if regexp.search(col) is not None:
        print(col)
        merged.iloc[0:,idx].astype('category')
    # otherwise check if it is a numeric type
    else:
        if (merged[col].dtype == 'int64' or merged[col].dtype == 'float64'):
            # fill numeric column NAs with mean
            merged[col].fillna(merged[col].mean())

# check if any of these contain NaN
merged.iloc[0:,1:8].isnull().values.sum()
