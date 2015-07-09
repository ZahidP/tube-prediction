import json
import zipfile
import matplotlib.pyplot as plt
import re
import numpy as np
import pandas as pd

mypath = "/Users/zahidpanjwani/Desktop/Code/Kaggle/Tube-Pricing/competition_data/"
compfiles = ['bill_of_materials.csv','comp_adaptor.csv', 'comp_boss.csv', 'comp_elbow.csv', 'comp_float.csv', 'comp_hfl.csv', 'comp_nut.csv', 'comp_other.csv', 'comp_sleeve.csv', 'comp_straight.csv', 'comp_tee.csv', 'comp_threaded.csv','train_set.csv','tube.csv']

arr = []
i = 0
for f in compfiles:
    r_file = mypath + f
    arr.append(pd.read_csv(r_file))
    i = i + 1

merge = arr_df[12].merge(arr_df[0],how="inner",on="tube_assembly_id")
merge = df_merge.merge(arr_df[13],how="inner",on="tube_assembly_id")

df_merge.to_csv("/Users/zahidpanjwani/Desktop/Code/Kaggle/Tube-Pricing/competition_data/merged.csv")



merged = pd.read_csv("/Users/zahidpanjwani/Desktop/Code/Kaggle/Tube-Pricing/competition_data/merged.csv")
train = merged.iloc[0:,1:9]
bill = merged.iloc[0:,10:22]
merged.iloc[0:,1:10].isnull().values.sum()
