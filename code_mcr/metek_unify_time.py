#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Read in .raw files, add metadata, save as .nc files with metadata
# Usable only for Uni Basel Metek sonic: location ro, level 1 (N1)
# data from other Basel sonics is stored differently


import numpy as np
import pandas as pd
import xarray as xr
import matplotlib.pyplot as plt
import glob
import os
import datetime as dt


from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl

# TODO 
# compare with other stations?
# DoY 212 - DoY 214 8:30 OR DoY 216 22:30 saved as summer CET, not CET
# TODO check the daily cycles to determine time?
# DoY 216 - 218 missing anyway


# path to first few days of data
path = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'
# list  all files from those days: full path
files = sorted(glob.glob(os.path.join(path, '*.nc')))

# Make a list of strings holding the dates of the files
dates = pd.date_range(start='1999-07-31', end='1999-08-08', freq='D')
datestrings = dates.strftime('%Y_%m_%d')

# initiate figure
fig, axes = plt.subplots(nrows=3, ncols=3, figsize=[12,12])
axes = axes.flatten()

# list all files at the given date
for i, d in enumerate(datestrings):
    print(d)
    files_day = [f for f in files if d in f]
    if len(files_day) > 0:
        ds_day = xr.open_mfdataset(files_day)
        # 5 min rolling mean
        # T_5min = ds_day.T.rolling(time=6000, center=True).mean()
        T_5min = ds_day.T.resample(time="300s").mean()
        # axes[i].plot(T_5min.time, T_5min)
        T_5min.plot(ax=axes[i])
    
