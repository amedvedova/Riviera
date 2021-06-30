#!/usr/bin/env python3
# -*- coding: utf-8 -*-

###############################################################################
#
# script to checks if my calculations are correct for the 30 min averages from
# different sonics: check temperature, humidity, wind components...
#
# Variable names in the database:
# AHA = absolute humidity
# AHS = abs. hum. st. dev
# ATA = accoustic temperature
# MSA = scalar wind speed
# USA, VSA, WSA = wind components
#
###############################################################################


import numpy as np
import pandas as pd
import xarray as xr
import os
import matplotlib.pyplot as plt
import xdrlib


verbose = True

# path to database files with 30 min averages, processed by Basel
path_asc = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/asc/'
path_bin = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/bin/'

# path to processed high resolution Basel data that I want to check
path_data = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# choose some random files to load and compare (not all combinations work)
# TODO AHA E12 (humidity)
# tower = 'E1_2'  # matrix
tower = 'E2_3'  # gill
# tower = 'E2_4'  # gill
# tower = 'E2_5'  # matrix
# tower = 'F2_1'  # gill
# tower = 'F2_2'  # matrix

# %% load high-resolution and database data

# High resolution
# load the files as one combined data set
files = sorted([os.path.join(path_data, f) for f in os.listdir(path_data)
                if tower in f])[:24]
ds = xr.open_mfdataset(files,
                       coords=['time'],
                       combine='nested',
                       data_vars=['u', 'v', 'w', 'T', 'q'])
# add wind speed to data set - sepratate wind components cannot be compared
#   because Basel did a coordinate transform

ds['wspd'] = np.sqrt(ds.u**2 + ds.v**2 + ds.w**2)

# Database data
# parse filename to correspond to the sonic codes in the 30 min filenames
sonicstring = tower[0:4].replace('_', '')

# get 30 min averages of the needed variables from the high-resolution dataset
var = ['wspd', 'u', 'v', 'w', 'T']
ds_30min = ds[var].resample(time='30min').mean(dim='time', skipna=True)

# change K to C
ds_30min['T'] = ds_30min['T'] - 273.15


# %%

def asc_to_series(var):
    var_caps = var.upper() + '_'
    path = os.path.join(path_asc, var_caps+sonicstring+'.asc')
    data = pd.read_csv(path, header=None, na_values=-9999)

    daterange = pd.date_range(start='1999-07-10',
                              freq='30min',
                              periods=len(data))
    data.index = daterange
    return data


def xdr_to_series(var):
    var_caps = var.upper() + '_'
    path = os.path.join(path_bin, var_caps+sonicstring+'.xdr')
    buffer = open(path, 'rb').read()
    unpacker = xdrlib.Unpacker(buffer)
    data = np.array(unpacker.unpack_farray(4560, unpacker.unpack_float))
    data[data == -9999] = np.nan

    daterange = pd.date_range(start='1999-07-10',
                              freq='30min',
                              periods=len(data))
    data_series = pd.Series(data, daterange)
    return data_series


# %% ATA

# ATA_asc = asc_to_series('ATA')
# ATA_30min = ATA_asc.loc[ds_30min.time.values]

# # Figure + values: compare temperatures
# fig, ax = plt.subplots(figsize=[12, 12])
# ds_30min.T.plot(ax=ax)
# ATA_30min.plot(ax=ax)
# fig.show()

# if verbose:
#     print(ds_30min.T.values)
#     print(ATA_30min.values.squeeze())

# %% MSA

try:
    MSA_xdr = xdr_to_series('MSA')
    MSA_30min = MSA_xdr.loc[ds_30min.time.values]
    
    # Figure + values: compare wind speeds
    fig, ax = plt.subplots(figsize=[12, 12])
    ds_30min.wspd.plot(ax=ax, label='high freq')
    MSA_30min.plot(ax=ax, label='database')
    ax.legend(fontsize=16)
    
    if verbose:
        print('WINDSPEED')
        print(ds_30min.wspd.values)
        print(MSA_30min.values.squeeze())
        print('')
except FileNotFoundError:
    pass


# %% USA VSA WSA

# USA_xdr = xdr_to_series('USA')
# VSA_xdr = xdr_to_series('VSA')
# WSA_xdr = xdr_to_series('WSA')

# USA_30min = USA_xdr.loc[ds_30min.time.values]
# VSA_30min = VSA_xdr.loc[ds_30min.time.values]
# WSA_30min = WSA_xdr.loc[ds_30min.time.values]


# # Figure + values: compare wind speed w component
# fig, axes = plt.subplots(ncols=3, nrows=1, figsize=[12, 8])
# ds_30min.u.plot(ax=axes[0])
# USA_30min.plot(ax=axes[0])
# ds_30min.v.plot(ax=axes[1])
# VSA_30min.plot(ax=axes[1])
# ds_30min.w.plot(ax=axes[2])
# WSA_30min.plot(ax=axes[2])

# if verbose:
#     print('U WIND')
#     print(ds_30min.u.values)
#     print(USA_30min.values.squeeze())
#     print('')
#     print('V WIND')
#     print(ds_30min.v.values)
#     print(VSA_30min.values.squeeze())
#     print('')
#     print('W WIND')
#     print(ds_30min.w.values)
#     print(WSA_30min.values.squeeze())
#     print('')
