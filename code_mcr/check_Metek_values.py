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

# local imports
from matrix_calibration import get_all_corrections as correct_matrix


verbose = True

# path to database files with 30 min averages, processed by Basel
path_asc = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/asc/'
path_bin = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/bin/'

# path to processed high resolution ETH data that I want to check
path_data = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# choose some random files to load and compare (not all combinations work)
tower = 'E1_1'
# date = '_1999_08_25'
date = '_1999_09_05'
# date = '_1999_10_01'

# %% load high-resolution and database data

# High resolution
# load the files as one combined data set
files = sorted([os.path.join(path_data, f) for f in os.listdir(path_data)
                if tower+date in f])[:24]
ds = xr.open_mfdataset(files,
                       coords=['time'],
                       combine='nested',
                       data_vars=['u', 'v', 'w', 'T', 'q'])
# add wind speed to data set - sepratate wind components cannot be compared
#   because Basel did a coordinate transform

# turn wind
u = -1 * ds.u.load().values
v = -1 * ds.v.load().values
w = ds.w.load().values

# # calibrate: no eff path, mat99
# serial = 'Metek USA-1 9903006'
# u_corr, v_corr, w_corr = correct_matrix(u, v, w, serial)

# # turn wind back
# u = -u_corr
# v = -v_corr
# w = w_corr

ds['wspd'] = ds.w * 0 + np.sqrt(u**2 + v**2 + w**2)

# Database data
# parse filename to correspond to the sonic codes in the 30 min filenames
sonicstring = tower[0:4].replace('_', '')


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


ATA_asc = asc_to_series('ATA')
MSA_asc = asc_to_series('MSA')
VSA_asc = asc_to_series('VSA')
WSA_asc = asc_to_series('WSA')

MSA_xdr = xdr_to_series('MSA')
USA_xdr = xdr_to_series('USA')
VSA_xdr = xdr_to_series('VSA')
WSA_xdr = xdr_to_series('WSA')

# %%

# get 30 min averages of the needed variables from the high-resolution dataset
var = ['wspd', 'u', 'v', 'w', 'T']
ds_30min = ds[var].resample(time='30min').mean(dim='time', skipna=True)

# change K to C
ds_30min['T'] = ds_30min['T'] - 273.15

# get values from ATA, MSA and AHA, corresponding to the time of the dataset
ATA_30min = ATA_asc.loc[ds_30min.time.values]
MSA_30min = MSA_asc.loc[ds_30min.time.values]
# USA_30min = USA_asc.loc[ds_30min.time.values]
VSA_30min = VSA_asc.loc[ds_30min.time.values]
WSA_30min = WSA_asc.loc[ds_30min.time.values]

# ATA_bin_30min = ATA_bin.loc[ds_30min.time.values]
MSA_xdr_30min = MSA_xdr.loc[ds_30min.time.values]
USA_xdr_30min = USA_xdr.loc[ds_30min.time.values]
VSA_xdr_30min = VSA_xdr.loc[ds_30min.time.values]
WSA_xdr_30min = WSA_xdr.loc[ds_30min.time.values]


# %% Plotting and checking values

# Figure + values: compare temperatures
# Temperature is on point
fig, ax = plt.subplots(figsize=[12, 12])
ds_30min.T.plot(ax=ax)
ATA_30min.plot(ax=ax)


if verbose:
    print(ds_30min.T.values)
    print(ATA_30min.values.squeeze())

# Figure + values: compare wind speeds
# Wind speed is off: does it need calibration?
fig, ax = plt.subplots(figsize=[12, 12])
ds_30min.wspd.plot(ax=ax)
MSA_30min.plot(ax=ax)
MSA_xdr_30min.plot(ax=ax)

if verbose:
    print(ds_30min.wspd.values)
    print(MSA_30min.values.squeeze())
    print(MSA_xdr_30min.values.squeeze())

# Figure + values: compare wind speed w component
# asc and xdr values consistent
fig, ax = plt.subplots(figsize=[12, 12])
ds_30min.w.plot(ax=ax)
WSA_30min.plot(ax=ax)
# WSA_xdr_30min.plot(ax=ax)

if verbose:
    print('High-res wind data:')
    print(ds_30min.w.values)
    print('Low-res wind data:')
    print(WSA_30min.values.squeeze())
    print(WSA_xdr_30min.values.squeeze())
