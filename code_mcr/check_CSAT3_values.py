#!/usr/bin/env python3
# -*- coding: utf-8 -*-

###############################################################################
#
# script to checks if my calculations are correct for the 30 min averages from
# different sonics: check temperature, humidity, wind components...
#
# Variable names in the database:
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


# Indicates whether the uvw values should be calibrated in this script. Set to
# true only if the files were not calibrated beforehand.
calibrate = False

verbose = True

# path to database files with 30 min averages, processed by Basel
path_asc = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/asc/'
path_bin = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/bin/'

# path to processed high resolution Basel data that I want to check
path_data = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# Tower and levels of the CSAT3 sonics
# tower = 'E2_1'  # 118
tower = 'E2_2'  # 199

# %% load high-resolution and database data

# get all files for the chosen sonic
files = sorted([os.path.join(path_data, f) for f in os.listdir(path_data)
                if tower in f])
# choose some ~20ish random files to plot
files = files[10:35]
# load the high resolution files as one combined data set
ds = xr.open_mfdataset(files,
                       coords=['time'],
                       combine='nested',
                       data_vars=['u', 'v', 'w', 'T', 'q'])

# turn wind
u = ds.u.load().values
v = -1 * ds.v.load().values
w = ds.w.load().values

# if calibration was not applied before, optionally apply now
if calibrate:
    # get serial number
    serial = ds.attrs['sonic serial number']
    # apply matrix calibration
    u_corr, v_corr, w_corr = correct_matrix(u, v, w, serial)

    # plot uncalibrated and calibrated wind values, to see if everything is ok
    if verbose:
        fig, axes = plt.subplots(ncols=3, nrows=1, figsize=[12, 8])
        axes[0].plot(u_corr)
        axes[0].plot(u)
        axes[1].plot(v_corr)
        axes[1].plot(v)
        axes[2].plot(w_corr)
        axes[2].plot(w)
        print(np.max(np.abs(u-u_corr)))

    # turn v-wind back, assign back corrected wind values
    u = u_corr
    v = -1 * v_corr
    w = w_corr


# add wind speed to data set - separate wind components cannot be compared
#   because Basel did a coordinate transform. To compare, get both calibrated
#   and uncalibrated wind speeds: these are equal if calibrate == False
ds['wspd'] = np.sqrt(ds.u**2 + ds.v**2 + ds.w**2)
ds['wspd_calib'] = ds.w * 0 + np.sqrt(u**2 + v**2 + w**2)

# Database data
# parse filename to correspond to the sonic codes in the 30 min filenames
sonicstring = tower[0:4].replace('_', '')

# get 30 min averages of the needed variables from the high-resolution dataset
var = ['u', 'v', 'w', 'wspd', 'T', 'wspd_calib']
ds_30min = ds[var].resample(time='30min').mean(dim='time', skipna=True)

# I used 403, Basel used 402.7 as the conversion factor between
#   speed of sounde and temperature, then change K to C
ds_30min['T'] = ((ds_30min['T']) * 403 / 402.7) - 273.15
ds_30min.load()


# %% Functions to get variable series

def asc_to_series(var):
    """
    Load dataframe from an ascii file for a given variable

    Parameters
    ----------
    var : str
        name of the variable to load from an .asc file:
            - ATA = temperature
            - MSA = windspeed
            - USA, VSA, WSA = wind components

    Returns
    -------
    data : pd.Dataframe
        dataframe containing the loaded variable with the associated timestamps

    """
    # Capitalize variable name
    var_caps = var.upper() + '_'
    # Get path to the file to load
    path = os.path.join(path_asc, var_caps+sonicstring+'.asc')
    # Load data as a csv file
    data = pd.read_csv(path, header=None, na_values=-9999)
    # Make the corresponding daterange
    daterange = pd.date_range(start='1999-07-10',
                              freq='30min',
                              periods=len(data))
    # Assign the daterange as an index to the dataframe
    data.index = daterange
    return data


def xdr_to_series(var):
    """
    Load data series from a xdr file for a given variable

    Parameters
    ----------
    var : str
        name of the variable to load from a .xdr file:
            - ATA = temperature
            - MSA = windspeed
            - USA, VSA, WSA = wind components

    Returns
    -------
    data : pd.Series
        dataframe containing the loaded variable with the associated timestamps

    """
    # Capitalize variable name
    var_caps = var.upper() + '_'
    # Get path to the file to load
    path = os.path.join(path_bin, var_caps+sonicstring+'.xdr')
    # Load binary data to buffer
    buffer = open(path, 'rb').read()
    # Unpack all values into one array
    unpacker = xdrlib.Unpacker(buffer)
    data = np.array(unpacker.unpack_farray(4560, unpacker.unpack_float))
    # replace -9999 values with nans
    data[data == -9999] = np.nan
    # Make the corresponding daterange
    daterange = pd.date_range(start='1999-07-10',
                              freq='30min',
                              periods=len(data))
    # turn array into series
    data_series = pd.Series(data, daterange)
    return data_series


# %% ATA: temperature

# get 30 min data from the database
ATA_asc = asc_to_series('ATA')
ATA_30min = ATA_asc.loc[ds_30min.time.values]

# Figure + values: compare temperatures
fig, ax = plt.subplots(figsize=[12, 12])
# compare averaged high-frequency and 30-min database data
ds_30min.T.plot(ax=ax)
ATA_30min.plot(ax=ax)


if verbose:
    # compare numeric values: useful when values are very close
    print(ds_30min.T.values)
    print(ATA_30min.values.squeeze())

# %% MSA: wind speed

# use try/except because the MSA file doesn't exist for all sonics
try:
    # get 30 min data from the database
    MSA_xdr = xdr_to_series('MSA')
    MSA_30min = MSA_xdr.loc[ds_30min.time.values]

    # Figure + values: compare wind speeds
    fig, ax = plt.subplots(figsize=[12, 12])
    # compare uncalibrated, calibrated and database values
    ds_30min.wspd.plot(ax=ax, label='high freq')
    ds_30min.wspd_calib.plot(ax=ax, label='high freq calibrated')
    MSA_30min.plot(ax=ax, label='database')
    ax.legend(fontsize=16)

    if verbose:
        print('WINDSPEED')
        print(ds_30min.wspd.values)
        print(MSA_30min.values.squeeze())
        print('')
except FileNotFoundError:
    pass


# %% USA VSA WSA: wind speed components
# these are not expected to be the same for high-freq and database data since
# in the original data anaylsis they applied a coordinate transform

# get 30 min data from the database
USA_xdr = xdr_to_series('USA')
VSA_xdr = xdr_to_series('VSA')
WSA_xdr = xdr_to_series('WSA')

USA_30min = USA_xdr.loc[ds_30min.time.values]
VSA_30min = VSA_xdr.loc[ds_30min.time.values]
WSA_30min = WSA_xdr.loc[ds_30min.time.values]


# Figure + values: compare wind speed w component
fig, axes = plt.subplots(ncols=3, nrows=1, figsize=[12, 8])
ds_30min.u.plot(ax=axes[0])
USA_30min.plot(ax=axes[0])
ds_30min.v.plot(ax=axes[1])
VSA_30min.plot(ax=axes[1])
ds_30min.w.plot(ax=axes[2])
WSA_30min.plot(ax=axes[2])

if verbose:
    print('U WIND')
    print(ds_30min.u.values)
    print(USA_30min.values.squeeze())
    print('')
    print('V WIND')
    print(ds_30min.v.values)
    print(VSA_30min.values.squeeze())
    print('')
    print('W WIND')
    print(ds_30min.w.values)
    print(WSA_30min.values.squeeze())
    print('')
