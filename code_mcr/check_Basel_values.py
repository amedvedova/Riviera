#!/usr/bin/env python3
# -*- coding: utf-8 -*-

###############################################################################
#
# script to checks if my calculations are correct for the 30 min averages from
# different sonics: check temperature, humidity, wind components...
#
# Variable names in the database:
# AHA = absolute humidity
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
from gill_calibration import get_all_corrections as correct_gill


# Indicates whether the uvw values should be calibrated in this script.
# Set to true only if the files were NOT calibrated beforehand!
calibrate = False
# Print information, plot extra figures
verbose = True

# path to database files with 30 min averages, processed by Basel
path_asc = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/asc/'
path_bin = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/bin/'

# path to processed high resolution Basel data that I want to check
path_data = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'


# Gill sonics: tower+level, calibration, serial number
tower = 'E1_2'  # matrix, 043, krypton
# tower = 'E2_3'  # gill, 211
# tower = 'E2_4'  # gill, 213
# tower = 'E2_5'  # matrix, 212
# tower = 'F2_1'  # gill, 208
# tower = 'F2_2'  # matrix, 160

# Metek sonic: tower+level, calibration
# tower = 'E1_1'  # matrix

# CSAT sonics: tower+level, calibration, serial number
# tower = 'E2_1'  # matrix, 118
# tower = 'E2_2'  # matrix, 199

# Gill HS sonic: tower+level, calibration
# tower = 'E2_6'  # matrix, krypton


# %% load high-resolution and database data

# choose some random files, load the files as one combined data set
n = 300
files = sorted([os.path.join(path_data, f) for f in os.listdir(path_data)
                if tower in f])[n:n+25]
ds = xr.open_mfdataset(files,
                       coords=['time'],
                       combine='nested',
                       data_vars=['u', 'v', 'w', 'T', 'q'])

# prepare variables for possible calibration (not needed if calibration==False)
u = ds.u.load().values
v = ds.v.load().values
w = ds.w.load().values

# apply calibration for comparison, only if it has not been applied before
# if data is already calibrated, this would apply the calibration the 2nd time
if calibrate:
    # get serial number
    serial = ds.attrs['sonic serial number']
    # Sonic-dependent calibration
    if 'Gill R2' in serial:
        # matrix calibration
        if serial[-3:] in ['043', '212', '160']:
            u_corr, v_corr, w_corr = correct_matrix(u, v, w, serial)
        # gill calibration
        elif serial[-3:] in ['211', '213', '208']:
            u_corr, v_corr, w_corr = correct_gill(u, v, w, serial[-4:])
        # reassign uvw
        u = u_corr
        v = v_corr
        w = w_corr
    elif 'Metek' in serial:
        # matrix calibration
        u_corr, v_corr, w_corr = correct_matrix(-1*u, -1*v, w, serial)
        # reassign uvw
        u = -1 * u_corr
        v = -1 * v_corr
        w = w_corr
    elif 'CSAT' in serial:
        # matrix calibration
        u_corr, v_corr, w_corr = correct_matrix(u, -1*v, w, serial)
        # reassign uvw
        u = u_corr
        v = -1 * v_corr
        w = w_corr
    elif 'Gill HS' in serial:
        # matrix calibration
        u_corr, v_corr, w_corr = correct_matrix(-1*u, v, w, serial)
        # reassign uvw
        u = -1 * u_corr
        v = v_corr
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
# only two sonics have humidity data
if tower in ['E1_2', 'E2_6']:
    var = ['wspd', 'wspd_calib', 'u', 'v', 'w', 'T', 'q']
else:
    var = ['wspd', 'wspd_calib', 'u', 'v', 'w', 'T']
ds_30min = ds[var].resample(time='30min').mean(dim='time', skipna=True)

# change K to C
ds_30min['T'] = ds_30min['T'] - 273.15


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
ds_30min.T.plot(ax=ax, label='high freq')
ATA_30min.plot(ax=ax)
# Add title
ax.set_title('Temperature', fontsize='16')


if verbose:
    # compare numeric values: useful when values are very close
    print('TEMPERATURE')
    print(ds_30min.T.values)
    print(ATA_30min.values.squeeze())
    print('')

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
    # Add title
    ax.set_title('Windspeed', fontsize='16')

    if verbose:
        print('WINDSPEED')
        print(ds_30min.wspd.values)
        print(MSA_30min.values.squeeze())
        print('')
except FileNotFoundError:
    pass


# %% AHA: humidity

# use try/except because the MSA file doesn't exist for all sonics
try:
    # get 30 min data from the database
    AHA_asc = asc_to_series('AHA')
    AHA_30min = AHA_asc.loc[ds_30min.time.values]

    # Figure + values: compare wind speeds
    fig, ax = plt.subplots(figsize=[12, 12])
    # compare uncalibrated, calibrated and database values
    ds_30min.q.plot(ax=ax, label='high freq')
    # ds_30min.wspd_calib.plot(ax=ax, label='high freq calibrated')
    AHA_30min.plot(ax=ax, label='database')
    ax.legend(fontsize=16)
    # Add title
    ax.set_title('Humidity', fontsize='16')

    if verbose:
        print('HUMIDITY')
        print('high frequency:')
        print(ds_30min.q.values)
        print('database:')
        print(AHA_30min.values.squeeze())
        print(AHA_30min.values.squeeze() - ds_30min.q.values)
        print('')
except FileNotFoundError:
    pass


# %% USA VSA WSA: wind speed components
# these are not expected to be the same for high-freq and database data since
# in the original data anaylsis they applied a coordinate transform

# # get 30 min data from the database
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