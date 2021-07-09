#!/usr/bin/env python3
# -*- coding: utf-8 -*-

###############################################################################
#
# script to checks if my calculations are correct for the 30 min averages from
# different sonics: check temperature, humidity, wind components...
#
# Variable names in the database:
# AHA = absolute humidity
# ATA = accoustic temperature (ATA1 = 1 min data)
# USA, VSA, WSA = wind components
# MSA = wind speed
#
###############################################################################


import numpy as np
import pandas as pd
import xarray as xr
import os
import matplotlib.pyplot as plt


verbose = True

# path to database files with 30 min averages, processed by Basel
path_avg = '/home/alve/Desktop/Riviera/MAP_subset/data/database/data/asc/'

# path to the processed high resolution ETH data to be checked
path_data = '/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics_processed/'

# choose some random files to load and compare
tower = 'A1_1'  # A, krypton
# tower = 'A1_2'  # B
# tower = 'A1_3'  # C, krypton
# tower = 'B_1'  # E
# tower = 'B_2'  # F, krypton
# tower = 'B_3'  # G
# tower = 'C_1'  # D
# tower = 'D_1'  # H

# %% load high-resolution and database data

# load the high resolution files as one combined data set
files = sorted([os.path.join(path_data, f) for f in os.listdir(path_data)
                if tower in f])[75:95]
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

# path to files with ATA (temperature), MSA (wind speed), AHA (humidity)
ATApath = os.path.join(path_avg, 'ATA_'+sonicstring+'.asc')
MSApath = os.path.join(path_avg, 'MSA_'+sonicstring+'.asc')

# load files
ATA = pd.read_csv(ATApath, header=None, na_values=-9999, names=['ATA'])
MSA = pd.read_csv(MSApath, header=None, na_values=-9999, names=['MSA'])

# get daterange for the data as indicated in the database
#   starting at 10.07.1999 in 30 min initervals
daterange = pd.date_range(start='1999-07-10', freq='30min', periods=len(ATA))

# assing the daterange as index to ATA etc
ATA.index = daterange
MSA.index = daterange

# if present, repeat for AHA (humidity)
if sonicstring in ['A11', 'A13', 'B2']:
    AHApath = os.path.join(path_avg, 'AHA_'+sonicstring+'.asc')
    AHA = pd.read_csv(AHApath, header=None, na_values=-9999, names=['AHA'])
    AHA.index = daterange


# %% Data processing

# get 30 min averages of the needed variables from the high-resolution dataset
variables = ['wspd', 'T']
if sonicstring in ['A11', 'A13', 'B2']:
    variables = ['wspd', 'T', 'rho_dry', 'rho_wet']

# Get 30 minute averages
ds_30min = ds[variables].resample(time='30min').mean(dim='time')

# Change K to C
ds_30min['T'] = ds_30min['T'] - 273.15

# get values from ATA, MSA and AHA, corresponding to the time of the dataset
ATA_30min = ATA.loc[ds_30min.time.values]
MSA_30min = MSA.loc[ds_30min.time.values]
if sonicstring in ['A11', 'A13', 'B2']:
    AHA_30min = AHA.loc[ds_30min.time.values]


# %% Plotting and checking values: temperature and wind speed

# Figure + values: compare temperatures
fig, ax = plt.subplots(figsize=[12, 12])
ds_30min.T.plot(ax=ax, label='high freq temp', linewidth=3)
ATA_30min.plot(ax=ax, color='k', linestyle='-.')

ax.legend()

if verbose:
    print(ds_30min.T.values)
    print(ATA_30min.values.squeeze())

# Figure + values: compare wind speeds
fig, ax = plt.subplots(figsize=[12, 12])
ds_30min.wspd.plot(ax=ax, label='high freq wspd', linewidth=3)
MSA_30min.plot(ax=ax, color='k', linestyle='-.')

ax.legend()

if verbose:
    print(ds_30min.wspd.values)
    print(MSA_30min.values.squeeze())


# %% Humidity checks

# Figure + values: compare humidity (if available)
if sonicstring in ['A11', 'A13', 'B2']:
    fig, ax = plt.subplots(figsize=[12, 12])

    ds_30min.rho_wet.plot(ax=ax, label='high freq rho_wet', linewidth=3)
    ds_30min.rho_dry.plot(ax=ax, label='high freq rho_dry', linewidth=3)
    AHA_30min.plot(ax=ax, label='database AHA', color='k', linestyle='-.')

    ax.legend()

    if verbose:
        # Reasonable values for reference:
        # https://www.tis-gdv.de/tis_e/misc/klima-htm/
        print(ds_30min.rho_dry.values)
        print(ds_30min.rho_wet.values)
        print(AHA_30min.values.squeeze())
