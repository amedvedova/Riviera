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
# ATA = accoustic temperature (ATA1 = 1 min data)
# ATS = accoustic temp st dev
# MSA = scalar wind speed
# NST = number of samples
# PSA = barometric pressure
# USA, VSA, WSA = wind components
# USS, VSS, WSS = wind components st dev
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

# path to processed high resolution ETH data that I want to check
path_data = '/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics_processed/'

# choose some random files to load and compare (not all combinations work)
tower = 'A1_1'
# tower = 'B_2'
# tower = 'C_1'
# tower = 'D_1'
# date = '_1999_08_25'
date = '_1999_09_05'
# date = '_1999_10_01'

# %% load high-resolution and database data

# High resolution
# load the files as one combined data set
files = sorted([os.path.join(path_data, f) for f in os.listdir(path_data)
                if tower+date in f])
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
ATA = pd.read_csv(ATApath, header=None, na_values=-9999)
MSA = pd.read_csv(MSApath, header=None, na_values=-9999)

# get daterange for the data as indicated in the database
#   starting at 10.07.1999 in 30 min initervals
daterange = pd.date_range(start='1999-07-10', freq='30min', periods=len(ATA))

# assing the daterange as index to ATA etc
ATA.index = daterange
MSA.index = daterange

# if present, repeat for AHA (humidity)
if sonicstring in ['A11', 'A13', 'B2']:
    AHApath = os.path.join(path_avg, 'AHA_'+sonicstring+'.asc')
    AHA = pd.read_csv(AHApath, header=None, na_values=-9999)
    AHA.index = daterange


# %% Data processing

# get 30 min averages of the needed variables from the high-resolution dataset
variables = ['wspd', 'T']
if sonicstring in ['A11', 'A13', 'B2']:
    variables = ['wspd', 'T', 'q']

ds_30min = ds[variables].resample(time='30min').mean(dim='time')

# get voltage and try to see if I can get the Basel-like values
# all calibration coefficients are in the attributes
voltage = ds.voltage

# I used 403, Basel used 402.7 as the conversion factor between
#   speed of sounde and temperature, then change K to C
if tower[0] in ['A', 'B', 'C']:
    ds_30min['T'] = ((ds_30min['T']) * 403 / 402.7) - 273.15
if tower[0] in ['D']:
    ds_30min['T'] = ds_30min['T'] - 273.15

# get values from ATA, MSA and AHA, corresponding to the time of the dataset
ATA_30min = ATA.loc[ds_30min.time.values]
MSA_30min = MSA.loc[ds_30min.time.values]
if sonicstring in ['A11', 'A13', 'B2']:
    AHA_30min = AHA.loc[ds_30min.time.values]

    # ds.q is specific humidity [kg/kg], we want absolute hum. [g/m^3]
    q_abs = ds_30min.q * 1000 / 1.293  # [1 m^3 air = 1.293 kg]


# %% Plotting and checking values

# Figure + values: compare temperatures
fig, ax = plt.subplots(figsize=[12, 12])
ds_30min.T.plot(ax=ax)
ATA_30min.plot(ax=ax)

if verbose:
    print(ds_30min.T.values)
    print(ATA_30min.values.squeeze())

# Figure + values: compare wind speeds
fig, ax = plt.subplots(figsize=[12, 12])
ds_30min.wspd.plot(ax=ax)
MSA_30min.plot(ax=ax)

if verbose:
    print(ds_30min.wspd.values)
    print(MSA_30min.values.squeeze())

# Figure + values: compare humidity (if available)
if sonicstring in ['A11', 'A13', 'B2']:
    fig, ax = plt.subplots(figsize=[12, 12])
    q_abs.plot(ax=ax)
    AHA_30min.plot(ax=ax)

    if verbose:
        print(q_abs.values)
        # Huh, values of 35g/m^3 at below 20C, this seems wrong.
        # https://www.tis-gdv.de/tis_e/misc/klima-htm/
        # Thus, no point in comparing!
        print(AHA_30min.values.squeeze())
