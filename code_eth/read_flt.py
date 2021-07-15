#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Read in .flt files, add metadata, save as .nc files with metadata
# Usable for the one ETH sonic with sampling frequency of 10 Hz
# Sonic located at site "D" (Torrazza), but is denoted by 'H' in ETH files

import numpy as np
import pandas as pd
import xarray as xr
import glob
import os

# local imports
from utils import make_namestrings
from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl, krypton_SN, krypton_height


verbose = True

# flag to save files
savefiles = True
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics_processed/'

# path to all data from ETH sonics: sorted in subfolders by day of year
path = "/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics"
# list all .flt files in all subfolders
files_all = sorted(glob.glob(os.path.join(path, '**/H*.flt')))

# max sampling frequency of the sonic
freq = 10    # [Hz]

# loop through all files, read out and store the useful data file by file
for filename in files_all:

    # get only the filename within the folder, without the full path
    fname = os.path.split(filename)[1]

    # get location (sonic letter), initial time of file, name for saving output
    loc, date, output_name = make_namestrings(fname)

    # For debugging and seeing conversion progress
    if verbose:
        print(date, filename)

    # make a range of time values: use fixed time periods
    timerange_full = pd.date_range(date, freq=str(1/freq)+'S', periods=18000)

    # open file for reading (r)
    fopen = open(filename, 'r')
    # read flt data into one vector
    arr = np.fromfile(fopen, dtype=np.float32)

    # number of columns in data: 9000 data points per variable per 30 min
    # 4 variables without krypton: 72000 / 1800 sec / 10 Hz = 4 channels
    if len(arr) == 72000:
        channels = 4
    # 5 variables after krypton is added (Sep 20th, i.e. day 263)
    # 90000 / 1800 sec / 10 Hz = 5 channels
    elif len(arr) == 90000:
        channels = 5
    # if data is missing, skip file (should never happen with available data)
    else:
        print('Length of array: {}. Shape cannot be changed.'.format(len(arr)))
        continue

    # reshape the data array:
    arr = np.reshape(arr, (-1, channels))

    # Data variables to make the dataset
    data_vars = dict(u=('time', arr[:, 0]),
                     v=('time', arr[:, 1]),
                     w=('time', arr[:, 2]),
                     T=('time', arr[:, 3] + 273.15))   # covert temp in  C to K
    coords = dict(time=timerange_full)

    # Make dataset
    ds = xr.Dataset(data_vars=data_vars, coords=coords)

    # Add attributes to the variables
    ds.u.attrs = {'units': 'm/s'}
    ds.v.attrs = {'units': 'm/s'}
    ds.w.attrs = {'units': 'm/s'}
    ds.T.attrs = {'units': 'K',
                  'info': 'sonic (virtual) temperature'}

    # # Add general metadata
    ds.attrs['frequency [Hz]'] = freq

    # # sonic metadata
    ds.attrs['sonic tower and level'] = sonic_location[loc]
    ds.attrs['sonic serial number'] = sonic_SN[loc]
    ds.attrs['sonic height [m]'] = sonic_height[loc]
    ds.attrs['sonic location [lat, lon]'] = sonic_latlon[loc]
    ds.attrs['tower altitude [m a.s.l.]'] = height_asl[loc]

    # If humidity data is available:
    if arr.shape[1] > 4:
        # add humidity and metadate
        humidity = arr[:, 4]
        ds = ds.assign({'q': ('time', humidity)})
        ds.q.attrs = {'units': '?',
                      'info': 'The original data had no metadata about humidity. The database of MAP-Riviera 30 min data does not contain data from this krypton at all.'}
        # add krypton metadata
        ds.attrs['krypton serial number'] = krypton_SN[loc]
        ds.attrs['krypton height'] = krypton_height[loc]

    if savefiles:
        ds.to_netcdf(os.path.join(save_folder, output_name))
