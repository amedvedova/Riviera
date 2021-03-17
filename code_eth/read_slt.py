#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Read in .SLT files, add metadata, save as .nc files with metadata
# Usable only for ETH sonics with sampling frequency of 20.82 Hz
# there are 7 of these: with labels a, b, c, d, e, f, g (given by ETH)
# data from sonic h stored in a different format

import numpy as np
import pandas as pd
import xarray as xr
import matplotlib.pyplot as plt
import glob
import os

# local imports
from utils import make_namestrings, make_time_arrays, get_reference_data, \
                  get_hourly_vars, get_temperature, get_temp_simplified, \
                  get_humidity, prepare_ds_with_ref_data, \
                  prepare_ds_no_ref_data
from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl, krypton_SN, krypton_height

# path to all data from ETH sonics: sorted in subfolders by day of year
path = "/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics"
# list all .SLT files in all subfolders
files_all = sorted(glob.glob(os.path.join(path, '**/F*.SLT')))

# max sampling frequency of the sonics: 75000 data points per hour
# some data files contain fewer points, but all at least 73500
freq = 75000 / 60 / 60    # 20.833 Hz


# TODO temporary: for trying out data analysis
# files_all = files_all[:15]
fs = [f for f in os.listdir()if '.SLT' in f]

# loop through all files, read out and store the useful data file by file
# for filename in files_all:
for filename in sorted(fs)[0:4]:

    # get only the filename within the folder, without the full path
    fname = os.path.split(filename)[1]

    # get location (sonic letter), initial time of file, name for saving output
    loc, date, output_name = make_namestrings(fname)

    # make a range of time values: use fixed time periods
    timerange_full, timerange_30min = make_time_arrays(date, freq)

    # open file for reading (r)
    fopen = open(filename, 'r')
    # read SLT data into one vector
    arr = np.fromfile(fopen, dtype=np.int16)
    # change type to support more precision
    arr = arr.astype(np.float64)

    # get the number of columns in data:
    # at these sonics there are also kryptons (in the 5th channel)
    if loc in ['A', 'C']:
        channels = 5
    # no kryptons at these sonics (only 4 channels)
    elif loc in ['B', 'D', 'E', 'G']:
        channels = 4
    # krypton + 2 additional analogue channels for T and q (7 channels)
    elif loc in ['F']:
        channels = 7

    # reshape the data array:
    arr = np.reshape(arr, (-1, channels))
    # first line is a useless header: get rid of it
    arr = arr[1:, :]

    # pad array to length 750000 (full hour)
    arr = np.pad(arr, ((0, 75000-arr.shape[0]), (0, 0)),
                 'constant', constant_values=np.nan)

    # get reference data, stored in 30 min intervals
    # if available, the file stored has rh, temperature and pressure - needed
    # to get specific humidity and vapor pressure. If not available: None.
    ref_data = get_reference_data(path, loc, fname, timerange_30min)

    # prepare data to make the dataset, based reference data availability
    # get hourly averages, if available
    if ref_data is not None:
        t_h, rh_h, p_h, e_h, rho_wv_h, rho_air_h = get_hourly_vars(ref_data)
        data_vars, coords, T_info = prepare_ds_with_ref_data(arr,
                                                             timerange_full,
                                                             timerange_30min,
                                                             date,
                                                             loc,
                                                             ref_data)
    # If reference data is not available
    else:
        data_vars, coords, T_info = prepare_ds_no_ref_data(arr,
                                                           timerange_full,
                                                           date)

    # Make dataset
    ds = xr.Dataset(data_vars=data_vars,
                    coords=coords)

    # Add attributes to the variables
    ds.u.attrs = {'units': 'm/s'}
    ds.v.attrs = {'units': 'm/s'}
    ds.w.attrs = {'units': 'm/s'}
    ds.T.attrs = {'units': 'K',
                  'info': T_info}

    # Add general metadata
    ds.attrs['frequency [Hz]'] = freq

    # sonic metadata
    ds.attrs['sonic tower and level'] = sonic_location[loc]
    ds.attrs['sonic serial number'] = sonic_SN[loc]
    ds.attrs['sonic height [m]'] = sonic_height[loc]
    ds.attrs['sonic location [lat, lon]'] = sonic_latlon[loc]
    ds.attrs['tower altitude [m a.s.l.]'] = height_asl[loc]

    # there's a krypton present AND reference data is available
    # add the humidity data + metadata to the dataset
    if 'q' in ds.variables:
        # add units, krypton metadata (height and serial number)
        ds.q.attrs = {'units': 'kg/kg'}
        ds.attrs['krypton serial number'] = krypton_SN[loc]
        ds.attrs['krypton height'] = krypton_height[loc]

    if ref_data is not None:
        # Add 30 min variable metadata
        ds.T_30min.attrs = {'units': 'K',
                            'info': 'Loaded from reference file'}
        ds.rh_30min.attrs = {'units': '%',
                             'info': 'Loaded from reference file'}
        ds.p_30min.attrs = {'units': 'hPa',
                            'info': 'Calculated from the reference pressure and temperature in the reference file'}

    print('\n \n', ds)
    