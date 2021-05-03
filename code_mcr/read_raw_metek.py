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

from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl

# TODO check with Iva about what to do
# DoY 212 - DoY 214 8:30 OR DoY 216 22:30saved as summer CET, not CET


# set minimum number of values which must be present in the file to process it
count_threshold = 17500

# flag to save files, folder to which files will be saved
savefiles = True
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# path to data from the MCR sonics at "mn" location
path = "/home/alve/Desktop/Riviera/MAP_subset/data/ro/rohdaten/fast"

# list N1/N3 .raw and files in all subfolders (day of year)
# N1/N3 refers to levels where CSAT3 sonics were used
files_N1 = sorted(glob.glob(os.path.join(path, '**/RO_N1_*.raw')))

# frequency of CSAT3 sonics
freq = 10

# concatenate the two lists
# TODO process all files, not only a subset
f_raw_all = files_N1[:10]

# %% Function definitions


def uvwt_from_file(lines):
    """
    Function for reading the opened file (buffer) and converting the data to
    a numpy array with u, v, w, T variables. All calculations and processing
    are done within this function.

    Parameters
    ----------
    file : list
        list of strings, one line = one measurement

    Returns
    -------
    uvwt : np.array
        numpy array holding the processed values of u, v, w, T

    """

    # make an empty storage array
    uvwt = np.empty((18000, 4))
    uvwt.fill(np.nan)

    # loop through the first 18000 lines of each file, re-scale all values
    for i, l in enumerate(lines[:18000]):
        # check if all values are present
        if len(l) == 41:
            uvwt[i, 0] = float(l[6:12]) / 100.0
            uvwt[i, 1] = float(l[16:22]) / 100.0
            uvwt[i, 2] = float(l[26:32]) / 100.0
            uvwt[i, 3] = float(l[36:42]) / 100.0 + 273.15

    return uvwt


def ds_from_uvwt(uvwt_full, date):
    """
    Covnvert the np data array to a dataset containing metadata

    Parameters
    ----------
    uvwt_full : np.array
    date : Timestamp
        timestamp of the beginning of the measurement period

    Returns
    -------
    ds : xr.Dataset


    """
    # if the full field contains a few points more than 18000, cut those, and
    #   if there are fewer points, nothing happens
    uvwt = uvwt_full[:18000, :]

    # on the other hand, pad to 18000 for shorter files
    uvwt = np.pad(uvwt, ((0, 18000-uvwt.shape[0]), (0, 0)),
                  'constant', constant_values=np.nan)

    # make a dictinary of variables
    data_vars = dict(u=('time', uvwt[:, 0]),
                     v=('time', uvwt[:, 1]),
                     w=('time', uvwt[:, 2]),
                     T=('time', uvwt[:, 3]))
    # make a range of time values: use fixed time periods
    timerange_full = pd.date_range(date,
                                   freq=str(1/freq)+'S',
                                   periods=18000)
    # define coordinates
    coords = dict(time=timerange_full)

    # make dataset
    ds = xr.Dataset(data_vars=data_vars, coords=coords)

    # Add attributes to the variables
    ds.u.attrs = {'units': 'm/s'}
    ds.v.attrs = {'units': 'm/s'}
    ds.w.attrs = {'units': 'm/s'}
    ds.T.attrs = {'units': 'K'}

    return ds


def info_from_filename(file):
    # filename
    filename = os.path.split(file)[1]
    # string containing the time information
    datestring = filename[6:-4]
    # proper timestamp denoting when the file begins
    date = pd.to_datetime(datestring, format='%Y_%j_%H%M%S')

    # location of the sonic: hardcoded here since there's nly one Metek sonic
    loc = 'E1_1'

    return loc, date


# %%

for f_raw in f_raw_all:
    with open(f_raw, encoding='ascii') as file:
        # get location and time of file from the filename
        loc, date = info_from_filename(f_raw)

        # read raw ascii from the file
        raw_text = file.read()
        # use the line separator to identify lines: Each has 41 characters
        lines = raw_text.split('\n')
        # get count of measurements
        count = len(lines)
        # TODO ask Iva: if a lot of data is missing, skip file
        if count <= count_threshold:
            continue
        # load data from the buffer, process it and make and uvwt array
        uvwt = uvwt_from_file(lines)
        # make a data set from the array, add metadata of variables
        ds = ds_from_uvwt(uvwt, date)

        # add general metadata
        ds.attrs['frequency [Hz]'] = freq
        ds.attrs['sonic tower and level'] = sonic_location[loc]
        ds.attrs['sonic serial number'] = sonic_SN[loc]
        ds.attrs['sonic height [m]'] = sonic_height[loc]
        ds.attrs['sonic location [lat, lon]'] = sonic_latlon[loc]
        ds.attrs['tower altitude [m a.s.l.]'] = height_asl[loc]

        # save data
        if savefiles:
            # produce output name based on time and location
            output_name = '{}_{}.nc'.format(loc, date.strftime('%Y_%m_%d_%H%M'))
            # save file
            ds.to_netcdf(os.path.join(save_folder, output_name))

# fig, axes = plt.subplots(nrows=2, ncols=2, figsize=[12,12])
# axes = axes.flatten()
# for i in range(4):
#     axes[i].plot(uvwt[:, i])
