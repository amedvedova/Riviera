#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Read in .raw files, add metadata, save as .nc files with metadata
# Usable only for Uni Basel Gill R2 sonics: locations mn (level 3, 4 5),
# ro (level 2), ag (level 1, 2)
# data from other Basel sonics is stored differently.
#
# This data requires calibration!
#
# mn: no analog inputs
# ro: krypton used at the analog input
# ag: no analog inputs
#
# GILL R2 TRANSIT COUNT FORMAT (1 character = 1 byte) aa only if analog inputs
# are present, otherwise these bytes are missing
# 1122334455667788aa99
# 1 = start of record HEX "8181" = -32383
# 2 = record number (Counter)
# 3 = transit count t-b axis 1 msb/lsb
# 4 = transit count b-t axis 1 msb/lsb
# 5 = transit count t-b axis 2 msb/lsb
# 6 = transit Count b-t axis 2 msb/lsb
# 7 = transit Count t-b axis 3 msb/lsb
# 8 = transit Count b-t axis 3 msb/lsb
# [a] = analog inputs msb/lsb
# 9 = end of record HEX "8282" = -32126

import numpy as np
import pandas as pd
import xarray as xr
import matplotlib.pyplot as plt
import glob
import os

from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl


# set minimum number of values which must be present in the file to process it
count_threshold = 36000     # out of 37496

# flag to save files, folder to which files will be saved
savefiles = False
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# paths to data from the MCR sonics
path_ag = "/home/alve/Desktop/Riviera/MAP_subset/data/ag/rohdaten/fast"
path_mn = "/home/alve/Desktop/Riviera/MAP_subset/data/mn/rohdaten/fast"
path_ro = "/home/alve/Desktop/Riviera/MAP_subset/data/ro/rohdaten/fast"

# list all relevant .raw and files in all subfolders (day of year)
files_ag_N2 = sorted(glob.glob(os.path.join(path_ag, '**/AG_N2_*.raw')))
files_ag_N4 = sorted(glob.glob(os.path.join(path_ag, '**/AG_N4_*.raw')))
files_mn_N4 = sorted(glob.glob(os.path.join(path_mn, '**/MN_N4_*.raw')))
files_mn_N5 = sorted(glob.glob(os.path.join(path_mn, '**/MN_N5_*.raw')))
files_mn_N7 = sorted(glob.glob(os.path.join(path_mn, '**/MN_N7_*.raw')))
files_ro_N2 = sorted(glob.glob(os.path.join(path_ro, '**/RO_N2_*.raw')))

# frequency of CSAT3 sonics
freq = 20

# concatenate the two lists
# TODO process all files, not only a subset
f_raw_all = files_ag_N2[:10] + files_ag_N4[:10] + files_mn_N4[:10] + \
            files_mn_N5[:10] + files_mn_N7[:10] + files_ro_N2[:10]

f_raw_all = f_raw_all[2:3]


# %% Function definitions


def uvwt_from_file(file, count, bytes_per_row):
    """
    Function for reading the opened file (buffer) and converting the data to
    a numpy array with u, v, w, T variables. All calculations and processing
    are done within this function.

    Parameters
    ----------
    file : _io.BufferedReader
        opened file of the raw binary data


    Returns
    -------
    uvwt : np.array
        numpy array holding the processed values of u, v, w, T

    """
    # read raw bytes from the file
    raw_bytes = file.read()
    dtype = '{}u2'.format(int(bytes_per_row/2))
    # Load data
    transit_count = np.frombuffer(raw_bytes, dtype=dtype)
    # tc: transit counts

    # TODO get uvwt from transit count
    uvwt = 0 * transit_count
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
    # if the full field contains a few points more than 36000, cut those
    #   if there are fewer points, nothing happens
    # TODO how many data points are there? what's the frequency?
    uvwt = uvwt_full[:36000, :]

    # on the other hand, pad to 36000 for shorter files
    uvwt = np.pad(uvwt, ((0, 36000-uvwt.shape[0]), (0, 0)),
                  'constant', constant_values=np.nan)

    # make a dictinary of variables
    data_vars = dict(u=('time', uvwt[:, 0]),
                     v=('time', uvwt[:, 1]),
                     w=('time', uvwt[:, 2]),
                     T=('time', uvwt[:, 3]))
    # make a range of time values: use fixed time periods
    timerange_full = pd.date_range(date,
                                   freq=str(1/freq)+'S',
                                   periods=36000)
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

    # get location and level of the sonic
    if filename[0:5] == 'AG_N2':
        loc = 'F2_1'
    elif filename[0:5] == 'AG_N4':
        loc = 'F2_2'
    elif filename[0:5] == 'MN_N4':
        loc = 'E2_3'
    elif filename[0:5] == 'MN_N7':
        loc = 'E2_4'
    elif filename[0:5] == 'MN_N8':
        loc = 'E2_5'
    elif filename[0:5] == 'RO_N2':
        loc = 'E1_2'
    else:
        loc = None

    return loc, date


# %%

for f_raw in f_raw_all:
    with open(f_raw, 'rb') as file:
        # get location and time of file
        loc, date = info_from_filename(f_raw)
        # get number of analog inputs: 1 at RO_N2, 0 otherwise
        if loc == 'E1_2':
            ai = 1
        else:
            ai = 0
        # get number of bytes in each measurement - depends on analog inputs
        bytes_per_row = 18 + 2*ai

        # get filesize in bytes
        size = os.path.getsize(f_raw)
        # get count of measurements: (size/10)-1 since each measurement is
        # 10 bytes and the first measurement is corrupt
        count = int(size / bytes_per_row)
        # TODO ask Iva: if a lot of data is missing, skip file
        if count <= count_threshold:
            continue
        # load data from the buffer, process it and make and uvwt array
        uvwt = uvwt_from_file(file, count, bytes_per_row)
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
# fig.show()
