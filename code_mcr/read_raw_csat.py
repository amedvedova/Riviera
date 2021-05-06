#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Read in .raw files, add metadata, save as .nc files with metadata
# Usable only for Uni Basel CSAT3 sonics: location mn, levels 1 and 3 (N1, N3)
# data from other Basel sonics is stored differently
#
#
# CSAT FORMAT (1 character means 1 bit), see CSAT3 manual (Appendix B):
# 5 units ("words") of 2 bytes each per measurement
# little endian: read the two bytes (16 bits) from right to left to get value,
#   i.e. b15 b14 b13 ... b2 b1 b0
# Word 0: x component (16 bits)
# Word 1: y component (16 bits)
# Word 2: z component (16 bits)
# Word 3: speed of sound (16 bits)
# Word 4: ddddxxyy zzcccccc
# d = diagnostic flags, x/y/z = x/y/z-range, c = counter
#
#
# Reference for easy binary/int16 conversions:
# https://www.rapidtables.com/convert/number/decimal-to-binary.html


import numpy as np
import pandas as pd
import xarray as xr
import matplotlib.pyplot as plt
import glob
import os

from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl


# set minimum number of values which must be present in the file to process it
count_threshold = 35000

# flag to save files, folder to which files will be saved
savefiles = False
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# path to data from the MCR sonics at "mn" location
path = "/home/alve/Desktop/Riviera/MAP_subset/data/mn/rohdaten/fast"

# list N1/N3 .raw and files in all subfolders (day of year)
# N1/N3 refers to levels where CSAT3 sonics were used
files_mn_N1 = sorted(glob.glob(os.path.join(path, '**/MN_N1_*.raw')))
files_mn_N3 = sorted(glob.glob(os.path.join(path, '**/MN_N3_*.raw')))

# frequency of CSAT3 sonics
freq = 20

# concatenate the two lists
# TODO process all files, not only a subset
f_raw_all = files_mn_N1[:10] + files_mn_N3[:10]
f_raw_all = files_mn_N1 + files_mn_N3


# %% Function definitions


def range_from_binary(col_1, col_2):
    """
    Function to calculate the multipliction factor for each wind component
    (u_full = u_stored * 0.001 * multiplication_factor); each multiplication
    factor is stored in two bits. Since the input is 2 bit values, there are
    four possible input/output values:
    11: 0.25 (2e-2),
    10: 0.5 (2e-1),
    01: 1 (2e0),
    00: 2 (2e1)

    Parameters
    ----------
    col_1, col_2 : np.array
        columns holding the input bits: each column comtaint only values of 0/1

    Returns
    -------
    mul_falctor : np.array
        Array holding the multiplication factor for one of the wind components,
        values cover the entire 30 min period

    """
    # Convert binary to decimal
    exp = (col_1 * 2**1) + (col_2 * 2**0)
    # Get the multiplication factor
    mul_factor = 2 * 1/(2**exp)
    return mul_factor


def uvwt_from_file(file, count):
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
    # Load file as 5 columns of int16 (2 bytes), little-endian byte order ("<")
    arr_ints = np.frombuffer(raw_bytes, dtype='<5i2', offset=1, count=count)
    # Second, load file as 10 columns of raw binary data ("void format")
    arr_void = np.frombuffer(raw_bytes, dtype='<10B', offset=1, count=count)
    # Cut the needed columns from each loaded array:
    # Columns 1-4 from uvwt (2 bytes each): data needs to be converted further!
    uvwt = (arr_ints[:, 0:4]).astype(np.float64)
    # Columns 9-10 from raw bytes (1 byte each): needed for further conversions
    scalefactors = arr_void[:, 8:]
    # Convert last two columns to pure bits
    scales = np.unpackbits(scalefactors, bitorder='little').reshape(-1, 16)
    # Extract the two bits corresponding to each range separately (see manual)
    scales_x = scales[:, [11, 10]]
    scales_y = scales[:, [9, 8]]
    scales_z = scales[:, [7, 6]]

    # From the raw bytes, get scalefactor bits (x/y/z/ range, see info above)
    ux = range_from_binary(scales_x[:, 0], scales_x[:, 1])
    uy = range_from_binary(scales_y[:, 0], scales_y[:, 1])
    uz = range_from_binary(scales_z[:, 0], scales_z[:, 1])

    # conversions based on the reference manual
    uvwt[:, 0] = uvwt[:, 0] * 0.001 * ux
    uvwt[:, 1] = uvwt[:, 1] * 0.001 * uy
    uvwt[:, 2] = uvwt[:, 2] * 0.001 * uz
    uvwt[:, 3] = (uvwt[:, 3] * 0.001 + 340.0)**2 / 402.7

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


def floor_30min(time):
    # TODO
    """
    calculate dt
    subtract
    get index of orignal time
    return index - use this later for padding
    """
    time_rounded = None
    return time_rounded


def info_from_filename(file):
    # filename
    filename = os.path.split(file)[1]
    # string containing the time information
    datestring = filename[6:-4]
    # proper timestamp denoting when the file begins
    date = pd.to_datetime(datestring, format='%Y_%j_%H%M%S')
    # TODO round down to 30 min
    # https://stackoverflow.com/questions/32723150/rounding-up-to-nearest-30-minutes-in-python

    # get level of the sonic: either 1 (N1) or 2 (N3)
    if filename[3:5] == 'N1':
        loc = 'E2_1'
    elif filename[3:5] == 'N3':
        loc = 'E2_2'

    return loc, date


# %%
counter = 0
for f_raw in f_raw_all:
    with open(f_raw, 'rb') as file:
        # get location and time of file
        loc, date = info_from_filename(f_raw)
        # get filesize in bytes
        size = os.path.getsize(f_raw)
        # get count of measurements: (size/10)-1 since each measurement is
        # 10 bytes and the first measurement is corrupt
        count = int((size / 10) - 1)
        if count < 32400: 
            print('{} - {} - datapoints: {}'.format(date, f_raw[-14:-11], count))
            counter += 1
            continue
        else:
            continue
        # TODO ask Iva: if a lot of data is missing, skip file
        if count <= count_threshold:
            continue
        # load data from the buffer, process it and make and uvwt array
        uvwt = uvwt_from_file(file, count)
        # make a data set from the array, add metadata of variables
        ds = ds_from_uvwt(uvwt, date)

        # add general metadata
        ds.attrs['frequency [Hz]'] = freq
        ds.attrs['sonic tower and level'] = sonic_location[loc]
        ds.attrs['sonic serial number'] = sonic_SN[loc]
        ds.attrs['sonic height [m]'] = sonic_height[loc]
        ds.attrs['sonic location [lat, lon]'] = sonic_latlon[loc]
        ds.attrs['tower altitude [m a.s.l.]'] = height_asl[loc]
        ds.time.attrs['info'] = 'time in CET'

        # save data
        if savefiles:
            # produce output name based on time and location
            output_name = '{}_{}.nc'.format(loc, date.strftime('%Y_%m_%d_%H%M'))
            # save file
            ds.to_netcdf(os.path.join(save_folder, output_name))

print('Counter: {}'.format(counter))
# fig, axes = plt.subplots(nrows=2, ncols=2, figsize=[12,12])
# axes = axes.flatten()
# for i in range(4):
#     axes[i].plot(uvwt[:, i])
# fig.show()
