#!/usr/bin/env python3
# -*- coding: utf-8 -*-

###############################################################################
#
# Read in .raw files, add metadata, save as .nc files with metadata
# Usable only for Uni Basel CSAT3 sonics: location mn, levels 1 and 3 (N1, N3)
# Data from other Basel sonics is stored and processed differently.
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
#
###############################################################################


import numpy as np
import pandas as pd
import xarray as xr
import glob
import os
import datetime as dt

# local imports
from matrix_calibration import get_all_corrections as correct_matrix
from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl


verbose = True

# flag: use the Uni Basel matrix calibration for the CSAT3 sonics
calibrate = True

# flag to save files, folder to which files will be saved
savefiles = True
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'


# path to data from the MCR sonics at "mn" location
path = "/home/alve/Desktop/Riviera/MAP_subset/data/mn/rohdaten/fast"

# list N1/N3 .raw and files in all subfolders (day of year)
# N1/N3 refers to levels where CSAT3 sonics were used at Monte Nuovo
files_mn_N1 = sorted(glob.glob(os.path.join(path, '**/MN_N1_*.raw')))
files_mn_N3 = sorted(glob.glob(os.path.join(path, '**/MN_N3_*.raw')))

# frequency of CSAT3 sonics
freq = 20   # [Hz]

# concatenate the two lists
f_raw_all = files_mn_N1 + files_mn_N3


# %% Function definitions


def range_from_binary(col_1, col_2):
    """
    Function to calculate the multipliction factor for each wind component
    (u_full = u_stored * 0.001 * multiplication_factor); each multiplication
    factor is stored in two bits. Since the input is 2 bit values, there are
    four possible input/output values:
    input bits: multiplication factor
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


def uvwt_from_file(file, count, serial, calibrate=True):
    """
    Function for reading the opened file (buffer) and converting the data to
    a numpy array with u, v, w, T variables. All calculations and processing
    are done within this function.

    Parameters
    ----------
    file : _io.BufferedReader
        opened file of the raw binary data
    count : int
        number of lines holding the data, i.e. number of measurements
    serial : str
        serial number of the CSAT sonic
    calibrate : bool, optional
        Optional flag - should the matrix calibration from Uni Basel be applied
        to the raw data? The default is True.

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

    # conversions based on the reference manual: wind components, temperature
    uvwt[:, 0] = uvwt[:, 0] * 0.001 * ux
    uvwt[:, 1] = uvwt[:, 1] * 0.001 * uy
    uvwt[:, 2] = uvwt[:, 2] * 0.001 * uz
    uvwt[:, 3] = (uvwt[:, 3] * 0.001 + 340.0)**2 / 402.7

    # apply calibration to match the database (30-min averages)
    if calibrate:
        # separate components, turned wind for calibration
        u_corr, v_corr, w_corr = correct_matrix(uvwt[:, 0],         # u
                                                -1 * uvwt[:, 1],    # -v
                                                uvwt[:, 2],         # w
                                                serial)
        # assign components back to the original array, turn v component back
        uvwt[:, 0] = u_corr
        uvwt[:, 1] = -1 * v_corr
        uvwt[:, 2] = w_corr

    return uvwt


def ds_from_uvwt(uvwt_full, date, date_rounded):
    """
    Convert the np data array to a dataset containing metadata

    Parameters
    ----------
    uvwt_full : np.array
        array holding the uvwt values
    date : Timestamp
        timestamp of the beginning of the measurement period
    date_rounded : Timestamo
        Date rounded down to the last previous half-hour, used for filename

    Returns
    -------
    ds : xr.Dataset
        dataset with all the uvwt values, including coordinates

    """

    # if the full field contains a few points more than 36000, cut those
    #   if there are fewer points, nothing happens
    uvwt = uvwt_full[:36000, :]

    # if files don't start at full 30 min, pad in the beginning
    # first, based on time, get number of missing measurements (20 per second)
    if date == date_rounded:
        missing_front = 0
    else:
        missing_front = ((date - date_rounded).seconds) * freq
        # if we'd have more than 18000 points with this padding, pad less
        if missing_front + uvwt.shape[0] > 36000:
            missing_front -= (missing_front + uvwt.shape[0] - 36000)

    # second, add rows of NaNs in the beginning of the file
    uvwt = np.pad(uvwt, ((missing_front, 0), (0, 0)),
                  'constant', constant_values=np.nan)

    # on the other hand, pad to 36000 for shorter files
    missing_end = 36000 - uvwt.shape[0]
    uvwt = np.pad(uvwt, ((0, missing_end), (0, 0)),
                  'constant', constant_values=np.nan)

    # make a dictinary of variables
    data_vars = dict(u=('time', uvwt[:, 0]),
                     v=('time', uvwt[:, 1]),
                     w=('time', uvwt[:, 2]),
                     T=('time', uvwt[:, 3]))
    # make a range of time values: use fixed time periods
    timerange_full = pd.date_range(date_rounded,
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

    # Add info how much files were padded in the beginning/end
    ds.attrs['padded_front'] = missing_front
    ds.attrs['padded_end'] = missing_end

    return ds


def info_from_filename(file):
    """
    Based on the filename, this function determines the location of the sonic
    (i.e. the tower and the level of the sonic on the given tower), together
    with generating the timestamp for the output file.

    One more timestamp s generated: the input time rounded down to the nearest
    half-hour. E.g. for files with timestamp at 16:22 the rounded time will be
    16:00. If the two timestamps are not equal, file is padded with NaNs in the
    beginning of the measurement period.

    Parameters
    ----------
    file : str
        Filename containg the info about location of the sonic and timestamp

    Returns
    -------
    loc : str
        location of the sonic: tower+level
    date : pandas timestamp
        original timestamp parsed from the filename
    date_rounded : pandas timestamp
        timestamp rounded down to the nearest 30 min

    """
    # filename
    filename = os.path.split(file)[1]
    # string containing the time information
    datestring = filename[6:-4]
    # proper timestamp denoting when the file begins
    date = pd.to_datetime(datestring, format='%Y_%j_%H%M%S')

    # get the previous whole half-hour time
    # (some files start e.g. at 16:04 - in that case, make a 16:00 timestamp)
    # Define the rounding period to 30 minutes
    delta = dt.timedelta(minutes=30)
    # Convert the pandas timestamp to python datetime
    pydate = date.to_pydatetime()
    # Round down to nearest 30 min; convert back to pandas timestamp
    date_rounded = pd.to_datetime(pydate - (pydate - dt.datetime.min) % delta)

    # get level of the sonic: either 1 (N1) or 2 (N3)
    if filename[0:5] == 'MN_N1':
        loc = 'E2_1'
    elif filename[0:5] == 'MN_N3':
        loc = 'E2_2'

    return loc, date, date_rounded


# %% Production step: make .nc files

# loop through list of files
for f_raw in f_raw_all:
    with open(f_raw, 'rb') as file:
        # get location and time of file
        loc, date, date_30min_floor = info_from_filename(f_raw)

        # For debugging and seeing conversion progress
        if verbose:
            print(date, f_raw)
        # get filesize in bytes
        size = os.path.getsize(f_raw)
        # get count of measurements: (size/10)-1 since each measurement is
        # 10 bytes and the first measurement is always corrupt
        count = int((size / 10) - 1)

        # load data from the buffer, process it and make and uvwt array
        uvwt = uvwt_from_file(file, count, sonic_SN[loc], calibrate=calibrate)
        # make a data set from the array, add metadata of variables
        ds = ds_from_uvwt(uvwt, date, date_30min_floor)

        # add general metadata
        ds.attrs['frequency [Hz]'] = freq
        ds.attrs['sonic tower and level'] = sonic_location[loc]
        ds.attrs['sonic serial number'] = sonic_SN[loc]
        ds.attrs['sonic height [m]'] = sonic_height[loc]
        ds.attrs['sonic location [lat, lon]'] = sonic_latlon[loc]
        ds.attrs['tower altitude [m a.s.l.]'] = height_asl[loc]
        ds.time.attrs['info'] = 'time in CET'

        # add info about calibration
        if calibrate:
            ds.attrs['calibration applied'] = 'matrix'
        else:
            ds.attrs['calibration applied'] = 'none'

        # save data
        if savefiles:
            # produce output name based on time and location
            output_name = '{}_{}.nc'.format(loc, date_30min_floor.strftime('%Y_%m_%d_%H%M'))
            # save file
            ds.to_netcdf(os.path.join(save_folder, output_name))
