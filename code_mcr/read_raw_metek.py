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
import datetime as dt


from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl

# TODO 
# TODO check with Iva about what to do
# DoY 212 - DoY 214 8:30 OR DoY 216 22:30 saved as summer CET, not CET
# TODO check the daily cycles to determine time?
# DoY 216 - 218 missing anyway
# since there's no distinct daiy cycle on the first few days, this is not
# possible to determine


# flag to save files, folder to which files will be saved
savefiles = True
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# path to data from the MCR sonics at "mn" location
path = "/home/alve/Desktop/Riviera/MAP_subset/data/ro/rohdaten/fast"

# list  .raw and files in all subfolders (day of year)
# N1 refers to the level where the Metek sonic was used
files_ro_N1 = sorted(glob.glob(os.path.join(path, '**/RO_N1_*.raw')))

# frequency of Metek sonic
freq = 10

# TODO process all files, not only a subset
f_raw_all = files_ro_N1[0:183]
# f_raw_all = files_ro_N1

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

    # make an empty storage array; last line is always empty
    uvwt = np.empty((len(lines)-1, 4))
    uvwt.fill(np.nan)

    # loop through the first 18000 lines of each file, re-scale all values
    for i, l in enumerate(lines):
        # check if all values are present
        if len(l) == 41:
            uvwt[i, 0] = float(l[6:12]) / 100.0
            uvwt[i, 1] = float(l[16:22]) / 100.0
            uvwt[i, 2] = float(l[26:32]) / 100.0
            uvwt[i, 3] = float(l[36:42]) / 100.0 + 273.15
        # if not, continue to the next line
        else:
            continue

    return uvwt


def ds_from_uvwt(uvwt_full, date, date_rounded):
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

    # if files don't start at full 30 min, pad in the beginning
    # first, based on time, get number of missing measurements (10 per second)
    if date == date_rounded:
        missing_front = 0
    else:
        missing_front = ((date - date_rounded).seconds) * freq
        # if we'd have more than 18000 points with this padding, pad less
        if missing_front + uvwt.shape[0] > 18000:
            missing_front -= (missing_front + uvwt.shape[0] - 18000)


    # second, add rows of NaNs in the beginning of the file
    uvwt = np.pad(uvwt, ((missing_front, 0), (0, 0)),
                  'constant', constant_values=np.nan)

    # on the other hand, pad to 36000 for shorter files
    missing_end = 18000 - uvwt.shape[0]
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

    # location of the sonic: hardcoded here since there's nly one Metek sonic
    loc = 'E1_1'

    return loc, date, date_rounded


def fix_time(time):
    # at the Maruso-Roasco location, on days of year 212-BLAH the measurements
    # were recorded ic summer CET instead of CET. Afterwards, all time was
    # recorded in CET. For consistency, the first few days
    # are changed to CET. 
    return time


# %%

for f_raw in f_raw_all:
    with open(f_raw, encoding='ascii') as file:
        # get location and time of file from the filename
        loc, date, date_30min_floor = info_from_filename(f_raw)

        # read raw ascii from the file - if there are faulty bits, skip
        try:
            raw_text = file.read()
        # If file cannot be decoded, skip. No useful info in these files anyway
        except UnicodeDecodeError:
            continue

        # use the line separator to identify lines: Each has 41 characters
        lines = raw_text.split('\n')
        # get count of measurements
        count = len(lines)
        print(date, count)
        # load data from the buffer, process it and make and uvwt array
        uvwt = uvwt_from_file(lines)
        # make a data set from the array, add metadata of variables
        ds = ds_from_uvwt(uvwt, date, date_30min_floor)

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
            output_name = '{}_{}.nc'.format(loc, date_30min_floor.strftime('%Y_%m_%d_%H%M'))
            # save file
            ds.to_netcdf(os.path.join(save_folder, output_name))


# fig, axes = plt.subplots(nrows=2, ncols=2, figsize=[12,12])
# axes = axes.flatten()
# for i in range(4):
#     axes[i].plot(uvwt[:, i])
