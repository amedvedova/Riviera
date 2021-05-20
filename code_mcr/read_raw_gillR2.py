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
# 2 = record number (Counter): values 0-10000
# 3 = transit count t-b axis 1 msb/lsb: values -10000 - 15000
# 4 = transit count b-t axis 1 msb/lsb: values -10000 - 15000
# 5 = transit count t-b axis 2 msb/lsb: values -10000 - 15000
# 6 = transit Count b-t axis 2 msb/lsb: values -10000 - 15000
# 7 = transit Count t-b axis 3 msb/lsb: values -10000 - 15000
# 8 = transit Count b-t axis 3 msb/lsb: values -10000 - 15000
# [a] = analog inputs msb/lsb
# 9 = end of record HEX "8282" = -32126

import numpy as np
import pandas as pd
import xarray as xr
import matplotlib.pyplot as plt
import glob
import os
import datetime as dt

from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl
from gill_calibrate import get_all_corrections as correct


verbose = True

# constants and utils etc
FREQ = 29.4912e6        # [Hz] Freq of ultrasonic pulse
PATHLENGTH = 0.149      # [m] Distance between sonic heads

# flag to save files, folder to which files will be saved
savefiles = True
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

# frequency of Gill R2 sonics
freq = 37500/1800

# concatenate the two lists
# TODO process all files, not only a subset
n = 50
f_raw_all = files_ag_N2[:n] + files_ag_N4[:n] + files_mn_N4[:n] + \
            files_mn_N5[:n] + files_mn_N7[:n] + files_ro_N2[:n]
# f_raw_all = files_ag_N2 + files_ag_N4 + files_mn_N4 + \
#             files_mn_N5 + files_mn_N7 + files_ro_N2


# %% Function definitions


def tc_from_file(raw_bytes, bytes_per_row):
    """
    Function to read in the raw bytes and try to parse them into an array,
    based on the number of channels (bytes_per_row) - 6 channels usually, 7 if
    a krypton is present as well.

    If this function fails (e.g. missing bytes), a ValueError is raised and
    another function is called, which deals with broken files.

    Parameters
    ----------
    raw_bytes : bytes
        bytes read from the file; to be parsed into a meaningful array
    bytes_per_row : int
        how many bytes there are in each row: depends on the number of analog
        inputs.

    Returns
    -------
    tc : np.array
        array containing 6 transit count columns (2 per sonic axis) and
        possibly humidity data (which is useless since we don't know the units
        and encoding).

    """

    # convert bytes to signed two-byte integers (int16)
    tc = np.frombuffer(raw_bytes, dtype='>i2')
    # reshape the array based on the number of columns
    tc = tc.reshape((-1, int(bytes_per_row/2)))

    # Check if data is correctly recorded
    check_1 = (tc[:, 0] == -32383).all()   # hex(33153) = 8181
    check_2 = (tc[:, -1] == -32126).all()  # hex(33410) = 8282
    if not (check_1 and check_2):
        # If data is not recorded correctly, the second data reading method
        # will be used
        raise ValueError

    # After checks, strip off first two + last column (checks, counter, checks)
    # We'll be left with 6 columns (or 7 if there's a sonic)
    tc = tc[:, 2:-1]

    return tc


def tc_from_corrupt_file(raw_bytes, bytes_per_row):
    """
    Another function to read in the raw bytes and try to parse them into an
    array, based on the number of channels (bytes_per_row) - 6 channels
    usually, 7 if a krypton is present as well.

    Use this function if there's a value error when trying to parse the stream
    of bytes: it works by identifying complete rows which start and end with
    the correct control bytes.

    Parameters
    ----------
    raw_bytes : bytes
        bytes read from the file; to be parsed into a meaningful array
    bytes_per_row : int
        how many bytes there are in each row: depends on the number of analog
        inputs.

    Returns
    -------
    tc : np.array
        array containing 6 transit count columns (2 per sonic axis)

    """

    # There's probably a more elegant way to do this...
    # Split the file into groups of bytes: each group begins with
    # hex(33153) = 8181, ends with hex(33410) = 8282 and has 18/20 bytes

    # Split data records: each has to begin with 8181
    raw_lines = raw_bytes.split((b'\x81\x81'))
    # select lines that end with 8282 and have the correct number of bytes
    select_lines = [line for line in raw_lines if (line[-2:] == b'\x82\x82') &
                    (len(line) == (bytes_per_row - 2))]

    # join these good bytestrings into a stream of bytes again
    cleaned_bytes = b''.join(select_lines)

    # make an array of transit counts from the cleaned data
    # -2 bcs the 8181 bytes are removed now, and /2 bcs two bytes per stream
    channels = int((bytes_per_row - 2) / 2)
    # turn into an array with channels as columns
    tc = np.frombuffer(cleaned_bytes, dtype='>{}i2'.format(channels))

    # Remove first column (counter) and last column (check bytes)
    # we'll be left with 6/7 columns
    tc = tc[:, 1:-1]

    return tc


def uvwt_from_tc(tc, loc):
    """
    Function to calculate the wind speed components and temperature from the
    transit counts.

    First, calculate the "axis velocities" (av) for each axis. From those,
    based on the geometry of the sonic, calculate the raw wind components; the
    values in this calculation are based on the reference manual of the sonic
    (only the Gill R3 reference manual is available as of 2021), as well as on
    the original analysis script provided by Rolang Vogt. The values in the
    manual and the script are identical. These are UNCALBRATED wind components.

    Temperature calculation and correction:
    From the transit counts, uncorrected speed of sound (c) is
    calculated for each axis. Correction due to crosswind is then applied, for
    reference check e.g. see e.g. DOI 10.1007/s10546-015-0010-3, Eq. 10.
    This correction was also applied in the Basel lab analysis script.
    For correction, the UNCALIBRATED wind speed is used (Based on the Basel
    code). Temperature based on speed of sound and crosswind is calculated
    separately for each sonic axis, and then averaged.
    This correction is usually up to ~0.1%.

    Calibration of wind speed components:
    Wind speed along all axes is affected because of flow distortion due to
    transducer shadowing of the path. The distortion is thus direction
    dependent.
    For each sonic, there's a separate calibration file for the u-,
    v-components and there's one common file for all the sonics to calibrate
    the w-component. This is done by calling the "correct" function.

    Parameters
    ----------
    tc : np.array
        array containing the transit counts, possibly also humidity data
        (humidity is not processed due to the lack of documentation)
    loc : str
        location of the sonic, used to extract its serial number

    Returns
    -------
    uvwt_corr : np.array
        calibrated and corrected values of wind speed and temperature

    """
    # Replace values of -10000 (faulty reading on one axis) with NaNs
    # convert integer counts to floats
    tc = tc.astype(np.float)
    # identify correct measurements: rows with all values > -10000
    correct_rows = (tc > -10000).all(axis=1)
    # if there are faulty rows, replace those with zeros
    if np.sum(correct_rows < tc.shape[0]):
        tc = (tc.T * correct_rows).T
        tc[tc == 0] = np.nan

    # 6 channels: ax1 tc1, ax1 tc2, ax2 tc1, ax2 tc2, ax3 tc1, ax3 tc2
    # get axis velocity av: av = 0.5*pathlength*freq*(1/tc1 - 1/tc2)  [m/s]
    av1 = 0.5 * PATHLENGTH * FREQ * (1/tc[:, 0] - 1/tc[:, 1])
    av2 = 0.5 * PATHLENGTH * FREQ * (1/tc[:, 2] - 1/tc[:, 3])
    av3 = 0.5 * PATHLENGTH * FREQ * (1/tc[:, 4] - 1/tc[:, 5])

    # get wind components from axis velocities (based on geometry)
    # these are RAW values: u > 0 points to 150deg, v > 0 points to 240deg
    u = (2*av1 - av2 - av3) / 2.1213
    v = (av2 - av3) / 1.2247
    w = (-av1 - av2 - av3) / 2.1213

    # The Basel lab in their analysis applied a correction
    # The correction ends up being less than 0.01%, why even bother...
    # get speed of sound: separately for each axis, then average
    c1 = 0.5 * PATHLENGTH * FREQ * (1/tc[:, 0] + 1/tc[:, 1])
    c2 = 0.5 * PATHLENGTH * FREQ * (1/tc[:, 2] + 1/tc[:, 3])
    c3 = 0.5 * PATHLENGTH * FREQ * (1/tc[:, 4] + 1/tc[:, 5])

    # for reference: non-corrected temperature calculation
    t = (c1**2 + c2**2 + c3**2)/402.7 / 3

    # get windspeed
    wspd = np.sqrt(u**2 + v**2 + w**2)
    # get components of wspd orthogonal to each path: naming after Basel code
    vu = np.sqrt(wspd**2 - av1**2)
    vv = np.sqrt(wspd**2 - av2**2)
    vw = np.sqrt(wspd**2 - av3**2)
    # get temperature along each axis from speed of sound and correction
    t1 = (c1**2 + vu**2) / 402.7
    t2 = (c2**2 + vv**2) / 402.7
    t3 = (c3**2 + vw**2) / 402.7
    # average temperature: corrected for the cross-wind component
    # corrected temp is always higher than the uncorrected one, obviously
    t_corr = (t1 + t2 + t3)/3

    # extract sonic serial number based on the location: only last 4 digits
    serial = sonic_SN[loc][-4:]
    # calibrate uvw
    u_corr, v_corr, w_corr = correct(u, v, w, serial)

    # make one array from the calculated and corrected values
    # uvwt = np.array([u, v, w, t]).T
    uvwt_corr = np.array([u_corr, v_corr, w_corr, t_corr]).T

    if verbose:
        correction_ratio = 100*np.max(t_corr/t) - 100
        if correction_ratio > 0.09:
            print(correction_ratio)

    return uvwt_corr


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
    # if the full field contains a few points more than 37500, cut those
    #   if there are fewer points, nothing happens in this step
    uvwt = uvwt_full[:37500, :]

    # if files don't start at full 30 min, pad in the beginning
    # first, based on time, get number of missing measurements (10 per second)
    if date == date_rounded:
        missing_front = 0
    else:
        missing_front = int(((date - date_rounded).seconds) * freq)
        # if we'd have more than 18000 points with this padding, pad less
        if missing_front + uvwt.shape[0] > 37500:
            missing_front -= (missing_front + uvwt.shape[0] - 37500)

    # second, add rows of NaNs in the beginning of the file
    uvwt = np.pad(uvwt, ((missing_front, 0), (0, 0)),
                  'constant', constant_values=np.nan)

    # on the other hand, in this step pad shorter files to 37500
    missing_end = 37500 - uvwt.shape[0]
    uvwt = np.pad(uvwt, ((0, missing_end), (0, 0)),
                  'constant', constant_values=np.nan)

    # make a dictinary of variables
    data_vars = dict(u=('time', uvwt[:, 0]),
                     v=('time', uvwt[:, 1]),
                     w=('time', uvwt[:, 2]),
                     T=('time', uvwt[:, 3]))
    # make a range of time values: use fixed time periods
    timerange_full = pd.date_range(date,
                                   freq=str(1/freq)+'S',
                                   periods=37500)
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
    """
    Determines the location (tower + level) and the start date and time from
    the filename

    Parameters
    ----------
    file : str
        Name of the file

    Returns
    -------
    loc : str
        location string denoting tower and level
    date : pd.Timestamp
        start of the 30 min period timestamp

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

    # get location and level of the sonic
    if filename[0:5] == 'AG_N2':
        loc = 'F2_1'
    elif filename[0:5] == 'AG_N4':
        loc = 'F2_2'
    elif filename[0:5] == 'MN_N4':
        loc = 'E2_3'
    elif filename[0:5] == 'MN_N5':
        loc = 'E2_4'
    elif filename[0:5] == 'MN_N7':
        loc = 'E2_5'
    elif filename[0:5] == 'RO_N2':
        loc = 'E1_2'
    else:
        loc = None

    return loc, date, date_rounded


# %%


for f_raw in f_raw_all:
    with open(f_raw, 'rb') as file:
        # get location and time of file
        loc, date, date_30min_floor = info_from_filename(f_raw)
        # get number of analog inputs: 1 at RO_N2, 0 otherwise
        if loc == 'E1_2':
            ai = 1
        else:
            ai = 0
        # get number of bytes in each measurement - depends on analog inputs
        bytes_per_row = 18 + 2*ai

        # get filesize in bytes
        size = os.path.getsize(f_raw)
        # for extremely small files (arbitrarily chosen to circa 15%): skip
        if size < 5000:
            continue

        # get count of measurements: (size/10)-1 since each measurement is
        # 10 bytes and the first measurement is corrupt
        count = int(size / bytes_per_row)

        # load data from the buffer, process it and make and uvwt array
        # read raw bytes from the file
        raw_bytes = file.read()

        # Get the raw transit count array
        try:
            tc = tc_from_file(raw_bytes, bytes_per_row)
            corrupt_flag = 'Raw data contained no errors'
        except ValueError:
            tc = tc_from_corrupt_file(raw_bytes, bytes_per_row)
            corrupt_flag = 'Raw data contained corrupt bytes'

        # if verbose:
        #     print(count, tc.shape)
        # if there are more than 5% or broken data entries, skip the file
        if tc.shape[0] / count < 0.95:
            print('Broken file: {}'.format(os.path.split(f_raw)[1]))
            continue

        if ((tc[:, 0:6] < -10000) | (tc[:, 0:6] > 15000)).any():
            print('Broken file, values out of range')
            continue

        # From the transit counts, get uvwt array
        uvwt = uvwt_from_tc(tc, loc)

        # make a data set from the array, add metadata of variables
        ds = ds_from_uvwt(uvwt, date, date_30min_floor)

        # add general metadata
        ds.attrs['frequency [Hz]'] = 20.83
        ds.attrs['sonic tower and level'] = sonic_location[loc]
        ds.attrs['sonic serial number'] = sonic_SN[loc]
        ds.attrs['sonic height [m]'] = sonic_height[loc]
        ds.attrs['sonic location [lat, lon]'] = sonic_latlon[loc]
        ds.attrs['tower altitude [m a.s.l.]'] = height_asl[loc]
        # add information about whether the original file was corrupt
        ds.attrs['info'] = corrupt_flag

        # save data
        if savefiles:
            # produce output name based on time and location
            output_name = '{}_{}.nc'.format(loc, date.strftime('%Y_%m_%d_%H%M'))
            # save file
            ds.to_netcdf(os.path.join(save_folder, output_name))
