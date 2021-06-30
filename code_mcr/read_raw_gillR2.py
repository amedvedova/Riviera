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
import glob
import os
import datetime as dt

# local imports
from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl, gill_pathlengths
from gill_calibration import get_all_corrections as correct_gill
from matrix_calibration import get_all_corrections as correct_matrix


# settings: calibration, pathlengths, etc.: use "before cal." for everything!
temperature_correction = 'before_calibration'
# temperature_correction = 'after_calibration'
# temperature_correction = None

# use sanvittore for all "E" locations, default for "F2" location
pathlength_type = 'sanvittore'
# pathlength_type = 'default'

# calibration = 'matrix'
calibration = 'gill'
# calibration = None


verbose = True

# %%
# constants
FREQ = 29.4912e6      # [Hz] Freq of ultrasonic pulse used to get transit count


# flag to save files, folder to which files will be saved
savefiles = True
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# empty the directory
for file in os.listdir(save_folder):
    os.remove(os.path.join(save_folder, file))

# paths to data from the MCR sonics
path_ag = "/home/alve/Desktop/Riviera/MAP_subset/data/ag/rohdaten/fast"
path_mn = "/home/alve/Desktop/Riviera/MAP_subset/data/mn/rohdaten/fast"
path_ro = "/home/alve/Desktop/Riviera/MAP_subset/data/ro/rohdaten/fast"


# list all relevant .raw and files in all subfolders (day of year)
join = os.path.join
files_ag_N2 = sorted(glob.glob(join(path_ag, '**/AG_N2_*.raw')))  # 208 F21
files_ag_N4 = sorted(glob.glob(join(path_ag, '**/AG_N4_*.raw')))  # 160 F22
files_mn_N4 = sorted(glob.glob(join(path_mn, '**/MN_N4_*.raw')))  # 211 E23
files_mn_N5 = sorted(glob.glob(join(path_mn, '**/MN_N5_*.raw')))  # 213 E24
files_mn_N7 = sorted(glob.glob(join(path_mn, '**/MN_N7_*.raw')))  # 212 E25
files_ro_N2 = sorted(glob.glob(join(path_ro, '**/RO_N2_*.raw')))  # 043 E12


# concatenate the two lists
# TODO process all files, not only a subset
# TODO check if the files are as the ones in the database: correct settings?
n = 200
if calibration == 'matrix':
    # matrix calibrated: 160, 212, 43
    f_raw_all = files_ag_N4[n:n+24] + files_mn_N7[n:n+24] + files_ro_N2[n:n+24]
elif calibration == 'gill':
    # gill calibrated: 211, 213, 208
    f_raw_all = files_ag_N2[n:n+24] + files_mn_N4[n:n+24] + files_mn_N5[n:n+24]
else:
    f_raw_all = files_ag_N4[n:n+24] + files_mn_N7[n:n+24] + files_ro_N2[n:n+24] + \
                files_ag_N2[n:n+24] + files_mn_N4[n:n+24] + files_mn_N5[n:n+24]

# f_raw_all = files_ag_N2 + files_ag_N4 + files_mn_N4 + \
#             files_mn_N5 + files_mn_N7 + files_ro_N2


# f_raw_all = files_ag_N4[n:n+24] + files_mn_N7[n:n+24] + files_ro_N2[n:n+24] + \
#                 files_ag_N2[n:n+24] + files_mn_N4[n:n+24] + files_mn_N5[n:n+24]

# f_raw_all = files_ag_N4[n:n+24] + files_mn_N7[n:n+24] + files_ro_N2[n:n+24]


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


def tc_from_corrupt_file(raw_bytes, bytes_per_row, count):
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

    # Check: if there are more than 10% of fauty rows, return None
    # Run this check before we insert NaNs to missing measurements
    if tc.shape[0] / count < 0.90:
        if verbose:
            print('Broken file: {}'.format(os.path.split(f_raw)[1]))
        return None

    # Remove first column (counter) and last column (check bytes)
    # we'll be left with 6/7 columns
    tc = tc[:, 1:-1]

    return tc


def uvwt_from_tc(tc, loc, calibration='matrix',
                 pathlength_type='sanvittore',
                 temperature_correction=None):
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
    # extract sonic serial number based on the location: only last 4 digits
    serial = sonic_SN[loc]

    # Replace values of -10000 (faulty reading on one axis) with NaNs:
    # first identify correct measurements: rows with all values > -10000
    correct_rows = (tc > -10000).all(axis=1)
    # change type to float
    tc = tc.astype(np.float)
    # if there are faulty rows, replace those with zeros
    if np.sum(correct_rows < tc.shape[0]):
        tc = (tc.T * correct_rows).T
        tc = np.where(tc == 0, np.nan, tc)

    # get pathlengths for the given Gill sonic, or use defaults
    if pathlength_type == 'sanvittore':
        pathlengths = gill_pathlengths[loc]
    elif pathlength_type == 'default':
        pathlengths = [0.149, 0.149, 0.149]

    # 6 channels: ax1 tc1, ax1 tc2, ax2 tc1, ax2 tc2, ax3 tc1, ax3 tc2
    # get axis velocity av: av = 0.5*pathlength*freq*(1/tc1 - 1/tc2)  [m/s]
    av1 = 0.5 * pathlengths[0] * FREQ * (1/tc[:, 0] - 1/tc[:, 1])
    av2 = 0.5 * pathlengths[1] * FREQ * (1/tc[:, 2] - 1/tc[:, 3])
    av3 = 0.5 * pathlengths[2] * FREQ * (1/tc[:, 4] - 1/tc[:, 5])

    # get wind components from axis velocities (based on geometry)
    # these are RAW values: u > 0 points to 150deg, v > 0 points to 240deg
    u = (2*av1 - av2 - av3) / 2.1213
    v = (av2 - av3) / 1.2247
    w = (-av1 - av2 - av3) / 2.1213

    # apply corrections
    if calibration == 'matrix':
        # calibrate uvw with matrix calibration files: needs full serial no.
        u_corr, v_corr, w_corr = correct_matrix(u, v, w, serial)
    elif calibration == 'gill':
        # calibrate uvw with manufacturer files: needs only R2 number
        u_corr, v_corr, w_corr = correct_gill(u, v, w, serial[-4:])
    else:
        # apply no corrections, use raw values
        u_corr = u
        v_corr = v
        w_corr = w

    # get speed of sound: separate for each axis
    # The Basel lab in their analysis applied a cross-wind correction
    c1 = 0.5 * pathlengths[0] * FREQ * (1/tc[:, 0] + 1/tc[:, 1])
    c2 = 0.5 * pathlengths[1] * FREQ * (1/tc[:, 2] + 1/tc[:, 3])
    c3 = 0.5 * pathlengths[2] * FREQ * (1/tc[:, 4] + 1/tc[:, 5])

    if temperature_correction is None:
        # non-corrected temperature calculation
        t = (c1**2 + c2**2 + c3**2)/402.7 / 3
    else:
        if temperature_correction == 'before_calibration':
            # get windspeed from raw wind components
            wspd = np.sqrt(u**2 + v**2 + w**2)
        elif temperature_correction == 'after_calibration':
            # get windspeed from corrected wind components
            wspd = np.sqrt(u_corr**2 + v_corr**2 + w_corr**2)

        # get components of wind speed orthogonal to each path
        vu = np.sqrt(wspd**2 - av1**2)
        vv = np.sqrt(wspd**2 - av2**2)
        vw = np.sqrt(wspd**2 - av3**2)
        # get temperature along each axis from speed of sound and correction
        t1 = (c1**2 + vu**2) / 402.7
        t2 = (c2**2 + vv**2) / 402.7
        t3 = (c3**2 + vw**2) / 402.7
        # average temperature: corrected for the cross-wind component
        # corrected temp is always higher than the uncorrected one, obviously
        t = (t1 + t2 + t3)/3

    # make one array from the calculated and corrected values
    uvwt_corr = np.array([u_corr, v_corr, w_corr, t]).T

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
        missing_front = int(((date - date_rounded).seconds) * 37500/1800)
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
                                   freq=str(1800/37500)+'S',
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
        # Skip extremely small files (20kB). Complete files are >500 kB.
        if size < 20000:
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
            tc = tc_from_corrupt_file(raw_bytes, bytes_per_row, count)
            corrupt_flag = 'Raw data contained corrupt bytes'

            # if there are more than 10% or broken data entries, None is
            #   returned by the function and the file is skipped
            if tc is None:
                continue

        # From the transit counts, get uvwt array, applying a calibration
        uvwt = uvwt_from_tc(tc, loc,
                            calibration=calibration,
                            pathlength_type=pathlength_type,
                            temperature_correction=temperature_correction)

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
            output_name = '{}_{}.nc'.format(loc,
                                            date.strftime('%Y_%m_%d_%H%M'))
            # save file
            ds.to_netcdf(os.path.join(save_folder, output_name))
