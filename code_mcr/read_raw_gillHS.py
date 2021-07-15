#!/usr/bin/env python3
# -*- coding: utf-8 -*-

###############################################################################
#
# Read in .raw files, add metadata, save as .nc files with metadata.
# Usable only for Uni Basel Gill HS sonic.
# Data from other Basel sonics is stored and processed differently.
#
#
# GILL HS FORMAT:
# 1123445566[ss]77[aa]8
#
# 1 = ID-Byte hex 2 * "BA"
# 2 = status adresse (= counter 1-10)
# 3 = status data
# 4 = wind x
# 5 = wind y
# 6 = wind z
# [ss] = speed of sound
# 7 = absolute temperature
# [aa] = analog input
# 8 = Checksum
#
###############################################################################


import numpy as np
import pandas as pd
import xarray as xr
import glob
import os
import datetime as dt

# local imports
from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl
from matrix_calibration import get_all_corrections as correct_matrix
from krypton_calibration import calibrate_decide as get_rho_decide
from krypton_calibration import calibrate_high as get_rho_high


verbose = True

# %%

# flag: apply the Uni Basel matrix calibration to the Metek sonic raw data?
calibrate = True

# choose calibration for the krypton
# q_setting = 'decide'
q_setting = 'high'

# flag to save files, folder to which files will be saved
savefiles = True
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/basel_sonics_processed/'

# paths to data from the MCR sonics
path = "/home/alve/Desktop/Riviera/MAP_subset/data/mn/rohdaten/fast"


# list all relevant .raw and files in all subfolders (day of year)
join = os.path.join
f_raw_all = sorted(glob.glob(join(path, '**/MN_N8_*.raw')))  # HS E26 m

freq = 20  # [Hz] i.e. 36000 measurements per 30 min
loc = 'E2_6'


# %% Function definitions


def uvwtq_from_file(raw_bytes, bytes_per_row, calibrate=True):
    # serial number of the Gill HS sonic
    serial = 'Gill HS 000046'

    # read lines of 15 bytes based on the Gill HS data format
    # define names and types of each byte / 2 bytes
    dtype = '>i2'
    dtype_array = np.dtype([('idbyte', 'u2'),           # 47802
                            ('status_address', 'B'),    # counter 1-10
                            ('stat_data', 'B'),         # ?
                            ('u', dtype),               # wind: u * 100 [m/s]
                            ('v', dtype),               # wind: v * 100 [m/s]
                            ('w', dtype),               # wind: w * 100 [m/s]
                            ('temp', dtype),            # temperature [C] * 100
                            ('voltage', dtype),         # krypton voltage [mV]
                            ('endbyte', 'B')])          # ?
    # read in array from buffer
    arr = np.frombuffer(raw_bytes, dtype=dtype_array)

    # extract useful values from the array, scale as needed
    u = arr['u'] * 0.01
    v = arr['v'] * 0.01
    w = arr['w'] * 0.01
    t = arr['temp'] * 0.01 + 273.15  # convert to Kelvin
    voltage = arr['voltage'].astype(np.float)

    # calibrate wind speeds
    if calibrate:
        u_corr, v_corr, w_corr = correct_matrix(-1*u, v, w, serial)
    else:
        u_corr = -1 * u
        v_corr = v
        w_corr = w

    # calculate humidity
    if q_setting == 'decide':
        q = get_rho_decide(voltage, serial)
    elif q_setting == 'high':
        q = get_rho_high(voltage, serial)

    # combine into one array
    uvwtq = np.array([-1*u_corr, v_corr, w_corr, t, q]).T

    return uvwtq


def ds_from_uvwtq(uvwt_full, date, date_rounded):
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
    #   if there are fewer points, nothing happens in this step
    uvwt = uvwt_full[:36000, :]

    # if files don't start at full 30 min, pad in the beginning
    # first, based on time, get number of missing measurements (10 per second)
    if date == date_rounded:
        missing_front = 0
    else:
        missing_front = int(((date - date_rounded).seconds) * 36000/1800)
        # if we'd have more than 18000 points with this padding, pad less
        if missing_front + uvwt.shape[0] > 36000:
            missing_front -= (missing_front + uvwt.shape[0] - 36000)

    # second, add rows of NaNs in the beginning of the file
    uvwt = np.pad(uvwt, ((missing_front, 0), (0, 0)),
                  'constant', constant_values=np.nan)

    # on the other hand, in this step pad shorter files to 36000
    missing_end = 36000 - uvwt.shape[0]
    uvwt = np.pad(uvwt, ((0, missing_end), (0, 0)),
                  'constant', constant_values=np.nan)

    # make a dictinary of variables
    data_vars = dict(u=('time', uvwt[:, 0]),
                     v=('time', uvwt[:, 1]),
                     w=('time', uvwt[:, 2]),
                     T=('time', uvwt[:, 3]))
    # make a range of time values: use fixed time periods
    timerange_full = pd.date_range(date,
                                   freq=str(1800/36000)+'S',
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

    # add humidity if present
    if uvwt.shape[1] == 5:
        ds = ds.assign({'q': ('time', uvwt[:, 4])})
        ds.q.attrs = {'units': 'g/m^3',
                      'info': 'water vapor density'}

    # Add info how much files were padded in the beginning/end
    ds.attrs['padded_front'] = missing_front
    ds.attrs['padded_end'] = missing_end

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

    # Gill HS sonic location: only one exists within the Riviera project
    loc = 'E2_6'

    return loc, date, date_rounded


def produce_files(filelist, calibrate=True):
    """
    Production step: each raw file from the filelist is processed, metadata is
    added and the files are saved.

    Parameters
    ----------
    filelist : list
        list of all Gill R2 files to be ananlyzed
    calibration : bool, optional
        Indicates whether the matix calibration should be applied.
        The default is True.

    Returns
    -------
    None.

    """

    for f_raw in filelist:
        # print(f_raw)
        with open(f_raw, 'rb') as file:
            # get location and time of file
            loc, date, date_30min_floor = info_from_filename(f_raw)

            # For debugging and seeing conversion progress
            if verbose:
                print(date, f_raw)

            # get filesize in bytes
            size = os.path.getsize(f_raw)
            # Skip extremely small files (100kB). Complete files are >500 kB.
            if size < 100000:
                if verbose:
                    print('File too short')
                continue

            # load data from the buffer, process it and make and uvwt array
            # read raw bytes from the file
            raw_bytes = file.read()

            # From the transit counts, get uvwt array, applying a calibration
            try:
                uvwtq = uvwtq_from_file(raw_bytes, loc, calibrate=calibrate)
            except ValueError:
                if verbose:
                    print('Faulty file')
                continue

            # make a data set from the array, add metadata of variables
            ds = ds_from_uvwtq(uvwtq, date, date_30min_floor)

            # add general metadata
            ds.attrs['frequency [Hz]'] = 20
            ds.attrs['sonic tower and level'] = sonic_location[loc]
            ds.attrs['sonic serial number'] = sonic_SN[loc]
            ds.attrs['sonic height [m]'] = sonic_height[loc]
            ds.attrs['sonic location [lat, lon]'] = sonic_latlon[loc]
            ds.attrs['tower altitude [m a.s.l.]'] = height_asl[loc]

            # add info about calibration
            if calibrate:
                ds.attrs['calibration applied'] = 'matrix'
            else:
                ds.attrs['calibration applied'] = 'none'

            # save data
            if savefiles:
                # produce output name based on time and location
                output_name = '{}_{}.nc'.format(loc,
                                                date.strftime('%Y_%m_%d_%H%M'))
                # save file
                ds.to_netcdf(os.path.join(save_folder, output_name))
    return


# %% Production step

# mn_N8: Gill HS
produce_files(f_raw_all, calibrate)
