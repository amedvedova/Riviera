#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Read in .SLT files, add metadata, save as .nc files with metadata.
# Usable only for ETH sonics with sampling frequency of 20.83 Hz
# There are 7 of these: with labels a, b, c, d, e, f, g (labels given by ETH).
# Data from sonic h stored in a different format.

import numpy as np
import xarray as xr
import glob
import os

# local imports
from utils import make_namestrings, make_time_arrays, get_reference_data, \
                  prepare_ds_with_ref_data, prepare_ds_no_ref_data, get_rho_MCR
from sonic_metadata import sonic_location, sonic_height, sonic_SN, \
                           sonic_latlon, height_asl, \
                           krypton_SN, krypton_height, krypton_calibrations


verbose = True

# flag to save files, output folder
savefiles = True
save_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics_processed/'

# path to all data from ETH sonics: sorted in subfolders by day of year
path = "/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics"
# list all .SLT files in all subfolders
files_all = sorted(glob.glob(os.path.join(path, '**/*.SLT')))


# loop through all files, read out and store the useful data file by file
for filename in files_all:

    # get only the filename within the folder, without the full path
    fname = os.path.split(filename)[1]

    # get location (sonic letter), initial time of file, name for saving output
    loc, date, output_name = make_namestrings(fname)

    # if .nc files were already created, skip doing that again
    # if savefiles:
    #     if output_name in os.listdir(save_folder):
    #         continue

    # For debugging and seeing conversion progress
    if verbose:
        print(date, filename)

    # make a range of time values: use fixed time periods
    timerange_full, timerange_30min = make_time_arrays(date)

    # open file for reading ('r' flag)
    fopen = open(filename, 'r')
    # read SLT data into one vector
    arr = np.fromfile(fopen, dtype=np.int16)
    # change data type to support more precision
    arr = arr.astype(np.float64)

    # get the number of columns in data:
    # at sonics A/C there are also kryptons (in the 5th channel)
    if loc in ['A', 'C']:
        channels = 5
    # no kryptons at these sonics (only 4 channels)
    elif loc in ['B', 'D', 'E', 'G']:
        channels = 4
    # krypton + 2 additional analogue channels for T and q (7 channels)
    # the two extra channels are ignored
    elif loc in ['F']:
        channels = 7

    # reshape the data array based on number of channels:
    arr = np.reshape(arr, (-1, channels))
    # first line is a useless header: get rid of it
    arr = arr[1:, :]

    # if the array is too long (case for only few files) - cut it
    if arr.shape[0] > 75000:
        arr = arr[:75000, :]
    # pad array to length 750000 if there are fewer values (pad to full hour)
    arr = np.pad(arr, ((0, 75000-arr.shape[0]), (0, 0)),
                 'constant', constant_values=np.nan)

    # get reference data, stored in 30 min intervals
    # if available, the file stored has rh, temperature and pressure - needed
    # to get specific humidity and vapor pressure. If not available: None.
    ref_data = get_reference_data(path, loc, fname, timerange_30min)

    # prepare data to make the dataset, based on reference data availability
    # get hourly averages, if available
    if ref_data is not None:
        data_vars, coords = prepare_ds_with_ref_data(arr,
                                                     timerange_full,
                                                     timerange_30min,
                                                     date,
                                                     loc,
                                                     ref_data)
    # If reference data is not available
    else:
        data_vars, coords = prepare_ds_no_ref_data(arr,
                                                   timerange_full,
                                                   date,
                                                   loc)

    # Make dataset
    ds = xr.Dataset(coords=coords)
    ds = ds.expand_dims('time_1h')
    ds = ds.assign(data_vars)

    # Add attributes to the variables
    ds.u.attrs = {'units': 'm/s'}
    ds.v.attrs = {'units': 'm/s'}
    ds.w.attrs = {'units': 'm/s'}
    ds.T.attrs = {'units': 'K',
                  'info': 'sonic (virtual) temperature'}

    # Add general metadata
    ds.attrs['frequency [Hz]'] = 20.83

    # sonic metadata
    ds.attrs['sonic tower and level'] = sonic_location[loc]
    ds.attrs['sonic serial number'] = sonic_SN[loc]
    ds.attrs['sonic height [m]'] = sonic_height[loc]
    ds.attrs['sonic location [lat, lon]'] = sonic_latlon[loc]
    ds.attrs['tower altitude [m a.s.l.]'] = height_asl[loc]

    # if voltage data is available, get water vapor density from it
    if arr.shape[1] > 4:
        # get voltage
        voltage = arr[:, 4]
        # calculate water vapor density after the MCR procedure
        krypton_coeffs = krypton_calibrations[loc]
        rho_dry, rho_wet = get_rho_MCR(krypton_coeffs, voltage)
        # add raw voltage and calculated water vapor densities to the dataset
        ds = ds.assign({'voltage': ('time', voltage)})
        ds = ds.assign({'rho_dry': ('time', rho_dry)})
        ds = ds.assign({'rho_wet': ('time', rho_wet)})
        # add the attributes to the dataset and variables
        ds.attrs['krypton serial number'] = krypton_SN[loc]
        ds.attrs['krypton height'] = krypton_height[loc]
        ds.voltage.attrs = {'units': 'mV',
                            'info': 'raw voltage used to calculate humidity and water vapor density, see the "calibration_coeffs" attribute',
                            'calibration_coeffs': krypton_coeffs,
                            'calibration_coeffs_info': 'Calibration coefficients for the given Krypton, in the order: [B0, B1, B2, k_w_dry, k_w_wet, ln_V0_dry, ln_V0_wet, tube_separation_x]'}
        ds.rho_dry.attrs = {'units': 'g/m^3',
                            'info': 'water vapor density calculated using the "dry range" calibration coefficients'}
        ds.rho_wet.attrs = {'units': 'g/m^3',
                            'info': 'water vapor density calculated using the "wet range" calibration coefficients'}

    # add reference data+corresponding metadata if it exists
    if ref_data is not None:
        # add absolute temperature metadata
        if 'T_abs' in ds.variables:
            ds.T_abs.attrs = {'units': 'K',
                              'info': 'Absolute temperature (calculation takes humidity into account)'}

        # Add 30 min + 1 hour variable metadata
        ds.T_30min.attrs = {'units': 'K',
                            'info': '30-min average of temperature, loaded from reference file'}
        ds.rh_30min.attrs = {'units': '%',
                             'info': '30-min average of relative humidity, loaded from reference file'}
        ds.p_30min.attrs = {'units': 'Pa',
                            'info': 'Calculated from the reference pressure and temperature in the reference file'}
        ds.e_1h.attrs = {'units': 'Pa',
                         'info': 'Hourly average of vapor pressure, calculated from pressure, temperature and humidity'}
        ds.rho_wv_1h.attrs = {'units': 'g/m^3',
                              'info': 'Hourly average of water vapor density'}
        ds.rho_air_1h.attrs = {'units': 'g/m^3',
                               'info': 'Hourly average of air density'}

        # if there's a krypton present AND reference data is available
        # add the humidity data + metadata to the dataset
        if 'q_ETH' in ds.variables:
            # add units, krypton metadata (height and serial number)
            ds.q_ETH.attrs = {'units': 'kg/kg',
                              'info': 'Specific humidity, converted from Krypton voltage using the ETH conversion script'}

    # save files
    if savefiles:
        ds.to_netcdf(os.path.join(save_folder, output_name))
