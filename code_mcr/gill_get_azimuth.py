#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Get azimuth angle from u, v wind components: necessary for calibration
# u > 0: westerly wind
# v > 0: southerly wind
# TODO this is different in the sonic description!
# Meteorological coords: 0 deg = north, 90 deg = east

import numpy as np
import pandas as pd
import xarray as xr
import matplotlib.pyplot as plt
import os


# Debug settings: print errors etc
verbose = True

# Directory where calibration files are stored
dir_calib_files = '/home/alve/Desktop/Riviera/MAP_subset/code_mcr/cal_files'


def get_angle(u, v):
    # TODO
    return


def get_calib_values(serial):
    # load calibration files for the sonic based on the serial number

    # get paths to calibration files
    uv_file = os.path.join(dir_calib_files, '{}rcal.h'.format(serial))
    w_file = os.path.join(dir_calib_files, 'Wcal.h')

    # TODO do we need the 361st value?
    # get the magnitude and direction calibration values for u/v values
    mag_calib = pd.read_csv(uv_file, sep=",", skiprows=2, nrows=36, 
                            usecols=list(np.arange(0, 10)),
                            index_col=None, header=None,
                            engine='python').values.flatten()
    dir_calib = pd.read_csv(uv_file, sep=",", skiprows=41, nrows=36, 
                            usecols=list(np.arange(0, 10)),
                            index_col=None, header=None,
                            engine='python').values.flatten()

    # get the vertical speed calibration values: same for all sonics
    w_up_calib = pd.read_csv(w_file, sep=",|\n", skiprows=1, nrows=72, 
                             index_col=None, header=None,
                             engine='python').values.flatten()
    w_down_calib = pd.read_csv(w_file, sep=",|\n", skiprows=76, nrows=72,
                               index_col=None, header=None,
                               engine='python').values.flatten()
    
    return

"""
00: Figure out where u/v are pointing!
01: Is the calibration done before or after T is calculated?
02: Input: sonic serial number, uvw array
03: Based on serial number of the sonic, determine calilbration file -> format 
    string so that the file is read in automatically. UV calibration different
    for each sonic (serial no. + rcal), W calibration same for all (wcal file)
04: Read in the corresponding calibration files
"""