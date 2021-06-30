#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Get azimuth angle from u, v wind components: necessary for calibration
# u > 0: westerly wind
# v > 0: southerly wind
# Meteorological coords: 0 deg = north, 90 deg = east

import numpy as np
import pandas as pd
import os


# Debug settings: print errors etc
verbose = True

# Directory where calibration files are stored
dir_calib_files = '/home/alve/Desktop/Riviera/MAP_subset/code_mcr/cal_files'


# %%

def get_angle(u, v):
    """
    Calculate the UNCALIBRATED wind direction. Note that the sonic coordinate
    system is LEFT-HANDED and offset by 30 degrees.

    Usually, when dealing with angles, the angle increases conter-clockwise and
    0 degrees = East. However, we want meteorological coordinates: angle
    increases clockwis and 0 degrees = North.

    From the reference manual:
    The formula for the direction calculation is arctan(v/u) - 30 degrees.

    EXAMPLES:
    If u > 0, v = 0, the wind comes from 330 degrees.
    If u = 0, v > 0, the wind comes from 60 degrees.
    If u = v > 0, the wind comes from 15 degrees.

    Parameters
    ----------
    u, v : np.arrays
        uncalibrated u/v wind speeds

    Returns
    -------
    angle : np.array
        array of uncalibrated direction angles, with integer values 0-359

    """

    # get angle as float: arctan2 determines the quadrant correctly
    angle_flt = np.rad2deg(np.arctan2(v, u)) - 30
    # since NaN is a flaot and not an integer, replace nans by 0
    angle_flt[np.isnan(angle_flt)] = 0
    # get integer angle between 0-359: mod corrects negative values (adds 360)
    angle_int = np.mod(np.round(angle_flt), 360).astype(int)

    return angle_int


def get_calibration_values(serial):
    """
    Load calibration files for the sonic based on its serial number

    Parameters
    ----------
    serial : str
        Serial number of the Gill sonic

    Returns
    -------
    dir_calib, mag_calib, w_up_calib, w_down_calib : np.arrays
        Arrays containing direction, magnitude, and vertical velocity
        calibration values

    """

    # get paths to calibration files
    uv_file = os.path.join(dir_calib_files, '{}rcal.h'.format(serial))
    w_file = os.path.join(dir_calib_files, 'Wcal.h')

    # get the magnitude and direction calibration values for u/v values
    # these vales are different for each sonic
    # sonic 0043 has spaces as delimiters, the other have commmas
    if serial == '0043':
        delim = '\s+'
    else:
        delim = ','
    mag_calib = pd.read_csv(uv_file, sep=delim, skiprows=2, nrows=36,
                            usecols=list(np.arange(0, 10)),
                            index_col=None, header=None,
                            engine='python').values.flatten()
    dir_calib = pd.read_csv(uv_file, sep=delim, skiprows=41, nrows=36,
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

    return dir_calib, mag_calib, w_up_calib, w_down_calib


def get_uv_corrections(u, v, dir_calib, mag_calib, angle):
    """
    This function changes the direction and magnitude of the measured u and v
    wind components to account for the effect of the sonic arms being in the
    way of the airflow. First, direction correction (D) is applied, then
    magnitude correction (M).

    Correction factors D and M are deterined based on the uncalibrated angle
    and thus have to be calculated for each measurement separately.

    Direction correction:
    u' = u - Dv
    v' = v + Du
    Magnitude corection:
    u'' = Mu' = M * (u - Dv)
    v'' = Mv' = M * (v + Du)

    Parameters
    ----------
    u, v : np.arrays
        uncalibrated wind speeds
    dir_calib, mag_calib : np.arrays of length 360
        These arays hold correction factors for direction and magnitude, based
        on the uncalibrated angle
    angle : np.array
        Uncalibrated angle: atan(v/u) - 30

    Returns
    -------
    u_corr, v_corr : np.arrays
        calibrated wind speeds

    """
    # use UNCALIBRATED angle for determining both the direction/magnitude
    #   calibration factors: index by the integer angle between 0-359
    D = dir_calib[angle] / 65536
    M = mag_calib[angle] / 65536
    # First, correct the direction, then magnitude
    u_corr = M * (u - D*v)
    v_corr = M * (v + D*u)
    # NaNs are dealt with automatically because of the multiplication by u/v

    return u_corr, v_corr


def get_w_corrections(w, w_up_calib, w_down_calib, angle):
    # Get calibration values for positive/negative w-values
    w_pos = w_up_calib[angle] / 65536
    w_neg = w_down_calib[angle] / 65536
    # Runtimewarning is thrown when comparing with NaNs: ignore it
    with np.errstate(invalid='ignore'):
        # Multiply by w_pos where w>0 is true, multiply by w_neg otherwise
        w_corr = np.where(w > 0, w*w_pos, w*w_neg)

    return w_corr


def get_all_corrections(u, v, w, serial):
    # load calibration tables
    dir_cal, mag_cal, w_up_cal, w_down_cal = get_calibration_values(serial)
    # get direction angle
    angle = get_angle(u, v)
    # calibrate u and v components
    u_corr, v_corr = get_uv_corrections(u, v, dir_cal, mag_cal, angle)
    # calibrate w component
    w_corr = get_w_corrections(w, w_up_cal, w_down_cal, angle)

    return u_corr, v_corr, w_corr
