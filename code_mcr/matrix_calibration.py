#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# TODO

import numpy as np
import pandas as pd
import os


# Debug settings: print errors etc
verbose = True

# Directory where calibration files are stored
dir_calib_files = '/home/alve/Desktop/Riviera/MAP_subset/code_mcr/cal_files'


# %%


def get_calibration_values(serial):
    # get a serial-number-based string to load matrix calibration files
    if 'Gill R2' in serial:
        serno = 'S{}'.format(serial[-4:].lstrip('0'))
    elif 'Metek' in serial:
        serno = 'M9903006'
    elif 'CSAT' in serial:
        serno = 'C{}'.format(serial[-5:-2])
    elif 'Gill HS' in serial:
        serno = 'HS46'

    # get paths to calibration files
    p_file = os.path.join(dir_calib_files, 'p_{}.004'.format(serno))
    n_file = os.path.join(dir_calib_files, 'n_{}.004'.format(serno))

    # column names
    names = ['auu', 'auv', 'auw', 'au0', 'avu', 'avv',
             'avw', 'av0', 'awu', 'awv', 'aww', 'aw0']

    # get the plus/minus calibration files for each sonic, shape 90 x 12
    delim = '\s+'
    p_calib = pd.read_csv(p_file, sep=delim, skiprows=2, skipinitialspace=True,
                          index_col=None, header=None, names=names,
                          nrows=90, engine='python')
    n_calib = pd.read_csv(n_file, sep=delim, skiprows=2, skipinitialspace=True,
                          index_col=None, header=None, names=names,
                          nrows=90, engine='python')

    return p_calib, n_calib


def get_angle(u, v, gillR2=False):
    """
    Calculate the UNCALIBRATED wind direction.

    Gill R2:
    --------
    Note that the Gill R2 sonic coordinate system is LEFT-HANDED and offset
    by 30 degrees (see examples), i.e. raw positive u points towards 150 deg,
    raw v points towards 240 deg w.r.t. the "north arrow" on the sonic.

    Usually, when dealing with angles, the angle increases counter-clockwise
    and 0 degrees = East. However, we want meteorological coordinates: angle
    increases clockwise and 0 degrees = North.

    From Gill R2 reference manual:
    The formula for the direction calculation is arctan(v/u) - 30 degrees.

    EXAMPLES:
    If u > 0, v = 0, the wind comes from 330 degrees.
    If u = 0, v > 0, the wind comes from 60 degrees.
    If u = v > 0, the wind comes from 15 degrees.


    Metek / Gill HS / CSAT3:
    ------------------------
    For these sonics, the function takes input in a coordinate system where
    positive u points south, i.e. it is a northerly wind, and positive v points
    west, i.e. it is and easterly wind. This is a left-handed coordinate
    system, i.e. u x v = -w (u cross v).

    EXAMPLES:
    If u > 0, v = 0, the wind comes from 0 degrees.
    If u = 0, v > 0, the wind comes from 90 degrees.
    If u = v > 0, the wind comes from 45 degrees.


    This way we calculate the meteorological angle the same way for all sonics,
    but for Gill R2 we needto turn it by additional 30 degrees.


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
    angle_flt = np.rad2deg(np.arctan2(v, u))
    # if the sonic is Gill R2, subtract additional 30 degrees
    if gillR2:
        angle_flt = angle_flt - 30
    # since NaN is a flaot and not an integer, replace nans by 0
    angle_flt[np.isnan(angle_flt)] = 0
    # get integer angle between 0-359: mod corrects negative values (adds 360)
    angle_int = np.mod(np.round(angle_flt), 360).astype(int)

    return angle_int


def calibrate(u, v, w, p_calib, n_calib, angle):
    # Get indices where w is positive/negative
    w_pos = w > 0
    w_neg = w < 0

    # make storage arrays for corrected values
    u_corr = np.zeros_like(u)
    v_corr = np.zeros_like(v)
    w_corr = np.zeros_like(w)

    # select values of p/n_calib based on angle of each measurement
    p = p_calib.loc[list(angle)].values
    n = n_calib.loc[list(angle)].values

    # Get corrected u/v/w values where w > 0
    u_corr[w_pos] = u[w_pos] * p[w_pos, 0] + v[w_pos] * p[w_pos, 1] + \
                    w[w_pos] * p[w_pos, 2] + p[w_pos, 3]
    v_corr[w_pos] = u[w_pos] * p[w_pos, 4] + v[w_pos] * p[w_pos, 5] + \
                    w[w_pos] * p[w_pos, 6] + p[w_pos, 7]
    w_corr[w_pos] = u[w_pos] * p[w_pos, 8] + v[w_pos] * p[w_pos, 9] + \
                    w[w_pos] * p[w_pos, 10] + p[w_pos, 11]
    # Get corrected u/v/w values where w < 0
    u_corr[w_neg] = u[w_neg] * n[w_neg, 0] + v[w_neg] * n[w_neg, 1] + \
                    w[w_neg] * n[w_neg, 2] + n[w_neg, 3]
    v_corr[w_neg] = u[w_neg] * n[w_neg, 4] + v[w_neg] * n[w_neg, 5] + \
                    w[w_neg] * n[w_neg, 6] + n[w_neg, 7]
    w_corr[w_neg] = u[w_neg] * n[w_neg, 8] + v[w_neg] * n[w_neg, 9] + \
                    w[w_neg] * n[w_neg, 10] + n[w_neg, 11]

    return u_corr, v_corr, w_corr


def get_all_corrections(u, v, w, serial):
    # load calibration tables
    p_calib, n_calib = get_calibration_values(serial)

    # for determining the angle, we need to know if the sonic is a Gill R2
    if 'Gill R2' in serial:
        gillR2 = True
    else:
        gillR2 = False

    # get direction angle
    angle = get_angle(u, v, gillR2=gillR2)
    # round angle down to 4-degree steps, results in 90 values (0 to 89)
    angle_90 = (angle - angle % 4) / 4

    # calibrate components
    u_corr, v_corr, w_corr = calibrate(u, v, w, p_calib, n_calib, angle_90)

    return u_corr, v_corr, w_corr
