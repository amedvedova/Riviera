#!/usr/bin/env python3
# -*- coding: utf-8 -*-

###############################################################################
#
# Krypton info from the original .pro code:
#
# 1094 (Gill HS, mn_N8):
#     path_len = 0.999
#     clean windows:
#         V0f = 3538  &  Kwf = -0.135  full range
#         V0l = 3361  &  Kwl = -0.126  low range
#         V0h = 3798  &  Kwh = -0.140  high range
#     scaled windows:
#         V0fs = 2616  &  Kwfs = -0.140  full range
#         V0ls = 2590  &  Kwls = -0.138  low range
#         V0hs = 2670  &  Kwhs = -0.141  high range
#
#
# 1199 (Gill R2, ro_N2):
#     path_len = 1.311
#     clean windows:
#         V0f = 5001  &  Kwf = -0.146  full range
#         V0l = 5221  &  Kwl = -0.152  low range
#         V0h = 4850  &  Kwh = -0.144  high range
#     scaled windows:
#         V0fs = 3723  &  Kwfs = -0.143  full range
#         V0ls = 4000  &  Kwls = -0.144  low range
#         V0hs = 3390  &  Kwhs = -0.138  high range
#
#
# The original code only ever used the pathlength and the "clean window values
# The "scaled window" are just for reference in case they're ever needed.
#
###############################################################################


import numpy as np


# Debug settings: print errors etc
verbose = True


# %%

# Calibration values for the two Basel kryptons: both for CLEAN windows
# Gill R2, ro_N2:
krypton_1199 = {'path_len': 1.311,
                'V0f': 5001, 'Kwf': -0.146,  # full range
                'V0l': 5221, 'Kwl': -0.152,  # low range
                'V0h': 4850, 'Kwh': -0.144}  # high range


# Gill HS, mn_N8:
krypton_1094 = {'path_len': 0.999,
                'V0f': 3538, 'Kwf': -0.135,  # full range
                'V0l': 3361, 'Kwl': -0.126,  # low range
                'V0h': 3798, 'Kwh': -0.140}  # high range


def calibrate_decide(voltage, serial):
    """
    This function converts the voltage of the kryptons into water vapor
    density, using the calibration coefficients specific to each krypton.

    At first, calibration using the "full range" coefficients is applied. Based
    on its results, either low/high range coefficients are used, and this array
    is returned. The name of this function comes from this "decision making
    process".

    Parameters
    ----------
    voltage : array
        input voltage, values 0-5000 [mV].
    serial : str
        serial number of the SONIC accompanying the krypton

    Returns
    -------
    rho : array
        water vapor density

    """
    # Based on the SONIC serial number, get the Krypton calibration coeffs
    if serial == 'Gill R2A 0043':
        coeffs = krypton_1199
    elif serial == 'Gill HS 000046':
        coeffs = krypton_1094

    # make a storage array
    rho = np.zeros_like(voltage)

    # see the percentage of wrong measurements
    num_corrupt_values = (voltage < 0).sum() / len(voltage)
    # after the original script: set negative voltages to nan
    voltage[voltage <= 0] = 0.01
    # if too many values are corrupt, fill all with nans and return
    if num_corrupt_values > 0.2:
        rho.fill(np.nan)
        return rho
    else:

        # get rho using full range coeffs
        XKw = coeffs['path_len'] * coeffs['Kwf']
        logV0 = np.log(coeffs['V0f'])
        rho_temp = (np.log(voltage) - logV0) / XKw

        # determine new coeffs based on the "temporary" values
        if np.mean(rho_temp) > 9:
            if verbose:
                print('high')
            XKw = coeffs['path_len'] * coeffs['Kwh']
            logV0 = np.log(coeffs['V0h'])
        else:
            if verbose:
                print('low')
            XKw = coeffs['path_len'] * coeffs['Kwl']
            logV0 = np.log(coeffs['V0l'])
        # re-calculate rho with these coefficients
        rho = (np.log(voltage) - logV0) / XKw

    return rho


def calibrate_high(voltage, serial):
    """
    This function converts the voltage of the kryptons into water vapor
    density, using the calibration coefficients specific to each krypton.

    High range coefficients are always used, and this array is returned - thus
    the function name.

    Parameters
    ----------
    voltage : array
        input voltage, values 0-5000 [mV].
    serial : str
        serial number of the SONIC accompanying the krypton

    Returns
    -------
    rho : array
        water vapor density

    """
    # Based on the SONIC serial number, get the Krypton calibration coeffs
    if serial == 'Gill R2A 0043':
        coeffs = krypton_1199
    elif serial == 'Gill HS 000046':
        coeffs = krypton_1094

    # make a storage array
    rho = np.zeros_like(voltage)

    # see the percentage of wrong measurements
    num_corrupt_values = (voltage <= 0).sum() / len(voltage)
    # after the original script: set negative voltages to nan
    voltage[voltage <= 0] = 0.01
    # if too many values are corrupt, fill all with nans and return
    if num_corrupt_values > 0.2:
        rho.fill(np.nan)
        return rho
    # if enough values are okay:
    else:
        # get "high range" coefficients
        XKw = coeffs['path_len'] * coeffs['Kwh']
        logV0 = np.log(coeffs['V0h'])
        # calculate density
        rho = (np.log(voltage) - logV0) / XKw

    return rho