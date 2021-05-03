#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Calculations of temperature and humidity

import numpy as np
import pandas as pd
import os
from sonic_metadata import height_asl, sonic_height, krypton_calibrations


# %% FUNCTIONS FOR MAKING PARTS OF THE DATASET

def make_namestrings(fname):
    # parse filename for time information
    loc = fname[0]       # sonic identification letter
    DoY = fname[1:4]     # day of year
    hh = fname[4:6]      # hour of day

    # get the date+time in a proper datetime format
    datestring = '1999-{}-{}'.format(DoY, hh)
    date = pd.to_datetime(datestring, format='%Y-%j-%H')

    # create a name for the output file: location + date + time
    output_name = '{}_{}.nc'.format(loc, date.strftime('%Y_%m_%d_%H%M'))

    return loc, date, output_name


def make_time_arrays(init_time, freq):
    # Time array with the sonic frequency for high frequency variables
    timerange_full = pd.date_range(init_time,
                                   freq=str(1/freq)+'S',
                                   periods=75000)
    # Time array with 30 min timestamps for low frequency variables
    timerange_30min = pd.date_range(init_time,
                                    freq='1800S',
                                    periods=2)
    return timerange_full, timerange_30min


def prepare_ds_no_ref_data(arr, timerange_full, date):

    # Information about how temperature was calculated
    T_info = 'Temperature calculated WITHOUT reference data, only from speed of sound.'

    # Data variables to make the dataset
    data_vars = dict(u=('time', arr[:, 0]),
                     v=('time', arr[:, 1]),
                     w=('time', arr[:, 2]),
                     T=('time', arr[:, 3]))
    coords = dict(time=timerange_full,
                  time_1h=date)
    return data_vars, coords, T_info
