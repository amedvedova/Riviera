#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Read in .SLT files, add metadata, save as .nc files with metadata
# Usable only for ETH sonics with sampling frequency of 20.82 Hz
# there are 7 of these: with labels a, b, c, d, e, f, g (given by ETH)
# data from sonic h stored in a different format

import numpy as np
import pandas as pd
import xarray as xr
import glob
import os

# flag to save files, input folder, output folder
savefiles = False
input_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics_hourly/'
output_folder = '/home/alve/Desktop/Riviera/MAP_subset/data/eth_sonics_daily/'

# make string of dates when data is available
dates = pd.date_range(start='30/07/1999', end='28/10/1999', freq='1D')


# list all .nc files for input
files_all = sorted(os.listdir(input_folder))

dates = dates[20:22]
# loop through stations by letter
for letter in 'ACF':
    for date in dates:
        # make a string from the timestamp, used for selecting files
        date_string = date.strftime(format='%Y%m%d')
        # make a string which is in the name of all files of the station+day
        name_template = '{}-{}'.format(letter, date_string)
        # select files from a given station+day, following the name template
        files_selected = [os.path.join(input_folder, f) for f in files_all 
                          if name_template in f]
        print(name_template)
        print(files_selected)
        print(' ')

        daily_file = xr.open_mfdataset(files_selected)
        # merge with NaN in times?
        daily_file.to_netcdf(os.path.join(output_folder, name_template+'.nc'))
