#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Calculations of temperature and humidity

import numpy as np
import pandas as pd
import os
from sonic_metadata import height_asl, sonic_height, krypton_calibrations, \
                           sonic_output_name


# %% FUNCTIONS FOR MAKING PARTS OF THE DATASET

def make_namestrings(fname):
    """
    Parse the file name in order to determine the location of the sonic,
    starting date+time of the measurement, and make the name of the outpt file

    Parameters
    ----------
    fname : str
        name of the input filem without the full path

    Returns
    -------
    loc : str
        sonic location + level
    date : pd.Timestamp
        beginning of measurement period
    output_name : TYPE
        name of the output file - contains location and date as well

    """
    # parse filename for time information
    loc = fname[0]       # sonic identification letter, after ETH convention
    DoY = fname[1:4]     # day of year
    hh = fname[4:6]      # hour of day
    mm = fname[6:8]      # minutes of hour

    # get the date+time in a proper datetime format
    datestring = '1999-{}-{}{}'.format(DoY, hh, mm)
    date = pd.to_datetime(datestring, format='%Y-%j-%H%M')

    # create a name for the output file: location + date + time
    output_name = '{}_{}.nc'.format(sonic_output_name[loc],
                                    date.strftime('%Y_%m_%d_%H%M'))

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


def get_reference_data(path, loc, fname, time_index):
    '''
    If available, get reference data, stored in 30 min intervals. There are six
    variables, 2 entries per hour, in each file: local temperature, local rh,
    reference pressure and reference temperature: the reference values are
    measured at the Bosco di Sotta station. Two more columns hold rain and net
    radiation data which we don't need here.

    Reference data is NOT available for the entire campaign period.

    Parameters
    ----------
    path : str
        path to data
    loc : str
        letter denoting the sonic location
    fname : str
        filename: full path to the file where high frequency data is stored.
        The name pattern of reference data corresponds to the name of the
        reference data.
    time_index : pd.DateTimeIndex
        pandas index denoting times when the two 30-min periods are beginning

    Returns
    -------
    ref_data : pd.DataFrame
        reference data: local temperature, relative humidity and pressure, and
        reference pressure and temperature.

    '''

    # get a reference file corresponding to the high frequency variables
    file_ref = os.path.join(path, 'ref', loc,
                            '{}.ref'.format(os.path.splitext(fname)[0]))

    # if the file exists, load it and extract reference data
    if os.path.exists(file_ref):
        # load data, name columns
        # p_ref and t_ref measured only at Bosco di Sotta, used to calculate
        #   pressure at all other stations, based on the altitude difference
        ref_data = pd.read_csv(file_ref,
                               sep='\s+',
                               usecols=[0, 1, 2, 3],
                               names=['t', 'rh', 'p_ref', 't_ref'])
        # set index of reference data to the timestamp they correspond to
        ref_data.index = time_index.values
        # add local pressure data
        ref_data['p'] = get_local_pressure(loc, ref_data.t_ref, ref_data.p_ref)
        return ref_data
    # if the corresponding reference file does not exist, return None
    else:
        return None


def get_hourly_vars(df):
    '''
    The original analysis script (edi2000v2.f) works with hourly averages in
    order to calculate temperature from the speed of sound. This function is
    used to get hourly averages from half-hourly values.

    Parameters
    ----------
    df : pd.Datafrare
        pandas dataframe with the half-hourly values of temperature, relative
        humidity and pressure - these measurements are NOT from the sonic, but
        from other instruments.

    Returns
    -------
    t_h, rh_h, p_h, e_h, rho_wv_h : floats
        hourly average values of temperature, relative humidity, pressure,
        vapor pressure, water vapor density

    '''
    # get hourly temperature, relative humidity and pressure by averaging
    t_h = df.t.mean()
    rh_h = df.rh.mean()
    p_h = df.p.mean()

    # Make sure pressure is in Pa, not in hPa
    if p_h < 6e4:
        p_h = p_h*100

    # get hourly vapor pressure and density of water vapor from those
    e_h, rho_wv_h, rho_air_h = get_e_and_rho(t_h, rh_h, p_h)

    return t_h, rh_h, p_h, e_h, rho_wv_h, rho_air_h


def prepare_ds_with_ref_data(arr, timerange_full, timerange_30min,
                             date, loc, ref_data):
    """
    prepare dictionaries with all the variables and coordinates needed to make
    a xarray dataset

    Parameters
    ----------
    arr : np.array
        Array holding the raw uvwt values
    timerange_full : pd.DateTimeIndex
        timestamps corresponding to the high frequency data
    timerange_30min : pd.DateTimeIndex
        timestamps corresponding to the 30-min reference data
    date : pd.Timestamp
        timestamp denoting the beginning of the 1h period
    loc : str
        sonic location
    ref_data : pd.DataFrame
        reference 30-min data

    Returns
    -------
    data_vars : dict
        all variables for the desired xarray Dataset
    coords : dict
        coordinates to be used in the xarray Dataset

    """
    # convert u, v, w from cm/s to m/s
    arr[:, [0, 1, 2]] = arr[:, [0, 1, 2]] / 100.0
    # convert speed of sound to virtual temperature (in Kelvin)
    T = get_sonic_temperature(arr[:, 3])
    # get actual absolute (non-sonic) temperature
    T_abs = get_abs_temperature(arr[:, 3], ref_data)

    # Data variables to make the dataset
    data_vars = dict(u=('time', arr[:, 0]),
                     v=('time', arr[:, 1]),
                     w=('time', arr[:, 2]),
                     T=('time', T),
                     T_abs=('time', T_abs),
                     T_30min=('time_30min', ref_data.t),
                     rh_30min=('time_30min', ref_data.rh),
                     p_30min=('time_30min', ref_data.p * 100))  # convert to Pa
    coords = dict(time=timerange_full,
                  time_30min=timerange_30min,
                  time_1h=date.to_datetime64())

    # if krypton is present, calculate also humidity
    if (loc in ['A', 'C', 'F']):
        # from krypton voltage calculate humidity, add it to the data set
        voltage = arr[:, 4]
        # get necessary variables at hourly resolution
        t_h, rh_h, p_h, e_h, rho_wv_h, rho_air_h = get_hourly_vars(ref_data)
        # calculate specific humidity
        q = get_humidity(loc, voltage, rho_air_h, rho_wv_h)
        # add raw voltage and calculated humidity q to the data_vars dictionary
        data_vars['voltage'] = ('time', voltage)
        data_vars['q'] = ('time', q)
        # add hourly variables which were used to calculate q
        data_vars['e_1h'] = ('time_1h', [e_h])               # vapor pressure [Pa] 
        data_vars['rho_wv_1h'] = ('time_1h', [rho_wv_h])     # Air density [g/m^3]
        data_vars['rho_air_1h'] = ('time_1h', [rho_air_h])   # Water vapor density [g/m^3]

    return data_vars, coords


def prepare_ds_no_ref_data(arr, timerange_full, date):
    """
    prepare dictionaries with all the variables and coordinates needed to make
    a xarray dataset

    Parameters
    ----------
    arr : np.array
        Array holding the raw uvwt values
    timerange_full : pd.DateTimeIndex
        timestamps corresponding to the high frequency data
    date : pd.Timestamp
        timestamp denoting the beginning of the 1h period

    Returns
    -------
    data_vars : dict
        all variables for the desired xarray Dataset
    coords : dict
        coordinates to be used in the xarray Dataset

    """
    # convert u, v, w from cm/s to m/s
    arr[:, [0, 1, 2]] = arr[:, [0, 1, 2]] / 100.0
    # convert speed of sound to temperature (in Kelvin)
    T = get_sonic_temperature(arr[:, 3])

    # Data variables to make the dataset
    data_vars = dict(u=('time', arr[:, 0]),
                     v=('time', arr[:, 1]),
                     w=('time', arr[:, 2]),
                     T=('time', T))
    coords = dict(time=timerange_full,
                  time_1h=date)
    return data_vars, coords


def get_krypton_calibration(loc, ln_V_mean, rho_hourly):
    '''
    # I still don't fully understand why they convert it this way, using Taylor
    expansions around the mean...
    Original documentation:
    Using the mean hourly density ("rom"), this function calculates the
    instantaneous density for every possible signal (0 ... 5000mV) of the
    Krypton hygrometer. From the mean lnV (rlnvm) first calculate the mean V''
    (rlnvmm), corresponding to manual - then d(ro)/d(V'') (labelled drodlnv)
    and from that "ro".

    This function uses high-frequency voltage measurement from a krypton
    hygrometer and mean humidity measurement at the given station to obtain
    high-frequency humidity data. Assuming that average humidity (or rather,
    more specifically, mean water vapor density) is related to the logarithm of
    mean voltage, this function numerically produces a "calibration curve" for
    the given time period. This is done by Taylor-Expanding around the mean
    voltage value. This curve is later used as a look-up table for converting
    voltage into density and then to specific humidity.

    Useful reference: https://s.campbellsci.com/documents/us/manuals/kh20.pdf
    For producting a calibration curvem see Eq. A-4:
        ln(V) = -k_w * x * rho_w + ln(V_0)
        where x is the tube separation, k_w is the absorption coefficient, and
        V_0 is reference voltage.

    Info on using Taylor series (although not similar to what's done here):
    https://www.researchgate.net/profile/Henk-De-Bruin/publication/249603755_Oxygen_Sensitivity_of_Krypton_and_Lyman_Hygrometers/links/55d31b3508ae0b8f3ef92293/Oxygen-Sensitivity-of-Krypton-and-Lyman-Hygrometers.pdf

    From edi2000v2.f:
    The krypton voltage takes values between 0 and 5000. Thus its logarithm is
    at most np.log(5000) = 8.517.
    Wet conditions correspond to ln(V) = 0.00 to 6.599
    Dry conditions correspond to ln(V) = 6.60 to 8.517
    The change between wet/dry occurs at voltage of 735 mV

    Nomenclature:
    ln_V_mean: log of mean voltage over a given time period - reference voltage
              (originally rlnvm)
    rho_hourly: reference water vapor density
    ln_V0_wet/ln_V0_dry: reference calibration values for wet/dry conditions,
                         i.e. ln(V_0). Originally called rlnv0w/rlnv0d.
    kw_wet/kw_dry: reference values for absorption coefficients, originally
                   called rkww/rkwd
    d_rho_d_lnv = d(rho) / d(ln V)
    x: separation between krypton tubes in cm (originally xpk)

    Parameters
    ----------
    loc : str
        location, denoting the position of the krypton

    Returns
    -------
    rho : array
        "look up table/calibration curve": 5000 different values of rho,
        correspondng to 5000 different possible voltage values.

    '''
    # get calibration constants for a given kryptons
    # no clue what these are...
    [B0, B1, B2, kw_dry, kw_wet,
     ln_V0_dry, ln_V0_wet, x] = krypton_calibrations[loc]

    # Make storage arrays for water vapor density (output)
    rho = np.zeros(5000)

    # Choose different coefficients in wet and dry conditions
    if ln_V_mean < 6.600:        # wet conditions
        rlnvmm = (ln_V0_wet-ln_V_mean)/kw_wet
        rlnv = (ln_V0_wet-np.log(np.arange(1, 5001)))/kw_wet
    else:                       # dry conditions
        rlnvmm = (ln_V0_dry-ln_V_mean)/kw_dry
        rlnv = (ln_V0_dry-np.log(np.arange(1, 5001)))/kw_dry

    # get d(ln V): voltage differentials w.r.t. the reference voltage
    d_lnv = rlnv - rlnvmm

    # Taylor-Expansion around rho = rho_mean up to first order
    # first see if a solution is possible
    # rho(i) = B0 + B1*(V-V_O) + B2*(V-V_O)^2
    # get determinant: constant
    det = B1**2 - 4*B0*B2

    if (det + 4*B2*rho_hourly*x) > 0:
        # get another determinant: constant
        d2det = np.sqrt(det + 4*B2*rho_hourly*x)
        # get a third determinant: array!
        d3det = d2det - 2*B2*d_lnv

        # where d3det is positive, calculate drodlnv
        with np.errstate(invalid='ignore'):
            d_rho_d_lnv = np.where(d3det > 0, d2det**2 / d3det, 0)
    else:
        # if above condition is not fulfilled, make an array of zeors
        d_rho_d_lnv = np.zeros(5000)

    d_rho = d_rho_d_lnv * d_lnv / x

    # calclate density as a function of voltage for all 5000 values
    rho = d_rho + rho_hourly
    # if there are negative values, round to zero
    with np.errstate(invalid='ignore'):
        rho = np.where(rho < 0, 0, rho)

    return rho


# %% CALCULATIONS

def get_local_pressure(loc, temp, p0):
    ''' Calculate pressure at sonic location: pressure measured only at
    Bosco di Sotto tower (location "A1", or sonic "A")

    Adapted from the edi2000v2.f script, but written more clearly/explicitely

    Parameters
    ----------
    loc : str
        location (letter) denoting which sonic we're doing the calculation for
    temp : float/array/pd series
        reference temperature measured at the Bosco di Sotto station
    p0 : float/array/pd series
        reference pressure measured at the Bosco di Sotto station

    Returns
    -------
    p : float
        pressure at the given location, based on the height difference and
        the current pressure and temperature at Bosco di Sotto

    '''
    # get altitude difference between reference pressure measurement and sonic
    # sonic height plus difference in tower heights
    dz = sonic_height[loc] + height_asl[loc] - height_asl['A']

    # make sure temperature is in Kelvin
    if (temp < 200).any():
        temp = temp + 273.15

    # define constants
    M = 0.02897  # [kg/mol]     molar mass of air
    g = 9.81     # [m/s^2]      gravity
    R = 8.3143   # [N*m/mol/K]  universal gas constant

    # calculate pressure at location based on the barometric formula
    exp = -M*g*dz / (R*temp)
    p = p0 * np.exp(exp)
    return p


def get_e_and_rho(t_hourly, rh_hourly, p_hourly):
    '''
    Tis function calculates and returns one hour averages of vapor pressure
    and densities of air and water vapor.

    First calculate saturation vapor pressure from mean hourly temperature and
    pressure. Method taken from the original analysis script edi2000v2.f which
    used the Goff-Gratch (1945) equation:
    https://en.wikipedia.org/wiki/Goff%E2%80%93Gratch_equation

    From saturation vapor pressure, use rh to calculate vapor pressure
    Use vapor pressure to get mixing ratio
    Get air density using ideal gas law
    Get water vapor density using the mixing ratio and air density

    For calculations and conversions, it's useful to refer to:
    R. Stull, Meteorology for Scientistsand Engineers, 3rd Edition, Chapter 4

    Parameters
    ----------
    t_hourly : float
        mean hourly temperature
    rh_hourly : float
        mean hourly relative humidity
    p_hourly : float
        mean hourly pressure

    Returns
    -------
    e_hourly : float
        Mean hourly vapor pressure. Typical values of saturation vapor pressure
        are ~1200 Pa at 10C, ~2300 Pa at 20C. Vapor pressure should thus not
        exceed those values.
    rho_water_vapor_hourly : flaot
        Mean hourly density of water vapor. Values up to 9 g/m^3 at 10C,
        17 g/m^3 at 20C.
    rho_air_hourly : float
        Mean hourly density of air. Around 1250 g/m^3 at 10C.

    '''
    # Make sure temperature is in Kelvin
    if t_hourly < 200:
        t_hourly = t_hourly + 273.16

    # Compute the saturation vapour pressure e_s[hPa] from t_hourly
    T_frac = 373.16/t_hourly
    a = -7.90298*(T_frac-1)
    b = 5.02808*(np.log10(T_frac))
    c1 = (11.344*(1-(1/T_frac)))
    c2 = -7
    c = -1.3816*(10.**c2)*((10.**c1)-1)
    d1 = (-3.49149*(T_frac-1))
    d2 = -3
    d = 8.1328*(10.**d2)*((10.**d1)-1.)
    e = np.log10(1013.246)
    es_exp = a+b+c+d+e
    # saturation vapor pressure, multiplication by 100 to convert to [Pa]
    e_s = 100 * 10.0**es_exp

    # get vapor pressure [Pa] from e_s [Pa] and rh [%], originally called "em"
    e_hourly = e_s * rh_hourly/100

    # mixing ratio [kg/kg], originally called "rr"
    mixing_ratio = 0.62197*e_hourly/(p_hourly-e_hourly)

    # Density of dry air [g/m^3], in original script called "dd"
    rho_air_hourly = p_hourly/(287.058*t_hourly) * 1e3

    # Mean density of water vapour, in original script called "rom"
    # units: [g/m^3] * [kg/kg] = [g/m^3]
    rho_water_vapor_hourly = rho_air_hourly * mixing_ratio

    return e_hourly, rho_water_vapor_hourly, rho_air_hourly


def get_abs_temperature(sos, ref_data):
    """
    Calculates temperature from speed of sound in moist air
    Formula from original script edi2000v2.f - they adapted it most likely
    from https://doi.org/10.1175/1520-0469(1949)006<0273:PROTMB>2.0.CO;2
    (Barrett and Suomi, 1949)

    Note: there's a pretty large bias (>5K) between the t_hourly taken from the
    reference data and between the mean of this output.

    Parameters
    ----------
    sos : array
        speed of sound in moist air
    ref_data : pd.Dataframe
        pandas dataframe containing the half-houlry reference data

    Returns
    -------
    T : array
        temperature

    """
    # get average hourly variables
    t_h, rh_h, p_h, e_h, rho_wv_h, rho_air_h = get_hourly_vars(ref_data)

    # get temperature from speed of sound
    # first division by 50 because the instrument saves data as 1/50 m/s

    frac = 1 + 0.3192*(e_h/p_h)
    T = (sos * 0.02)**2 / 403 / frac
    return T


def get_sonic_temperature(sos):
    """
    Simplified formula for getting virtual temperature from speed of sound -
    it does not take into account pressure and humidity changes.

    The 0.02 comes from the way how the sonic stores the speed of sound data,
    i.e. this calculation is based on the eqn. T = c^2 / 403

    Parameters
    ----------
    sos : array
        speed of sound

    Returns
    -------
    T : array
        temperature

    """
    T = (sos * 0.02)**2 / 403
    return T


def get_humidity(loc, voltage, rho_air_hourly, rho_wv_hourly):
    '''
    Using the voltage measurement from the krypton, calculates specific
    humidity.

    Parameters
    ----------
    loc : str
        Location. Needed to get the calibration values for the specific krypton
    voltage : array
        Measurements of voltage, between 0-5000.
    rho_air_hourly : float
        Mean density of air in the given hour
    rho_wv_hourly : float
        Mean density of water vapor in the given hour

    Returns
    -------
    q : array
        High frequency values of specific humidity, same shape as voltage input
        Should not exceed ~0.04 kg/kg

    '''
    # I think that the original script did it over 30 min periods?
    # calculate logarithm of 1h mean voltage from the krypton
    with np.errstate(invalid='ignore'):
        ln_v_mean = np.log(np.nanmean(voltage))

    # get reference rho based on ln_v: 5000 values for each possible voltage
    rho_ref = get_krypton_calibration(loc, ln_v_mean, rho_wv_hourly)

    # temporarily replace nans: can't be used for indexing
    voltage_no_nans = np.nan_to_num(voltage, nan=1)
    # convert negative/zero values to 1: ln(1) = 0
    voltage_no_nans = np.where(voltage_no_nans < 1, 1, voltage_no_nans)
    # convert faulty voltage over 5000 to 1 as well
    voltage_no_nans = np.where(voltage_no_nans > 5000, 1, voltage_no_nans)
    # get density at high frequency by indexing the references
    # -1 due to python indexing: voltage=1 corresponds to zeroth rho value
    rho_high_freq = rho_ref[voltage_no_nans.astype('int32')-1]
    # reintroduce nan values to where they were originally
    rho_high_freq = np.where(np.isnan(voltage), np.nan, rho_high_freq)

    # get mixing ratio r from density fluctuations and average air density
    r = rho_high_freq/rho_air_hourly
    # get specific humidity [kg/kg]
    q = r/(1+r)

    return q
