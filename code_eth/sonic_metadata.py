#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Metadata to add to .nc files for all sonics and kryptons


# dictionaries with info about the sonics:
# location according to the MAP-Riviera project naming convention
sonic_location = {'A': 'A1 (Bosco di Sotta), lowest level',
                  'B': 'A1 (Bosco di Sotta), mid level',
                  'C': 'A1 (Bosco di Sotta), highest level',
                  'D': 'C (Pian Perdascio), only one level',
                  'E': 'B (Rored), lowest level',
                  'F': 'B (Rored), middle level',
                  'G': 'B (Rored), highest level',
                  'H': 'D (Torrazza), only one level'}

sonic_output_name = {'A': 'A1_1',
                     'B': 'A1_2',
                     'C': 'A1_3',
                     'D': 'C_1',
                     'E': 'B_1',
                     'F': 'B_2',
                     'G': 'B_3',
                     'H': 'D_1'}

# serial numbers of sonics, info from metadata report
sonic_SN = {'A': 'Gill R2A 0068',
            'B': 'Gill R2A 0069',
            'C': 'Gill R2A 0047',
            'D': 'Gill R2 0054',
            'E': 'Gill R2 0035',
            'F': 'Gill R2 0030',
            'G': 'Gill R2 0036',
            'H': 'Gill R3A 0103'}

# tower altitude: in the original fortran scripts labeled as "lheight"
height_asl = {'A': 250,
              'B': 250,
              'C': 250,
              'D': 340,
              'E': 760,
              'F': 760,
              'G': 760,
              'H': 256}

# height of the sonics on the masts, info from metadata report
# one README file contains different values, edi2000v2.f script has a different
# values for D (6.5 m)
sonic_height = {'A': 3.56,
                'B': 15.58,
                'C': 27.63,
                'D': 6.33,
                'E': 15.34,
                'F': 23.78,
                'G': 29.75,
                'H': 5.77}

# geographical location, info from metadata report
sonic_latlon = {'A': [46.254722, 9.011667],
                'B': [46.254722, 9.011667],
                'C': [46.254722, 9.011667],
                'D': [46.238056, 9.004722],
                'E': [46.263056, 9.030833],
                'F': [46.263056, 9.030833],
                'G': [46.263056, 9.030833],
                'H': [46.244167, 9.025833]}

# serial numbers of kryptons: taken from edi2000v2.f script and checked with a
# readme file "MAP-Riviera project sonic data" since the serial number of C is
# wrong (same as A) in the metadata report (H taken from the report)
krypton_SN = {'A': 'KH2O (1299)',
              'C': 'KH2O (1300)',
              'F': 'KH2O (1370)'}

# height of the krypton the mast, info from metadata report
# only for H it differs from the sonic height
krypton_height = {'A': 3.56,
                  'C': 27.63,
                  'F': 23.78,
                  'H': 5.78}

# values needed to convert voltage to humidity for each krypton
# taken from edi2000v2.f
# order: [b0, b1, b2, rkwd, rkww, rlnv0d, rlnv0w, xpk]
# Clean window: these were used!
# No "clean window" coeffs for F - these are copied from the scaled ones!
krypton_calibrations = {'A': [0.1212, 0.9790, 0.0007, 0.152,
                              0.137, 8.479, 8.301, 1.393],
                        'C': [0.0875, 0.9841, 0.0005, 0.149,
                              0.135, 8.456, 8.285, 1.438],
                        'F': [0.6768, 0.8617, 0.0051, 0.152,
                              0.137, 8.222, 8.054, 1.361]}
# # Scaled window calibration coefficients
# krypton_calibrations = {'A': [0.7582, 0.8636, 0.0044, 0.152,
#                               0.136, 8.111, 7.919, 1.393],
#                         'C': [0.6748, 0.8851, 0.0039, 0.149,
#                               0.135, 8.043, 7.866, 1.438],
#                         'F': [0.6768, 0.8617, 0.0051, 0.152,
#                               0.137, 8.222, 8.054, 1.361]}
