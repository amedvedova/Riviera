#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Metadata to add to .nc files for all sonics and kryptons

# dictionaries with info about the sonics:
# location according to the MAP-Riviera project naming convention
sonic_location = {'E1_1': 'Maruso-Roasco, level 1',
                  'E1_2': 'Maruso-Roasco, level 2',
                  'E2_1': 'Monte Nuovo, level 1',
                  'E2_2': 'Monte Nuovo, level 2',
                  'E2_3': 'Monte Nuovo, level 3',
                  'E2_4': 'Monte Nuovo, level 4',
                  'E2_5': 'Monte Nuovo, level 5',
                  'E2_6': 'Monte Nuovo, level 6',
                  'F1_1': 'Alpe Domas, level 1',
                  'F2_1': 'Alpe di Gagern, level 1',
                  'F2_2': 'Alpe di Gagern, level 2',
                  'G_1':  'Censo, level 1'}

# serial numbers of sonics, info from metadata report
sonic_SN = {'E1_1': 'Metek USA-1 9903006',
            'E1_2': 'Gill R2A 0043',
            'E2_1': 'CSAT 0118-2',
            'E2_2': 'CSAT 0199-2',
            'E2_3': 'Gill R2 0211',
            'E2_4': 'Gill R2 0213',
            'E2_5': 'Gill R2 0212',
            'E2_6': 'Gill HS 000046',
            'F1_1': 'Gill Enhanced 009',
            'F2_1': 'Gill R2 0208',
            'F2_2': 'Gill R2 0160',
            'G_1':  'Gill Enhanced 007'}

# tower altitude
height_asl = {'E1_1': 1060,
              'E1_2': 1060,
              'E2_1': 1030,
              'E2_2': 1030,
              'E2_3': 1030,
              'E2_4': 1030,
              'E2_5': 1030,
              'E2_6': 1030,
              'F1_1': 1750,
              'F2_1': 2110,
              'F2_2': 2110,
              'G_1':  870}

# geographical location, info from metadata report
# TODO: F2 location had variable sonic height probably?!?!
sonic_height = {'E1_1': 2.0,
                'E1_2': 12.7,
                'E2_1': 1.84,
                'E2_2': 6.34,
                'E2_3': 9.32,
                'E2_4': 12.82,
                'E2_5': 16.82,
                'E2_6': 22.68,
                'F1_1': 6.3,
                'F2_1': 3.1,
                'F2_2': 11.0,
                'G_1':  5.25}

# geographical location of towers
sonic_latlon = {'E1_1': [14.266667, 9.037222],
                'E1_2': [14.266667, 9.037222],
                'E2_1': [14.270556, 9.036389],
                'E2_2': [14.270556, 9.036389],
                'E2_3': [14.270556, 9.036389],
                'E2_4': [14.270556, 9.036389],
                'E2_5': [14.270556, 9.036389],
                'E2_6': [14.270556, 9.036389],
                'F1_1': [14.270000, 9.055278],
                'F2_1': [14.272778, 9.060833],
                'F2_2': [14.272778, 9.060833],
                'G_1':  [14.274167, 9.031667]}

# serial numbers of kryptons
krypton_SN = {'E1_2': 1199,
              'E2_6': 1094}

# height of the krypton the mast
krypton_height = {'E1_2': 12.7,
                  'E2_6': 22.68}
