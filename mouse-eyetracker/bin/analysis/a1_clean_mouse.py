# Cleaning routine to remove invalid paths from the data.
#!usr/bin/env python3

__author__  = "Chris Bao"
__version__ = "0.9"
__date__    = "24 Jun 2022"

from cmath import pi
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

DIR_ROOT  = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DIR_DATA  = os.path.join(DIR_ROOT, "data", "mouse", "raw")
DIR_CLEAN = os.path.join(DIR_ROOT, "data", "mouse", "clean")

if not os.path.exists(DIR_CLEAN):
    os.makedirs(DIR_CLEAN)

FILES_MOUSE = os.listdir(DIR_DATA)

# Bounds of rejection range for direction vectors
LOWER_BOUND: float = 7/6 * pi
UPPER_BOUND: float = 11/6 * pi

FRAME_WINDOW = 6
DROP_THRESHOLD = 22

def rejection_bounds(angle: float) -> bool:
    """
    Determine if an angle is within the set bounds.
    """
    return (LOWER_BOUND < angle) & (angle < UPPER_BOUND)

def clean(path) -> None:
    global total_removed
    """
    Clean a specific file. Removes raw_trials that have downward motions.
    To find this, we use a rolling average direction vector across a number
    of frames. If the direction is too steeply downwards, we'll reject the
    trial.
    """
    total_removed = 0
    raw_trials = pd.read_csv(path)
    # makes an empty DataFrame with the same headers as original
    cleaned_trials = raw_trials[raw_trials["trial"] == -1]
    # for raw_trials, calculate rolling average vector direction
    # if direction falls within bounds, reject the trial
    for i in range(raw_trials["trial"].iat[-1] + 1):
        evaluated_trial = raw_trials[raw_trials["trial"] == i].copy()
        
        evaluated_trial["x_delta"]   = evaluated_trial.x.diff().shift(0)
        evaluated_trial["x_rolling"] = evaluated_trial.x_delta.rolling(FRAME_WINDOW, FRAME_WINDOW).sum()
        evaluated_trial["y_delta"]   = evaluated_trial.y.diff().shift(0)
        evaluated_trial["y_rolling"] = evaluated_trial.y_delta.rolling(FRAME_WINDOW, FRAME_WINDOW).sum()
        evaluated_trial["v_angle"]   = np.mod(np.arctan2(evaluated_trial.y_rolling / FRAME_WINDOW, evaluated_trial.x_rolling / FRAME_WINDOW), 2 * pi)
        
        # if no invalid mouse movement, merge back
        if len(evaluated_trial[rejection_bounds(evaluated_trial["v_angle"])]) == 0:
            # `join="inner"` so we don't include extra calculated columns
            cleaned_trials = pd.concat((cleaned_trials, evaluated_trial), join="inner")
        elif len(evaluated_trial[rejection_bounds(evaluated_trial["v_angle"])]) != 0:
            # print(len(evaluated_trial[rejection_bounds(evaluated_trial["v_angle"])]))
            total_removed += 1

    file_name = iFile

    if total_removed <= DROP_THRESHOLD:
        cleaned_trials.to_csv(os.path.join(DIR_CLEAN, file_name), index=False)

for iFile in FILES_MOUSE:
    
    clean(os.path.join(DIR_DATA, iFile))

    if int(total_removed) <= DROP_THRESHOLD:
        print('%(number)02d trials removed from %(sub)s' % {'number':total_removed, 'sub':iFile})
    elif int(total_removed) > DROP_THRESHOLD:
        print('Block deleted: %(number)02d invalid trials from %(sub)s' % {'number':total_removed, 'sub':iFile})

