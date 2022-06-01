 #!/usr/bin/env python3
"""Source code for the Python part of the Motor Control experiment."""
from __future__ import absolute_import, division
from psychopy import locale_setup, sound, gui, visual, core, data, event, logging, clock, monitors
from psychopy.visual import textbox
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)
import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle

import random
import pandas as pd
import datetime

import os  # handy system and path functions
import sys  # to get file system encoding

from smite import SMITE, helpers, RED250mobile
from random import randrange

__author__      = "Chris Bao, Rajasi Desai, Ramiro Rea"
__copyright__   = "Copyright 2021 Chris Bao"
__version__     = "1.0.1"
__status__      = "Live"

### CONSTANTS ###
# Screen resolution
RES_WIDTH  = 1920 # Horizontal in pixels
RES_HEIGHT = 1080 # Vertical in pixels
MON_WIDTH  = 34.5 # Width screen in cm for HP Laptop
# MON_WIDTH = 52.7 # Width screen in cm Home

# Lab testing
# RES_WIDTH  = 1280 # Horizontal in pixels
# RES_HEIGHT = 1024 # Vertical in pixels
# MON_WIDTH  = 37.6 # Width screen in cm

MON_DIST   = 65   # Distance to screen in cm
 
LINE_WIDTH  = 2
FRAME_WIDTH = 40
FRAMERATE   = 1/60

# Stimuli and response settings
INFOBOX_SIZE  = 300 # prob and reward boxes
RESPONSE_SIZE = 500 # target options
TEXT_HEIGHT   = 100 # height of text in start, rest, end screens

# Fixation target
TARGET_SIZE = 0.8
TARGET_POS = (0,-7)

# Colors
HEX_WHITE  = "#ffffff"
HEX_BLACK  = "#000000"
HEX_YELLOW = "#E38411"
HEX_BLUE   = "#1170E3"
HEX_RED    = "#881c1c"

# States
START_STATE = 0 # block start
TRIAL_STATE = 1 # trial
PAUSE_STATE = 2 # wait for button press
REST_STATE  = 3 # break
END_STATE   = 4 # block end
ISI_STATE   = 5 # show fixation target
ERROR_STATE = 6 # message if mouse stops
CALIB_STATE = 7

INTER_STIMULUS_INTERVAL = 0.7

RESET_POINT = (0, -RES_HEIGHT/4) # where the mouse gets reset at the start of each trial

TEXT_WRAP_WIDTH = 1500

# Grid and mask positions
# Organized top left, top right, bot left, bot right as a 2x2 array
GRID_SIZE = 4.25

STIM_POS = (((-1.375, 7), (1.375, 7)),
                 ((-1.375, 4.25), (1.375, 4.25)))

STIM_FONT = "Courier New" #Monospaced font for the stimuli
STIM_SIZE = 0.7 #Around 268 pixels in the laptop screen

maskPosBot = [[[0,0], [0,0]],
              [[0,0], [0,0]]]

maskPosTop = [[[0,0], [0,0]],
              [[0,0], [0,0]]]
                   
maskPosLeft = [[[0,0], [0,0]],
               [[0,0], [0,0]]]
                   
maskPosRight = [[[0,0], [0,0]],
                [[0,0], [0,0]]]

## Offset mask around the stimulus                  
for row in range(2):
    for col in range(2):
        maskPosBot[row][col] = [STIM_POS[row][col][0], STIM_POS[row][col][1]-0.5]
        maskPosTop[row][col] = [STIM_POS[row][col][0], STIM_POS[row][col][1]+0.5]
        maskPosLeft[row][col] =[STIM_POS[row][col][0]-0.6, STIM_POS[row][col][1]]
        maskPosRight[row][col] = [STIM_POS[row][col][0]+0.6, STIM_POS[row][col][1]]

# Response position
RESPONSE_OFFSETS = ((-RES_WIDTH/2+RESPONSE_SIZE/4,+RES_HEIGHT/2-RESPONSE_SIZE/4),
                    (+RES_WIDTH/2-RESPONSE_SIZE/4,+RES_HEIGHT/2-RESPONSE_SIZE/4))
                   
## Monitor Settings
mon = monitors.Monitor(name = 'laptop_mon') # Defined in defaults file
mon.setWidth(MON_WIDTH) # Width of screen (cm)
mon.setDistance(MON_DIST) # Distance eye / monitor (cm) 
mon.setSizePix((RES_WIDTH,RES_HEIGHT))
mon.saveMon()

# SETUP 
## Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

## Store info about the experiment session
expInfo = {"expName":"Gambling Motor Control EyeTracker",
           "dateTime":str(datetime.datetime.now()).replace(" ", "-").replace(":", "-"),
           "psychopyVersion":"3.2.4"}

setupDialog = gui.Dlg(title=expInfo["expName"]) 
setupDialog.addField("Subject ID")
setupDialog.addField("Condition", choices=("A", "B", "C", "D"))
setupDialog.addField("Practice?", choices=("Yes", "No"))                     
response = setupDialog.show()

if(setupDialog.OK):
    expInfo["subjectID"] = response[0]
    expInfo["probTop"] = response[1] in ("A", "B") #prob top
    condition  = response[1] 
    testRun    = response[2] == "Yes"
else:
    core.quit() # user pressed cancel
 
# Define task structure
# number of blocks per subject; should be 5

if testRun == True and expInfo["subjectID"] != "t1" and expInfo["subjectID"] != "t2" :
    expInfo["subjectID"] = "practice"
    useTracker = False
    N_BLOCKS = 1
    TRIALS_PER_BLOCK = [10] * N_BLOCKS
elif expInfo["subjectID"] == "t1" and testRun == True:
    useTracker = False
    N_BLOCKS = 3
    TRIALS_PER_BLOCK = [4] * N_BLOCKS
elif expInfo["subjectID"] == "t2" and testRun == True:
    useTracker = True
    N_BLOCKS = 3
    TRIALS_PER_BLOCK = [4] * N_BLOCKS
elif testRun == False:
    useTracker = True
    N_BLOCKS = 5
    TRIALS_PER_BLOCK = [64] * N_BLOCKS

## Eye tracker settings
if useTracker == True:
    eye_tracker_name = 'RED250mobile'
    settings = SMITE.get_defaults(eye_tracker_name) 
    tracker = SMITE.Connect(settings)
    tracker.init()
    print(tracker.system_info)

## Output file names
### Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
behaviorFileName = _thisDir + os.sep + "data" + os.sep + "behavior" +\
        os.sep + "%s-%s-%s-%d" % ("behavior", expInfo["subjectID"],
        expInfo["dateTime"], expInfo["probTop"])
mouseFileName = _thisDir + os.sep + "data" + os.sep + "mouse" +\
        os.sep + "%s-%s-%s-%d" % ("mouse", expInfo["subjectID"],
        expInfo["dateTime"], expInfo["probTop"])
trackerFileName = _thisDir + os.sep + "data" + os.sep + "tracker" +\
        os.sep + "%s-%s-%s-%d" % ("tracker", expInfo["subjectID"],
        expInfo["dateTime"], expInfo["probTop"])

## Window settings
window = visual.Window(monitor  = mon, 
                       screen   = 0, 
                       size     = (RES_WIDTH, RES_HEIGHT),
                       units    = 'pix', 
                       fullscr  = True,
                       allowGUI = False) 

## System Variables
state = START_STATE
timer = core.CountdownTimer(INTER_STIMULUS_INTERVAL)
clock = core.monotonicClock # instance of core.MonotonicClock that starts with import of core
framecount = 0
block = 0
trial = 0
order = 0
trialStartTime = clock.getTime() # holds the time of each trial start as it runs
raw_data = pd.read_csv("ratio_data.csv")
random_sequence = randrange(1,11)
data = raw_data[raw_data.seq_id.eq(random_sequence)]

mouse = event.Mouse(win=window)
mouse_tracking_on = False # whether the mouse should be tracking
# for each trial, store the mouse info and time for each frame;
        # write during inter-stimulus interval
mousex = []
mousey = []
mouset = [] # time
mousef = [] # frame number
mouseLastPos = [-99999.0, -99999.0]
mouseTest = None
mouseStoppedCount = 0

if condition in ("A", "C"): # conditions A and C show blue on top
    topColor = HEX_BLUE
    botColor = HEX_YELLOW
else:
    topColor = HEX_YELLOW
    botColor = HEX_BLUE
              
stimColors = (((topColor), (topColor)),
             ((botColor), (botColor)))


##-----------------------------------------------------------

# Stimulus grid
## Stimulus settings
infoGrid = [[],[]]
for row in range(2):
    for col in range(2):
        infoGrid[row].append(visual.Rect(win       = window, 
                                         units     = "deg",
                                         lineWidth = LINE_WIDTH, 
                                         lineColor = HEX_BLACK, 
                                         fillColor = HEX_WHITE,
                                         size      = GRID_SIZE, 
                                         opacity   = 1, 
                                         pos       = STIM_POS[row][col]
                                         ))

infoMaskTop = [[],[]]
infoMaskBot = [[],[]]
infoMaskLft = [[],[]]
infoMaskRgt = [[],[]]
for row in range(2):
    for col in range(2):
        infoMaskTop[row].append(visual.TextStim(win    = window, 
                                                color  = stimColors[row][col],
                                                units  = "deg", 
                                                height = STIM_SIZE,
                                                font   = STIM_FONT,
                                                bold   = True,
                                                text   = '####', 
                                                pos    = maskPosTop[row][col]
                                                ))
                                              
        infoMaskBot[row].append(visual.TextStim(win    = window, 
                                                color  = stimColors[row][col],
                                                units  = "deg", 
                                                height = STIM_SIZE,
                                                font   = STIM_FONT,
                                                bold   = True,
                                                text   = '####', 
                                                pos    = maskPosBot[row][col]
                                                ))
                                                 
        infoMaskLft[row].append(visual.TextStim(win    = window, 
                                                color  = stimColors[row][col],
                                                units  = "deg", 
                                                height = STIM_SIZE,
                                                font   = STIM_FONT,
                                                bold   = True,
                                                text   = '#', 
                                                pos    = maskPosLeft[row][col]
                                                ))
                                                 
        infoMaskRgt[row].append(visual.TextStim(win    = window, 
                                                color  = stimColors[row][col],
                                                units  = "deg", 
                                                height = STIM_SIZE,
                                                font   = STIM_FONT,
                                                bold   = True,
                                                text   = '#', 
                                                pos    = maskPosRight[row][col]
                                                ))

infoText = [[],[]]
for row in range(2):
    for col in range(2):
        infoText[row].append(visual.TextStim(win    = window, 
                                             color  = stimColors[row][col],
                                             units  = "deg", 
                                             height = STIM_SIZE,
                                             font   = STIM_FONT,
                                             bold   = True,
                                             text   = '', 
                                             pos    = STIM_POS[row][col]
                                             ))

### RESPONSE ###
leftBox = visual.rect.Rect(win       = window, 
                           units     = "pix", 
                           lineWidth = LINE_WIDTH,
                           lineColor = HEX_BLACK, 
                           fillColor = HEX_WHITE, 
                           size      = RESPONSE_SIZE,
                           opacity   = 1, 
                           pos       = RESPONSE_OFFSETS[0]
                           )

rightBox = visual.rect.Rect(win=window, units="pix", lineWidth=LINE_WIDTH,
        lineColor=HEX_BLACK, fillColor=HEX_WHITE, size=RESPONSE_SIZE,
        opacity=1, pos=RESPONSE_OFFSETS[1])

leftText = visual.TextStim(win=window, font="Arial", color=HEX_BLACK,
        units="pix", height=RESPONSE_SIZE/2, text="L", pos=RESPONSE_OFFSETS[0])
rightText = visual.TextStim(win=window, font="Arial", color=HEX_BLACK,
        units="pix", height=RESPONSE_SIZE/2, text="R", pos=RESPONSE_OFFSETS[1])

startText = visual.TextStim(window,
            "Ready to go?\n\nPress [esc] to stop during any trial\n\nPress [space] to start",
            color=HEX_BLACK, height=TEXT_HEIGHT, units="pix", wrapWidth=TEXT_WRAP_WIDTH, alignHoriz='center')

errorText = visual.TextStim(window,
            "WRONG MOVE!\n\nRemember:\nDon't stop moving the mouse during the trial\n\n",
            color=HEX_BLACK, height=TEXT_HEIGHT*0.8, units="pix", wrapWidth=TEXT_WRAP_WIDTH, alignHoriz='center')

restText = visual.TextStim(window, 
           "Now you can take a break\n\nPress [space] to continue\n\nPress [r] to recalibrate", 
           color=HEX_BLACK, height=TEXT_HEIGHT, units="pix", wrapWidth=TEXT_WRAP_WIDTH, alignHoriz='center')

endText = visual.TextStim(window, "That was the last block. Thanks for your help!\n\nPress [esc] to finish", 
          color=HEX_BLACK, height=TEXT_HEIGHT, units="pix", wrapWidth=TEXT_WRAP_WIDTH, alignHoriz='center')
        
### FIXATION CROSS ###
targetVert = visual.Line(win       = window, 
                         lineColor = HEX_WHITE,
                         units     = "deg", 
                         start     = (0, -TARGET_SIZE/2),
                         end       = (0, TARGET_SIZE/2),
                         lineWidth = 10,
                         pos       = TARGET_POS)
                    
targetHorz = visual.Line(win       = window, 
                         lineColor = HEX_WHITE,
                         units     = "deg", 
                         start     = (-TARGET_SIZE/2, 0),
                         end       = (TARGET_SIZE/2, 0),
                         lineWidth = 10,
                         pos       = TARGET_POS)
                        
targetBack = visual.Circle(win    = window, 
                           color  = HEX_BLACK,
                           units  = "deg", 
                           radius = TARGET_SIZE/2, 
                           pos    = TARGET_POS)
                          
targetCenter = visual.Circle(win    = window, 
                             color  = HEX_BLACK,
                             units  = "deg", 
                             radius = TARGET_SIZE/15, 
                             pos    = TARGET_POS)

### Start button ###
buttonBody = visual.Rect(win       = window, 
                         fillColor = HEX_RED, 
                         lineWidth = LINE_WIDTH,
                         lineColor = HEX_WHITE, 
                         units     = "deg",
                         width     = STIM_SIZE * 3,
                         height    = STIM_SIZE * 2,
                         opacity   = 1,
                         pos       = TARGET_POS)

buttonText = visual.TextStim(win    = window, 
                            color  = HEX_WHITE,
                            units  = "deg", 
                            height = STIM_SIZE/2,
                            bold   = True,
                            text   = "Click to \nContinue", 
                            pos    = TARGET_POS)

### FUNCTION DEFS ###
def startBlock():
    global behaviorLog, mouseLog, behaviorFileName, mouseFileName, trackerFileName
    behaviorLog = open(behaviorFileName + "-" + str(block) + "-" + str(condition) + str(".csv"), "w")
    behaviorLog.write("start_time,end_time,trial,order,rand_id,trial_id,r_left,r_right,p_left,p_right,response\n")
    mouseLog = open(mouseFileName + "-" + str(block) + "-" +str(condition) + str(".csv"), "w")
    mouseLog.write("time,frame,trial,rand_id,trial_id,order,x,y\n")
    # if useTracker == True:
    #     tracker.start_recording() 
        
def endBlock():
    global behaviorLog, mouseLog
    behaviorLog.close()
    mouseLog.close()
    if useTracker == True:
        #tracker.stop_recording() 
        tracker.save_data(trackerFileName + "-" + str(block) + "-" +str(condition) + str(".idf"))

def startTrial():
    global trialStartTime
    mouse.setPos(newPos=RESET_POINT) # reset cursor to the middle of the screen
    mouse.getPos()
    leftBox.autoDraw = rightBox.autoDraw = True
    leftText.autoDraw = rightText.autoDraw = True
    trialStartTime = clock.getTime()

    # Add flag for trial start and initiate the recording
    if useTracker == True:
        trialInfo = getTrialValues()
        tracker.start_recording() 
        tracker.set_begaze_key_press("start_trial" + str(order) + 
                                     "_pl" + str(trialInfo[2]) + 
                                     "_rl" + str(trialInfo[0]) + 
                                     "_pr" + str(trialInfo[3]) + 
                                     "_rr" + str(trialInfo[1]))
        
    mouseLastPos = mouseLastPos = [-99999.0, -99999.0]
    mouseTest = None
    mouseStoppedCount = 0

def getTrialValues():
    rLeft    = data.iat[order,1]
    pLeft    = data.iat[order,2]
    rRight   = data.iat[order,3]
    pRight   = data.iat[order,4]
    rand_id  = data.iat[order,10]
    trial_id = data.iat[order,0]
    
    return (rLeft, rRight, pLeft, pRight, rand_id, trial_id)
        
def turnOnBoxes():
    # Rewards and probs are together on the same row
    # Position of prob and rewards is determined by condition
    # Draws top left -> top right -> bottom left -> bottom right
    # [0,0] -> [1,0] -> [0,1] -> [1,1]
    trialData = getTrialValues()
    p_order = expInfo["probTop"]
    infoText[1-p_order][0].text = trialData[2] # Prob left
    infoText[p_order][0].text   = trialData[0] # Reward left
    infoText[1-p_order][1].text = trialData[3] # Prob right
    infoText[p_order][1].text   = trialData[1] # Reward right

    for i in range(2):
        for j in range(2):
            infoGrid[i][j].autoDraw = infoText[i][j].autoDraw = True
            infoMaskTop[i][j].autoDraw = True
            infoMaskBot[i][j].autoDraw = True
            infoMaskLft[i][j].autoDraw = True
            infoMaskRgt[i][j].autoDraw = True
    
def turnOffBoxes():
    for i in range(2):
        for j in range(2):
            infoGrid[i][j].autoDraw = infoText[i][j].autoDraw = False
            infoMaskTop[i][j].autoDraw = False
            infoMaskBot[i][j].autoDraw = False
            infoMaskLft[i][j].autoDraw = False
            infoMaskRgt[i][j].autoDraw = False
    leftBox.autoDraw = rightBox.autoDraw = False
    leftText.autoDraw = rightText.autoDraw = False

def turnOnButton():
    buttonBody.autoDraw = buttonText.autoDraw = True

def turnOffButton():
    buttonBody.autoDraw = buttonText.autoDraw = False

def turnOnFixTarget():
    targetBack.autoDraw = targetVert.autoDraw = targetHorz.autoDraw = targetCenter.autoDraw = True

def turnOffFixTarget():
    targetBack.autoDraw = targetVert.autoDraw = targetHorz.autoDraw = targetCenter.autoDraw = False

def randomISI():
    return random.random()*0.3 + 0.3 # returns a random ISI between 300 and 600 ms

def stopTrial(left):
     # `left` is whether response is left or right
    # if `left` is `None`, that means the trial was skipped due to inactivity.
    global state, trial, block, order, mousex, mousey, mouset, mousef
    
    wrong_move = 0

    trialInfo = getTrialValues()
    #return (rLeft, rRight, pLeft, pRight, rand_id, trial_id)
    r_left   = trialInfo[0]
    r_right  = trialInfo[1]
    p_left   = trialInfo[2]
    p_right  = trialInfo[3]
    rand_id  = trialInfo[4]
    trial_id = trialInfo[5]

    if left is None:
        behavior = -1
        wrong_move = 1
    else:
        behavior = int(left)        

    # Add flag for trial end and stop recording    
    if useTracker == True:
        tracker.set_begaze_key_press("end_trial_" + str(order) + 
                                     "_pl_" + str(p_left) + 
                                     "_rl_" + str(r_left) + 
                                     "_pr_" + str(p_right) + 
                                     "_rr_" + str(r_right))    
        tracker.stop_recording() 

    behaviorLog.write(str(trialStartTime)+","+str(clock.getTime())+","
                      +str(trial)+","+str(order)+","
                      +str(rand_id)+","+str(trial_id)+","
                      +str(r_left)+","+str(r_right)+","+str(p_left)+","+str(p_right)+","
                      +str(behavior)+"\n")

    for i in range(len(mousex)):
        mouseLog.write(str(mouset[i])+","+str(mousef[i])+","+str(trial)+","
                       +str(rand_id)+","+str(trial_id)+","
                       +str(order)+","+str(mousex[i])+","+str(mousey[i])+"\n")

    mousex = []
    mousey = []
    mouset = []
    mousef = []

    trial += 1
    order += 1

    if trial == TRIALS_PER_BLOCK[block]:
        trial = 0
        block += 1
        if block == len(TRIALS_PER_BLOCK):  # no more blocks
            state = END_STATE
        else:
            state = REST_STATE
        endBlock()
    else:
        if wrong_move == 0:
            state = PAUSE_STATE
        else:
            state = ERROR_STATE
    timer.reset(randomISI())
    turnOffBoxes()
    
### MAIN PROGRAM LOOP ###

## Lauch eyetracker calibration routine
if useTracker == True:
    tracker.calibrate(window)
    
while True:
    if(state == START_STATE):
        startText.autoDraw = True
        if('space' in event.getKeys()):
            timer.reset(randomISI())
            state = ISI_STATE
            startTrial()
            startBlock()
            startText.autoDraw = False
            # if useTracker == True:
            #     tracker.start_recording()
            
    elif(state == TRIAL_STATE):
        # If mouse has moved since trial start, give cues
        window.mouseVisible = True
        mousePos = np.array(mouse.getPos())
        resetPos = np.array(RESET_POINT)
        testPos = mousePos == resetPos
        if(not testPos.all()):
            turnOnBoxes()
            mouse_tracking_on = True

        # Check for response (left or right)
        if(leftBox.contains(mouse)):
            stopTrial(True)
            mouse_tracking_on = False
        elif(rightBox.contains(mouse)):
            stopTrial(False)
            mouse_tracking_on = False
            
        # actually do the tracking
        if(mouse_tracking_on):
            (x, y) = mouse.getPos()
            mousex.append(x)
            mousey.append(y)
            mouset.append(clock.getTime())
            mousef.append(framecount)            

            # If mouse has stopped for 18 consecutive frames (300 ms),
            # then immediately stop the trial
            if mouseTest != None and sum(mouse.getPos() == mouseLastPos) == 2 and sum(mouse.getPos() == RESET_POINT) < 2:
                mouseStoppedCount += 1
            else:
                mouseStoppedCount = 0

            if(mouseStoppedCount > 50):
                stopTrial(None)
                mouseStoppedCount = 0
                mouse_tracking_on = False
                
            mouseTest = 1
            mouseLastPos = mouse.getPos()

    elif(state == PAUSE_STATE):
        turnOnButton() # Move on only after clicking the start button
        
        if(mouse.isPressedIn(buttonBody)):
            turnOffButton()
            timer.reset(randomISI())
            state = ISI_STATE

    elif(state == ERROR_STATE):
        errorText.autoDraw = True
        turnOnButton() # Move on only after the clicking the start button
        
        if(mouse.isPressedIn(buttonBody)):
            errorText.autoDraw = False
            turnOffButton()
            timer.reset(randomISI())
            state = ISI_STATE        

    elif(state == ISI_STATE):
        turnOnFixTarget()
        if(timer.getTime() <= 0):
            state = TRIAL_STATE
            turnOffFixTarget()
            startTrial()
            
    elif(state == REST_STATE):
        restText.autoDraw = True
        rest_choice = event.getKeys()
        if('space' in rest_choice):
            timer.reset(randomISI())
            state = ISI_STATE
            startTrial()
            startBlock()
            restText.autoDraw = False
        elif('r' in rest_choice):
            restText.autoDraw = False
            state = CALIB_STATE
    
    elif(state == CALIB_STATE):
        if useTracker == True:
            tracker.calibrate(window)
            state = REST_STATE      
            
    elif(state == END_STATE):
        endText.autoDraw = True
    
    if('escape' in event.getKeys()):
        
        endBlock()
        window.close() 
        core.quit()
    
    window.update()
    framecount += 1
    

