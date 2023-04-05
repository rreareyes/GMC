%% FIXATION DETECTION USING THE IDENTIFICATION BY 2-MEANS CLUSTERING (I2MC) ALGORITHM
% Description:
% The I2MC algorithm was designed to accomplish fixation detection in data
% across a wide range of noise levels and when periods of data loss may
% occur.

% Cite as:
% Hessels, R.S., Niehorster, D.C., Kemner, C., & Hooge, I.T.C., (2017).
% Noise-robust fixation detection in eye-movement data - Identification by 
% 2-means clustering (I2MC). Behavior Research Methods, 49(5):1802–1823.

% Version:
% v2.0.3

% For more information, questions, or to check whether we have updated to a
% better version, e-mail: royhessels@gmail.com / dcnieho@gmail.com. I2MC is
% available from www.github.com/royhessels/I2MC

% Most parts of the I2MC algorithm are licensed under the Creative Commons
% Attribution 4.0 (CC BY 4.0) license. Some functions are under MIT 
% license, and some may be under other licenses.

% Modified by Eduardo Rea
% GMC project
% NLP Lab UMass AMherst
% June 2022

%% INITIALIZE
clear variables; clear mex; close all; fclose('all'); clc;
dbstop if error;
commandwindow;

%% Directories
% Data folder should be structured by one folder for each participant with
% the eye-tracking data in textfiles in each folder.
cd('../../')
folders.root    = pwd;
folders.data    = fullfile(folders.root, 'data', 'tracker', 'split'); % folder in which data is stored (each folder in folders.data is considered 1 subject)
folders.output  = fullfile(folders.root, 'results', 'datasets'); % folder for output (will use structure in folders.data for saving output)
folders.scripts = fullfile(folders.root, 'bin', 'analysis');

file.output = fullfile(folders.output, 'preproc_fixations.csv');

%% NECESSARY VARIABLES
% General variables for eye-tracking data
opt.xres     = 1920; % maximum value of horizontal resolution in pixels
opt.yres     = 1080; % maximum value of vertical resolution in pixels
opt.missingx = -opt.xres; % missing value for horizontal position in eye-tracking data (example data uses -xres). used throughout functions as signal for data loss
opt.missingy = -opt.yres; % missing value for vertical position in eye-tracking data (example data uses -yres). used throughout functions as signal for data loss
opt.freq     = 250; % sampling frequency of data (check that this value matches with values actually obtained from measurement!)

% Variables for the calculation of visual angle
% These values are used to calculate noise measures (RMS and BCEA) of
% fixations. The may be left as is, but don't use the noise measures then.
% If either or both are empty, the noise measures are provided in pixels
% instead of degrees.
opt.scrSz        = [34.5 19.4]; % screen size in cm
opt.disttoscreen = 65; % distance to screen in cm.

% Get I2MC and import functions
folders.functions = fullfile(folders.scripts, 'helpers');
addpath(genpath(folders.functions))

% Plot results
do.plots = 0; % if set to 1, plot of fixation detection for each trial will be saved as png-file in output folder.
% the figures works best for short trials (up to around 20 seconds)

%% OPTIONAL VARIABLES
% The settings below may be used to adopt the default settings of the
% algorithm. Do this only if you know what you're doing. Uncomment the
% settings below and run the algorithm.

% % STEFFEN INTERPOLATION
% opt.windowtimeInterp = 0.1;  % max duration (s) of missing values for interpolation to occur
% opt.edgeSampInterp   = 2;    % amount of data (number of samples) at edges needed for interpolation
% opt.maxdisp          = opt.xres*0.2*sqrt(2); % maximum displacement during missing for interpolation to be possible
% 
% % K-MEANS CLUSTERING
% opt.windowtime   = 0.2;  % time window (s) over which to calculate 2-means clustering (choose value so that max. 1 saccade can occur)
% opt.steptime     = 0.02; % time window shift (s) for each iteration. Use zero for sample by sample processing
% opt.maxerrors    = 100;  % maximum number of errors allowed in k-means clustering procedure before proceeding to next file
% opt.downsamples  = [2 5 10];
opt.downsampFilter = 0; % use chebychev filter when downsampling? 1: yes, 0: no. requires signal processing toolbox. is what matlab's downsampling functions do, but could cause trouble (ringing) with the hard edges in eye-movement data
% 
% % FIXATION DETERMINATION
% opt.cutoffstd   = 2; % number of standard deviations above mean k-means weights will be used as fixation cutoff
opt.maxMergeDist = 30; % maximum Euclidean distance in pixels between fixations for merging
opt.maxMergeTime = 30; % maximum time in ms between fixations for merging
opt.minFixDur    = 40; % minimum fixation duration after merging, fixations with shorter duration are removed from output

%% SET-UP FOLDERS
if ~isdir(folders.output)
    mkdir(folders.output);
end

if ~isdir(folders.data)
    folders.data = uigetdir(folders.root);
end

%% START ALGORITHM
% Go through all folders (one folder is assumed to be one subject)
[subject_folder, n_subjects] = FolderFromFolder(folders.data);

subject_data = cell(n_subjects, 1);

parfor iSubject = 1:n_subjects
       
    data = [];
   
    % Get the number of files in the subject folder
    % If the directory is empty continue to the next subject
    [trial_file, n_files] = FileFromFolder(fullfile(folders.data, subject_folder(iSubject).name), 'silent', 'txt');
    if ~n_files
        disp('Folder is empty, continuing to next subject');
        continue
    end
    
    % Loop through the files from each subject 
    % Each trial should have its own .txt file for optimal behavior
    fix = {};
    trial_label = {};
    
    for iTrial = 1:n_files
       
        %% IMPORT DATA
        fprintf('Importing and processing %s/%s \n', subject_folder(iSubject).name, trial_file(iTrial).name)
        [data.time, data.left.X, data.left.Y, data.right.X, data.right.Y] = importRED250(fullfile(folders.data, subject_folder(iSubject).name,trial_file(iTrial).name), 1, [opt.xres opt.yres], opt.missingx, opt.missingy);
        
        % Check whether we have data, if not, continue to next file
        if length(data.time) < 10
            fprintf('Empty or broken file encountered, continuing to next file \n');
            continue
        end
        
        %% RUN FIXATION DETECTION
        fix{iTrial} = I2MCfunc(data, opt);
        n_fixations = numel(fix{iTrial}.start);
        
        % Create identifiers for each fixation
        trial_label{iTrial} = repmat(string(trial_file(iTrial).name(1:end-4)), n_fixations, 1);
        id_label = split(vertcat(trial_label{1:end}),'_');
               
        
    end
    
    % Merge data from all the trials
    extracted_fixations = vertcat(fix{1:end});
    subject_data{iSubject, 1} = cellstr([id_label, vertcat(extracted_fixations(1:end).startT), vertcat(extracted_fixations(1:end).endT), vertcat(extracted_fixations(1:end).dur), horzcat(extracted_fixations(1:end).xpos).', horzcat(extracted_fixations(1:end).ypos).']);
   
end

%% Merge data from all the subjects and save the output
results = cell2table(vertcat(subject_data{1:end}));
results.Properties.VariableNames = {'subject', 'block', 'trial', 'startT', 'endT', 'duration', 'x', 'y'};

writetable(results, file.output)

%% Get back to start
cd(folders.scripts)
