%% Logistic Regression and Variational Bayes

%

%% Authorship
% Eduardo Rea for project "GMC"
% NLP Lab UMass Amherst
% October 2021

%% Clear the work space
clear; clc

%% Base directories
cd('..')
folder.Root    = pwd;
folder.Scripts = fullfile(folder.Root, 'Scripts');
folder.Results = fullfile(folder.Root, 'Results', 'Datasets'); 
folder.Keys    = fullfile(folder.Root, 'Results', 'Keys');

%% Define output files
file.Win = fullfile(folder.Keys, 'model_classification_ratio.csv');
file.BF  = fullfile(folder.Results, 'bf_model_ratio.csv');

file.Weights = fullfile(folder.Results, 'beta_weights_ratio.csv');
file.Exceedance = fullfile(folder.Results, 'exceedance_probability_ratio.csv');

%% Define paths to input dataset
file.Behavior = fullfile(folder.Results, 'model_data_ratio.csv');

%% Load behavior data
raw = readtable(file.Behavior);
data.matrix = table2array(raw);

%% Set parameters from the experiment
params.SubjID       = unique(raw.id); 
params.Stage        = unique(raw.stage);
params.respLocation = 4;
params.idealObs     = 1;

%% Get the number of participants processed
params.nSubj  = length(params.SubjID);

%% LOGISTIC REGRESSION
% Loop through all the subjects and perform logistic regression in the data
% from each phase separately.

% Increase the number of iterations
opts = statset('glmfit');
opts.MaxIter = 100; % default value for glmfit is 100.

%% MODEL COMPARISON USING VARIATIONAL BAYES
%% Model construction
params.Model{1}  = [5,6]; % reference: reward, probability actual numbers
params.Model{2}  = 5;
params.Model{3}  = 6;
params.Model{4}  = 7; %expected value
params.Model{5}  = [8,9]; % relative difference: reward, prob
params.Model{6}  = 8;
params.Model{7}  = 9;
params.Model{8}  = [10,11]; %levels: reward, prob
params.Model{9}  = 10;
params.Model{10} = 11;
params.Model{11} = [12,13]; %truncated values (only tens considered)
params.Model{12} = 12;
params.Model{13} = 13;
params.Model{14} = [14,15]; %levels truncated: reward, prob
params.Model{15} = 14;
params.Model{16} = 15;
params.Model{17} = [16,17]; %tally: reward, prob
params.Model{18} = 16; 
params.Model{19} = 17;
params.Model{20} = 18; %tally sum
% Probability priority ttb
params.Model{21} = [19,20]; %magnitude:%PttbA (zero dif < 14)
params.Model{22} = [21,22];            %PttbB (zero dif < 27)
params.Model{23} = [23,24];            %PttbC (zero dif < 40)
params.Model{24} = [25,26];            %PttbD (zero dif < 53)
params.Model{25} = [27,28];            %PttbE (zero dif < 66)

params.Model{26} = [29,30]; %level:%PttbA (zero dif < 14)
params.Model{27} = [31,32];        %PttbB (zero dif < 27)
params.Model{28} = [33,34];        %PttbC (zero dif < 40)
params.Model{29} = [35,36];        %PttbD (zero dif < 53)
params.Model{30} = [37,38];        %PttbE (zero dif < 66)

params.Model{31} = [39,40]; %tally:%PttbA (zero dif < 14)
params.Model{32} = [41,42];        %PttbB (zero dif < 27)
params.Model{33} = [43,44];        %PttbC (zero dif < 40)
params.Model{34} = [45,46];        %PttbD (zero dif < 53)
params.Model{35} = [47,48];        %PttbE (zero dif < 66)

% Reward priority ttb
params.Model{36} = [49,50]; %magnitude:%RttbA (zero dif < 14)
params.Model{37} = [51,52];            %RttbB (zero dif < 27)
params.Model{38} = [53,54];            %RttbC (zero dif < 40)
params.Model{39} = [55,56];            %RttbD (zero dif < 53)
params.Model{40} = [57,58];            %RttbE (zero dif < 66)

params.Model{41} = [59,60]; %level:%RttbA (zero dif < 14)
params.Model{42} = [61,62];        %RttbB (zero dif < 27)
params.Model{43} = [63,64];        %RttbC (zero dif < 40)
params.Model{44} = [65,66];        %RttbD (zero dif < 53)
params.Model{45} = [67,68];        %RttbE (zero dif < 66)

params.Model{46} = [69,70]; %tally:%RttbA (zero dif < 14)
params.Model{47} = [71,72];        %RttbB (zero dif < 27)
params.Model{48} = [73,74];        %RttbC (zero dif < 40)
params.Model{49} = [75,76];        %RttbD (zero dif < 53)
params.Model{50} = [77,78];        %RttbE (zero dif < 66)

% Ratio levels
params.Model{51} = [79,80]; %Prob and Reward
params.Model{52} = 79;      %Probability
params.Model{53} = 80;      %Reward


%% Get number of models
params.nModel = size(params.Model, 2); % total number of models

%% Variational Bayes
bayes.winModel = NaN(params.nSubj, 1);
betas = [];

for iSubject = 1 : params.nSubj
    bayes.w{iSubject}          = NaN(params.nModel, 3);
    bayes.w10{iSubject}        = NaN(params.nModel, 3);
    bayes.diagV{iSubject}      = NaN(params.nModel, 3);
      
    bayes.Base = data.matrix(data.matrix(:, 1) == params.SubjID(iSubject), :);
    bayes.Outcome{iSubject} = bayes.Base(:, params.respLocation);
    
    for iModel = 1 : params.nModel

        bayes.indCueModel = 1:size(params.Model{iModel}, 2); %number of predictors
        bayes.Regressors{iSubject, iModel} = bayes.Base(:, params.Model{iModel});
                
        % Run variational bayes using noninformative priors
        [w, V, invV, logdetV, E_a, L] = bayes_logit_fit([ones(size(bayes.Regressors{iSubject, iModel}, 1), 1) bayes.Regressors{iSubject, iModel}], bayes.Outcome{iSubject});

        bayes.w{iSubject}(iModel, [1 bayes.indCueModel + 1])     = w; % cue weights
        bayes.w10{iSubject}(iModel, [1 bayes.indCueModel + 1])   = log10(exp(w));
        bayes.diagV{iSubject}(iModel, [1 bayes.indCueModel + 1]) = diag(V); % variance of w
        bayes.E_a(iSubject, iModel)                              = E_a;
        bayes.L(iSubject, iModel)                                = L; % log-likelihood of the data given the model (lower bound); larger L = better fit

        clear w V invV logdetV E_a L w2 V2 invV2 logdetV2 E_a2 L2

    end

    %% Weights using the optimal model (model 15)
    bayes.optW10(iSubject, :)      = bayes.w10{iSubject}(params.idealObs, :); % optimal weights

    %% Adjust log-likihood based on the optimal model
    bayes.adjL(iSubject, :)      = bayes.L(iSubject, :) - bayes.L(iSubject, params.idealObs);
    
    %% Calculate bayes factor
    bayes.BF(iSubject, :)      = exp(bayes.adjL(iSubject,:));
    
    %% Find the winning model (BF > 3)
    [wm,iwm] = sort(bayes.BF(iSubject,:),'descend'); %iwm tells us the position (i.e. the model); wm is the BF from that model
    if wm(1) > 3
        bayes.winModel(iSubject) = iwm(1);

    else % winning model is the optimal model
        bayes.winModel(iSubject) = params.idealObs; 

    end

    id_matrix = sort(repmat(params.SubjID(iSubject), params.nModel, 1));
    model_matrix = [1:params.nModel]';
    
    betas = [betas; [id_matrix, model_matrix, bayes.w{1, iSubject}]];
        
end

%% Group Model selection
[bayes.alpha(:), bayes.exp_r(:), bayes.xp(:)] = spm_BMS(bayes.L);


%% Export databases with winning models and the BF for each model

% Create variable names
model_cells = cell(1,length(params.Model)+1);
model_cells(:) = {'model'};
model_names = genvarname(model_cells);
model_names(1) = [];

bf_model = array2table([params.SubjID, bayes.BF], 'VariableNames', ['id', model_names]);
win_model = array2table([params.SubjID, bayes.winModel], 'VariableNames', {'id', 'model'});
beta_weigths = array2table(betas, 'VariableNames', {'id', 'model', 'intercept', 'x1', 'x2'});
exceedance_model = array2table(bayes.xp, 'VariableNames', model_names);

writetable(bf_model, file.BF) 
writetable(win_model, file.Win)
writetable(beta_weigths, file.Weights)
writetable(exceedance_model, file.Exceedance)

%% Go back to start
cd(folder.Scripts)