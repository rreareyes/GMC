%% Logistic Regression and Variational Bayes

%

%% Authorship
% Eduardo Rea for project "GMC"
% NLP Lab UMass Amherst
% July 2022

%% Clear the work space
clear; clc

%% Base directories
folder.Root    = fileparts(fileparts(cd));
folder.Scripts = fullfile(folder.Root, 'bin', 'analysis');
folder.Results = fullfile(folder.Root, 'results', 'datasets'); 
folder.Keys    = fullfile(folder.Root, 'results', 'keys');

if ~ exist(folder.Results, 'dir')
    mkdir(folder.Results)
end

if ~ exist(folder.Keys, 'dir')
    mkdir(folder.Keys)
end

%% Define output files
file.Win = fullfile(folder.Keys, 'model_classification.csv');
file.BF  = fullfile(folder.Results, 'bf_model.csv');

file.Weights = fullfile(folder.Results, 'beta_weights.csv');
file.Exceedance = fullfile(folder.Results, 'exceedance_probability.csv');

%% Define paths to input dataset
file.Behavior = fullfile(folder.Results, 'model_input.csv');

%% Load behavior data
raw = readtable(file.Behavior);
data.matrix = table2array(raw);

%% Set parameters from the experiment
params.SubjID       = unique(raw.id); 
params.respLocation = 3;
params.idealObs     = 1; %Reference Model will be the first

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
params.Model{1}  = [8, 9]; % reference: delta, actual numbers
params.Model{2}  = 8;
params.Model{3}  = 9;
params.Model{4}  = [10, 11]; %ratio
params.Model{5}  = 10; 
params.Model{6}  = 11;
params.Model{7}  = [12, 13]; %ratio levels
params.Model{8}  = 12;
params.Model{9}  = 13;
params.Model{10} = 16; %expected value
params.Model{11} = 17; %expected value ratio
params.Model{12} = 18; %subjective value (power) delta
params.Model{13} = [19, 20]; %delta levels
params.Model{14} = 19;
params.Model{15} = 20;
params.Model{16} = [21, 22]; %delta truncated 
params.Model{17} = 21; 
params.Model{18} = 22; 
params.Model{19} = [23, 24]; %delta truncated levels
params.Model{20} = 23;
params.Model{21} = 24; 
params.Model{22} = [25, 26]; %tallying
params.Model{23} = 25;
params.Model{24} = 26;
params.Model{25} = 27; %tallying sum

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
        [w, V, invV, logdetV, E_a, L] = vb_logit_fit([ones(size(bayes.Regressors{iSubject, iModel}, 1), 1) bayes.Regressors{iSubject, iModel}], bayes.Outcome{iSubject});

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