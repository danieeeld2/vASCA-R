function scores_run(varargin)
    % This script runs the scores function and saves the plot as an image.
    %
    % It takes command-line arguments to specify the input model and optional parameters:
    %
    % Usage: scores_run <model> [Name-Value Pairs]
    %   - <model>: String representation of the model structure OR path to a JSON file.
    %   - [Name-Value Pairs]: Optional parameters for scores.
    %
    % When providing a JSON file:
    %   - The JSON should contain a structure with fields:
    %       - lvs: array of latent variables
    %       - loads: loadings matrix (variables x components)
    %       - scores: scores matrix
    %       - var: total variance
    %       - av: centering parameters (optional)
    %       - sc: scaling parameters (optional)
    %
    % Optional Parameters:
    %   - 'ObsTest': [LxM] data set with observations to be visualized
    %   - 'PlotType': str
    %       'Scatter': scatterplot (by default)
    %       'Bars': bar plot
    %   - 'PlotCal': bool
    %       false: plot only test data
    %       true: plot both calibration and test (by default)
    %   - 'Title': (str) title for the plots. Empty by default
    %   - 'ObsLabel': [Kx1] name of the observations (numbers are used by default)
    %   - 'ObsClass': [Kx1] groups for different visualization (a single group by default)
    %   - 'BlurIndex': [1x1] avoid blur when adding labels (1 by default)
    %   - 'Color': Choose a color for your data.
    %       'hsv' for hsv palette
    %       'parula' for parula palette
    %       'okabeIto' for color blindness (by default for multiple classes)
    %
    % Example usage with JSON:
    %   matlab -nodisplay -r "scores_run('model.json')"
    %   octave --no-gui -q scores_run.m "model.json" "PlotType" "Bars"
    %
    % Outputs:
    %   - Saves the plot as 'scores_matlab.png' (or multiple images if multiple plots are generated)

    if isoctave()
        args = argv();
        % Attempt to install and load jsonlab if it's not already loaded
        % try
        %     pkg load jsonlab;
        % catch
        %     try
        %         fprintf('Attempting to install jsonlab package...\n');
        %         pkg install -forge jsonlab;
        %         pkg load jsonlab;
        %     catch ME
        %         if ~isempty(strfind(ME.message, 'package not found'))
        %             fprintf('Error: The package "jsonlab" was not found on Octave Forge.\n');
        %             fprintf('Please ensure you have a working internet connection and that Octave Forge is accessible.\n');
        %             fprintf('You might need to try installing it manually from the Octave command line:\n');
        %             fprintf('  pkg install -forge jsonlab\n');
        %         else
        %             error('Octave requires the jsonlab package. Error: %s', ME.message);
        %         end
        %     end
        % end
    else
        args = varargin;
    end

    if numel(args) < 1
        error('Usage: scores_run <model.json> [Name-Value Pairs]');
    end

    % Json file loading
    argModel = args{1};
    if exist(argModel, 'file') ~= 2
        error('File not found: %s', argModel);
    end

    try
        fid = fopen(argModel, 'r');
        if fid == -1, error('Cannot open file'); end
        str = fread(fid, inf, '*char')';
        fclose(fid);
        model = jsondecode(str);
        fprintf('Model loaded from: %s\n', argModel);
        disp(['Class of model: ', class(model)]); % Debugging line
        if isstruct(model)
            disp('Model is a struct.');
            disp(fieldnames(model));
        elseif iscell(model)
            disp('Model is a cell.');
            disp(size(model));
        else
            disp('Model is neither a struct nor a cell.');
        end
    catch ME
        error('JSON parsing failed: %s', ME.message);
    end

    % Model validation
    requiredFields = {'lvs', 'loads', 'scores', 'var'};
    for i = 1:length(requiredFields)
        if ~isfield(model, requiredFields{i})
            error('Missing required field: %s', requiredFields{i});
        end
    end
    if ~isfield(model, 'av'), model.av = []; end
    if ~isfield(model, 'sc'), model.sc = []; end

    num_observations = size(model.scores, 1);
    fprintf('Number of observations in scores matrix: %d\n', num_observations);

    % Parse optional parameters
    params = {};
    if length(args) > 1
        for i = 2:2:length(args)
            if i+1 > length(args)
                error('Parameter %s needs a value', args{i});
            end
            param_name = args{i};
            param_value = args{i+1};

            % Special handling for ObsLabel
            if strcmp(param_name, 'ObsLabel')
                % Convert string representation of cell array to actual cell array
                if ischar(param_value) && startsWith(param_value, '{') && endsWith(param_value, '}')
                    try
                        % Remove braces and split by semicolons
                        clean_str = param_value(2:end-1);
                        elements = strsplit(clean_str, ';');
                        % Trim whitespace and remove quotes
                        elements = strtrim(elements);
                        elements = strrep(elements, '''', '');
                        param_value = elements(:);  % Ensure column vector
                    catch
                        warning('Could not parse ObsLabel value: %s', param_value);
                        param_value = [];
                    end
                end
            % Handle other special parameters
            elseif strcmp(param_name, 'ObsClass')
                if ischar(param_value)
                    try
                        evaluated_value = eval(param_value);
                        if ~isvector(evaluated_value)
                            warning('ObsClass value should evaluate to a vector.');
                            param_value = [];
                        else
                            param_value = evaluated_value(:); % Ensure column vector
                            if length(param_value) ~= num_observations
                                warning('Number of elements in ObsClass (%d) does not match the number of observations (%d). Setting ObsClass to empty.', length(param_value), num_observations);
                                param_value = [];
                            end
                        end
                    catch
                        warning('Could not parse ObsClass value: %s', param_value);
                        param_value = [];
                    end
                end
            elseif strcmp(param_name, 'BlurIndex')
                if ischar(param_value)
                    try
                        param_value = str2double(param_value);
                    catch
                        warning('Could not parse BlurIndex value: %s', param_value);
                        param_value = 1; % Default value
                    end
                end
            elseif strcmp(param_name, 'PlotCal') % Add this block to handle PlotCal
                if strcmp(param_value, 'true')
                    param_value = true;
                elseif strcmp(param_value, 'false')
                    param_value = false;
                else
                    warning('Could not parse PlotCal value: %s. Using default (true).', param_value);
                    param_value = true; % Default value
                end
            end

            params{end+1} = param_name;
            params{end+1} = param_value;
        end
    end

    % Execute scores function
    addpath('../matlab');
    figHandles = scores(model, params{:});

    % Save figures
    for i = 1:length(figHandles)
        figure(figHandles(i));
        saveas(gcf, sprintf('scores_%d_matlab.png', i));
    end
    close all;
end

function tf = isoctave()
    tf = exist('OCTAVE_VERSION', 'builtin') ~= 0;
end

scores_run();