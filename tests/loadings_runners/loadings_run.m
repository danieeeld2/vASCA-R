function loadings_run(varargin)
    % This script runs the loadings function and saves the plot as an image.
    %
    % It takes command-line arguments to specify the input model and optional parameters:
    %
    % Usage: loadings_run <model> [Name-Value Pairs]
    %   - <model>: String representation of the model structure OR path to a JSON file.
    %   - [Name-Value Pairs]: Optional parameters for loadings.
    %
    % When providing a JSON file:
    %   - The JSON should contain a structure with fields:
    %       - lvs: array of latent variables
    %       - loads: loadings matrix (variables x components)
    %       - scores: scores matrix (optional)
    %
    % Optional Parameters:
    %   - 'PlotType': str
    %       'Scatter': scatterplot (by default)
    %       'Bars': bar plot
    %   - 'Title': (str) title for the plots. Empty by default
    %   - 'VarsLabel': [Mx1] name of the variables (numbers are used by default)
    %   - 'VarsClass': [Mx1] groups for different visualization (a single group by default)
    %   - 'BlurIndex': [1x1] avoid blur when adding labels (1 by default)
    %   - 'Color': Choose a color for your data.
    %       'hsv' for hsv palette
    %       'parula' for parula palette
    %       'okabeIto' for color blindness (by default for multiple classes)
    %
    % Example usage with JSON:
    %   matlab -nodisplay -r "loadings_run('model.json')"
    %   octave --no-gui -q loadings_run.m "model.json" "PlotType" "Bars"
    %
    % Outputs:
    %   - Saves the plot as 'loadings_matlab.png' (or multiple images if multiple plots are generated)

    if isoctave()
        args = argv();
        % Attempt to install and load jsonlab if it's not already loaded
        try
            pkg load jsonlab;
        catch
            try
                fprintf('Attempting to install jsonlab package...\n');
                pkg install -forge jsonlab;
                pkg load jsonlab;
            catch ME
                if ~isempty(strfind(ME.message, 'package not found'))
                    fprintf('Error: The package "jsonlab" was not found on Octave Forge.\n');
                    fprintf('Please ensure you have a working internet connection and that Octave Forge is accessible.\n');
                    fprintf('You might need to try installing it manually from the Octave command line:\n');
                    fprintf('  pkg install -forge jsonlab\n');
                else
                    error('Octave requires the jsonlab package. Error: %s', ME.message);
                end
            end
        end
    else
        args = varargin;
    end

    if numel(args) < 1
        error('Usage: loadings_run <model.json> [Name-Value Pairs]');
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
    requiredFields = {'lvs', 'loads'};
    for i = 1:length(requiredFields)
        if ~isfield(model, requiredFields{i})
            error('Missing required field: %s', requiredFields{i});
        end
    end
    if ~isfield(model, 'scores'), model.scores = []; end

    % Parse optional parameters - FIXED SECTION
    params = {};
    if length(args) > 1
        for i = 2:2:length(args)
            if i+1 > length(args)
                error('Parameter %s needs a value', args{i});
            end
            params{end+1} = args{i};
            param_value = args{i+1};
            if ischar(param_value)
                if (startsWith(param_value, '{') && endsWith(param_value, '}')) || ...
                (startsWith(param_value, '[') && endsWith(param_value, ']'))
                    try
                        evaluated_value = eval(param_value);
                        param_value = evaluated_value;
                    catch
                        fprintf('Warning: Could not evaluate parameter %s value: %s\n', args{i}, param_value);
                    end
                end
            end
            
            % Guardar el valor procesado
            params{end+1} = param_value;
        end
    end

    % Execute loadings function
    addpath('../matlab');
    figHandles = loadings(model, params{:});

    % Save figures
    for i = 1:length(figHandles)
        figure(figHandles(i));
        saveas(gcf, sprintf('loadings_%d_matlab.png', i));
    end
    close all;
end

function tf = isoctave()
    tf = exist('OCTAVE_VERSION', 'builtin') ~= 0;
end

loadings_run();
