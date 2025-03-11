function run_preprocess2D(varargin)
    % This script processes a dataset using the preprocess2D function.
    %
    % It takes command-line arguments to specify the dataset and optional parameters:
    %
    % Usage: run_preprocess2D <dataset> [Preprocessing] [Weights]
    %   - <dataset>: Path to the dataset (must be a CSV file).
    %   - [Preprocessing]: (Optional) Integer value {0, 1, 2, 3} specifying the preprocessing type.
    %   - [Weights]: (Optional) A 1xM vector specifying feature weights.
    %
    % Argument handling:
    % - If only the dataset is provided, the default preprocessing type is 2 (auto-scaling),
    %   and all weights are set to 1.
    % - If the second argument is a valid preprocessing type, it is used; otherwise, it is
    %   interpreted as the weights vector.
    % - If both optional parameters are given, the first is treated as the preprocessing type
    %   and the second as the weights vector.
    %
    % Example usage:
    %   matlab -nodisplay -r "run_preprocess2D('dataset.csv')"  % Default settings
    %   matlab -nodisplay -r "run_preprocess2D('dataset.csv', '1')"  % Preprocessing = 1
    %   matlab -nodisplay -r "run_preprocess2D('dataset.csv', '[0.5 1.5 1.0 0.8]')"  % Custom weights
    %   matlab -nodisplay -r "run_preprocess2D('dataset.csv', '1', '[0.5 1.5 1.0 0.8]')"  % Both params

    % Get arguments
    args = argv();

    % Check number of arguments (we need at least one dataset)
    if numel(args) < 1
        error('Not enough arguments. Usage: preprocess2D_run <dataset> [Preprocessing] [Weights]');
    end

    dataset = args{1};  % Get dataset

    % Check that it is a CSV file
    if ~strcmp(dataset(end-3:end), '.csv')
        error('Dataset must be a CSV file. Got: %s', dataset);
    end

    % Add path to preprocess2D.m script
    addpath('../matlab');

    % Read dataset from CSV file
    X = csvread(dataset);
    M = size(X, 2);         % Number of columns

    % Default values
    preprocessing = 2;      % Default: auto-scaling
    weights = ones(1, M);   % Default: vector of ones

    % Parse optional parameters
    paramList = {}; % Store name-value pairs

    if numel(args) >= 2
        val = str2double(args{2});
        if ~isnan(val) && ismember(val, [0, 1, 2, 3]) % Check if it's a valid preprocessing type
            preprocessing = val;
            paramList = [paramList, {'Preprocessing', preprocessing}];
        else
            weights = str2num(args{2}); % Convert string to vector
            if isempty(weights) || numel(weights) ~= M
                error('Invalid weights vector. Must be a 1xM vector.');
            end
            paramList = [paramList, {'Weights', weights}];
        end
    end

    if numel(args) >= 3
        weights = str2num(args{3});
        if isempty(weights) || numel(weights) ~= M
            error('Invalid weights vector. Must be a 1xM vector.');
        end
        paramList = [paramList, {'Weights', weights}];
    end

    % Call preprocess2D with optional parameters
    [Xcs, av, sc] = preprocess2D(X, paramList{:});

    % Save results to CSV files
    csvwrite('preprocess2D_matlab.csv', Xcs);
    csvwrite('average_matlab.csv', av);
    csvwrite('scale_matlab.csv', sc);
end

run_preprocess2D();
