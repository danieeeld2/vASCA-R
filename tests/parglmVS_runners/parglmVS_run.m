function parglmVS_run(varargin)
    % This script runs the parglmVS function and saves the results to a CSV file.
    %
    % It takes command-line arguments to specify the input data and optional parameters:
    %
    % Usage: parglmVS_run <X> <F> [Name-Value Pairs]
    %   - <X>: String representation of the data matrix X OR path to a CSV file.
    %   - <F>: String representation of the design matrix F OR path to a CSV file.
    %   - [Name-Value Pairs]: Optional parameters for parglmVS, e.g., 'Model', 'interaction', 'Nested', [1, 2], etc.
    %
    % Optional Parameters:
    %   - 'Model': Model type ('linear', 'interaction', 'full', or custom interactions).
    %   - 'Preprocessing': Preprocessing type (0: none, 1: mean-centering, 2: auto-scaling).
    %   - 'Permutations': Number of permutations (default: 1000).
    %   - 'Ts': Test statistic (0: SSQ, 1: F-value, 2: F-ratio with hierarchy).
    %   - 'Ordinal': Whether factors are ordinal (default: zeros(1, size(F, 2))).
    %   - 'Fmtc': Multiple test correction (0: none, 1: Bonferroni, 2: Holm, 3: Benjamini-Hochberg, 4: Q-value).
    %   - 'Coding': Coding type (0: sum/deviation, 1: reference).
    %   - 'Nested': Nested factors (default: []).
    %
    % Example usage in MATLAB:
    %   matlab -nodisplay -r "parglmVS_run('rand(40, 100)', '(randn(40, 1) > 0) + 1')"
    %   matlab -nodisplay -r "parglmVS_run('rand(40, 100)', '(randn(40, 1) > 0) + 1', 'Model', 'interaction', 'Nested', [1 2])"
    %   matlab -nodisplay -r "parglmVS_run('rand(40, 100)', '(randn(40, 1) > 0) + 1', 'Model', 'interaction', 'Preprocessing', '2', 'Permutations', '1000', 'Ts', 1, 'Ordinal', '0', 'Fmtc', 3, 'Coding', '1', 'Nested', '[1 2]')"
    %
    % Example usage in Octave:
    %   octave --no-gui -q parglmVS_run.m "rand(40, 100)" "(randn(40, 1) > 0) + 1"
    %   octave --no-gui -q parglmVS_run.m "rand(40, 100)" "(randn(40, 1) > 0) + 1" "Model" "interaction" "Nested" "[1 2]"
    %   octave --no-gui -q parglmVS_run.m "rand(40, 100)" "(randn(40, 1) > 0) + 1" "Model" "interaction" "Preprocessing" "2" "Permutations" "1000" "Ts" "1" "Ordinal" "0" "Fmtc" "3" "Coding" "1" "Nested" "[1 2]"
    %
    % You can also replace "parglmVS_run('rand(40, 100)', '(randn(40, 1) > 0) + 1')" with csv files from the data folder.
    %
    % Outputs:
    %   - Saves the results as 'parglmVS_matlab.csv'
    
    % Get command-line arguments
    if isoctave()
        args = argv();
    else
        args = varargin;
    end
    
    % Check number of arguments (at least X and F must be provided)
    if numel(args) < 2
        error(['Not enough arguments. Usage: parglmVS_run <X> <F> [Name-Value Pairs]\n\n', ...
               'Optional Parameters:\n', ...
               '- Model: Model type (''linear'', ''interaction'', ''full'', or custom interactions).\n', ...
               '- Preprocessing: Preprocessing type (0: none, 1: mean-centering, 2: auto-scaling).\n', ...
               '- Permutations: Number of permutations (default: 1000).\n', ...
               '- Ts: Test statistic (0: SSQ, 1: F-value, 2: F-ratio with hierarchy).\n', ...
               '- Ordinal: Whether factors are ordinal (default: zeros(1, size(F, 2))).\n', ...
               '- Fmtc: Multiple test correction (0: none, 1: Bonferroni, 2: Holm, 3: Benjamini-Hochberg, 4: Q-value).\n', ...
               '- Coding: Coding type (0: sum/deviation, 1: reference).\n', ...
               '- Nested: Nested factors (default: []).']);
    end
    
    % Parse X
    argX = args{1};
    if exist(argX, 'file') == 2
        % It's a file, try to load it as a CSV
        try
            X = dlmread(argX, ',');
            fprintf('Loaded X from: %s\n', argX);
        catch ME
            error('Error loading X from CSV file: %s', ME.message);
        end
    else
        % It's likely a string representation of a matrix
        try
            X = eval(argX);
            fprintf('Evaluated X: %s\n', argX);
        catch ME
            error('Error evaluating X: %s\n', ME.message);
        end
    end

    % Parse F
    argF = args{2};
    if exist(argF, 'file') == 2
        % It's a file, try to load it as a CSV
        try
            F = dlmread(argF, ',');
            fprintf('Loaded F from: %s\n', argF);
        catch ME
            error('Error loading F from CSV file: %s', ME.message);
        end
    else
        % It's likely a string representation of a matrix
        try
            F = eval(argF);
            fprintf('Evaluated F: %s\n', argF);
        catch ME
            error('Error evaluating F: %s\n', ME.message);
        end
    end
    
    % Parse optional name-value pairs
    optionalArgs = {};
    if numel(args) > 2
        % Remaining arguments are name-value pairs
        optionalArgs = args(3:end);
    end

    % Convert numeric parameters to numbers
    numericParams = {"Preprocessing", "Permutations", "Ts", "Fmtc", "Coding", "Ordinal"};
    
    for i = 1:2:numel(optionalArgs)-1
        paramName = optionalArgs{i};
        paramValue = optionalArgs{i+1};
        
        if ismember(paramName, numericParams)
            % Convert to numeric value
            optionalArgs{i+1} = str2num(paramValue); 
        end
    end

    % Handling the 'Nested' parameter correctly
    nestedIdx = find(strcmp(optionalArgs, 'Nested'));
    if ~isempty(nestedIdx)
        % Take the next argument as the 'Nested' matrix
        Nested = optionalArgs{nestedIdx + 1}; 
        
        % Convert the string to a matrix
        Nested = eval(Nested);
        fprintf('Nested: %s\n', mat2str(Nested));
        
        % Ensure that Nested is a matrix with 2 columns (nx2)
        if size(Nested, 2) ~= 2
            error('The "Nested" parameter must be a matrix with two columns.');
        end
        
        % Replace 'Nested' parameter with the actual matrix in optionalArgs
        optionalArgs{nestedIdx + 1} = Nested;
    else
        Nested = [];
    end

    % Adjust coding dimensions
    codingIdx = find(strcmp(optionalArgs, 'Coding'));
    if ~isempty(codingIdx)
        codingValue = optionalArgs{codingIdx + 1};
        if numel(codingValue) == 1 && size(F, 2) > 1
            optionalArgs{codingIdx + 1} = repmat(codingValue, 1, size(F, 2));
        end
    end

    % Adjust ordinal dimensions
    ordinalIdx = find(strcmp(optionalArgs, 'Ordinal'));
    if ~isempty(ordinalIdx)
        ordinalValue = optionalArgs{ordinalIdx + 1};
        if numel(ordinalValue) == 1 && size(F, 2) > 1
            optionalArgs{ordinalIdx + 1} = repmat(ordinalValue, 1, size(F, 2));
        end
    end
    
    % Add path to parglmVS.m script
    addpath('../matlab');
    
    % Call parglmVS function with optional arguments
    [T, parglmo] = parglmVS(X, F, optionalArgs{:});
    
    % Open the CSV file for writing
    fid = fopen('parglmVS_matlab.csv', 'w');

    for i = 1:numel(T.data)
        if ~isempty(T.data{i}) && isnumeric(T.data{i})
            row_data = T.data{i};
            fprintf(fid, '%f,', row_data(:));
            fprintf(fid, '\n');
        elseif ~isempty(T.data{i}) && iscell(T.data{i})
            for j = 1:numel(T.data{i})
                if isnumeric(T.data{i}{j})
                    fprintf(fid, '%f,', T.data{i}{j});
                else
                    fprintf(fid, '%s,', num2str(T.data{i}{j}));
                end
                if j < numel(T.data{i})
                    fprintf(fid, ',');
                end
            end
            fprintf(fid, '\n');
        end
    end

    % Close the file
    fclose(fid);
end

function tf = isoctave()
    % Check if the environment is Octave
    tf = exist('OCTAVE_VERSION', 'builtin') ~= 0;
end

parglmVS_run();