function parglmVS_run(varargin)
    % This script runs the parglmVS function and saves the results to a CSV file.
    %
    % It takes command-line arguments to specify the input data and optional parameters:
    %
    % Usage: parglmVS_run <X> <F> [Name-Value Pairs]
    %   - <X>: String representation of the data matrix X.
    %   - <F>: String representation of the design matrix F.
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
    %   matlab -nodisplay -r "parglmVS_run('rand(40, 100)', '(randn(40, 1) > 0) + 1', 'Model', 'interaction', 'Nested', [1, 2])"
    %   matlab -nodisplay -r "parglmVS_run('rand(40, 100)', '(randn(40, 1) > 0) + 1', 'Model', 'interaction', 'Preprocessing', 2, 'Permutations', 1000, 'Ts', 1, 'Ordinal', '[0, 1]', 'Fmtc', 3, 'Coding', '[0, 1]', 'Nested', '[1, 2]')"
    %
    % Example usage in Octave:
    %   octave --no-gui -q parglmVS_run.m "rand(40, 100)" "(randn(40, 1) > 0) + 1"
    %   octave --no-gui -q parglmVS_run.m "rand(40, 100)" "(randn(40, 1) > 0) + 1" "Model" "interaction" "Nested" "[1, 2]"
    %   octave --no-gui -q parglmVS_run.m "rand(40, 100)" "(randn(40, 1) > 0) + 1" "Model" "interaction" "Preprocessing" "2" "Permutations" "1000" "Ts" "1" "Ordinal" "[0, 1]" "Fmtc" "3" "Coding" "[0, 1]" "Nested" "[1, 2]"
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
    
    % Parse X and F (convert string to matrix)
    X = eval(args{1});
    F = eval(args{2});
    
    % Parse optional name-value pairs
    optionalArgs = {};
    if numel(args) > 2
        % Remaining arguments are name-value pairs
        optionalArgs = args(3:end);
    end
    
    % Add path to parglmVS.m script
    addpath('../matlab');
    
    % Call parglmVS function with optional arguments
    [T, parglmo] = parglmVS(X, F, optionalArgs{:});
    
    % Open the CSV file for writing
    fid = fopen('parglmVS_matlab.csv', 'w');

    % Write the headers (labels)
    fprintf(fid, '%s,', T.labels{:});
    fprintf(fid, '\n');  % New line after headers

    % Write the data from each cell
    for i = 1:numel(T.data)
        data = T.data{i};  % Get the content of the cell
        
        % Check if the data is numeric
        if isnumeric(data)
            % If numeric, convert to text and write
            fprintf(fid, '%f,', data);
        else
            % If not numeric, convert to text
            fprintf(fid, '%s,', num2str(data));
        end
        fprintf(fid, '\n');  % New line after each cell
    end

    % Close the file
    fclose(fid);
end

function tf = isoctave()
    % Check if the environment is Octave
    tf = exist('OCTAVE_VERSION', 'builtin') ~= 0;
end

parglmVS_run();