function createDesign_run(varargin)
    % This script generates a design matrix using createDesign function.
    %
    % It takes command-line arguments to specify the levels of factors and optional parameters:
    %
    % Usage: createDesign_run <levels> [Replicates]
    %   - <levels>: String representation of a cell array with factor levels.
    %   - [Replicates]: (Optional) Integer value specifying the number of replicates.
    %
    % Example usage:
    %   matlab -nodisplay -r "createDesign_run('{[1,2,3,4],[1,2,3]}')"  % Default replicates = 1
    %   matlab -nodisplay -r "createDesign_run('{[1,2,3,4],[1,2,3]}', '4')"  % Replicates = 4
    %   octave --no-gui -q createDesign_run.m "{[1,2,3,4];[1,2,3]}"  % Default replicates = 1
    %   octave --no-gui -q createDesign_run.m "{[1,2,3,4];[1,2,3]}" "4"  % Replicates = 
    %
    % Outputs:
    %   - Saves the generated design matrix as 'design_matrix_matlab.csv'
    
    % Get command-line arguments
    args = argv();
    
    % Check number of arguments (at least levels must be provided)
    if numel(args) < 1
        error('Not enough arguments. Usage: createDesign_run <levels> [Replicates]');
    end
    
    % Parse levels (convert string to cell array)
    levels = eval(args{1});
    if ~iscell(levels)
        error('Levels must be provided as a cell array string.');
    end
    
    % Default number of replicates
    reps = 1;
    
    % Parse optional replicates argument
    if numel(args) >= 2
        reps = str2double(args{2});
        if isnan(reps) || reps < 1
            error('Invalid value for Replicates. Must be a positive integer.');
        end
    end
    
    % Add path to createDesign.m script
    addpath('../matlab');
    
    % Call createDesign function
    F = createDesign(levels, 'Replicates', reps);
    
    % Save result to CSV file
    csvwrite('design_matrix_matlab.csv', F);
end

createDesign_run();
