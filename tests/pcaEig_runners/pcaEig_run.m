function pcaEig_run(varargin)
    % This script runs the pcaEig function and saves the results to a CSV file.
    %
    % It takes command-line arguments to specify the input data and optional parameters:
    %
    % Usage: pcaEig_run <X> [Name-Value Pairs]
    %   - <X>: String representation of the data matrix X OR path to a CSV file.
    %   - [Name-Value Pairs]: Optional parameters for pcaEig, e.g., 'PCs', [1, 2, 3].
    %
    % Optional Parameters:
    %   - 'PCs': Principal Components to consider (default: 0:rank(X))
    %
    % Example usage in MATLAB:
    %   matlab -nodisplay -r "pcaEig_run('rand(40, 100)')"
    %   matlab -nodisplay -r "pcaEig_run('rand(40, 100)', 'PCs', '[1 2 3]')"
    %
    % Example usage in Octave:
    %   octave --no-gui -q pcaEig_run.m "rand(40, 100)"
    %   octave --no-gui -q pcaEig_run.m "rand(40, 100)" "PCs" "[1 2 3]"
    %
    % You can also replace the matrix with a path to a CSV file:
    %   pcaEig_run('data.csv')
    %   pcaEig_run('data.csv', 'PCs', '[1 2 3]')
    %
    % Outputs:
    %   - Saves the results as 'pcaEig_results.csv'
    %   - Results include: scores, loads, variance, and other model information
    
    % Get command-line arguments
    if isoctave()
        args = argv();
    else
        args = varargin;
    end
    
    % Check number of arguments (at least X must be provided)
    if numel(args) < 1
        error(['Not enough arguments. Usage: pcaEig_run <X> [Name-Value Pairs]\n\n', ...
               'Optional Parameters:\n', ...
               '- PCs: Principal Components to consider (e.g., ''[1 2 3]'', default: all)']);
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
    
    % Parse optional name-value pairs
    optionalArgs = {};
    if numel(args) > 1
        % Remaining arguments are name-value pairs
        optionalArgs = args(2:end);
    end

    % Convert numeric parameters to numbers
    numericParams = {"PCs"};
    
    for i = 1:2:numel(optionalArgs)-1
        paramName = optionalArgs{i};
        paramValue = optionalArgs{i+1};
        
        if ismember(paramName, numericParams)
            % Convert to numeric value
            optionalArgs{i+1} = str2num(paramValue); 
        end
    end

    % Add path to pcaEig.m script
    addpath('../matlab');

    % Call pcaEig function with optional arguments
    model = pcaEig(X, optionalArgs{:});
    
    % Open the CSV file for writing
    fid = fopen('pcaEig_results.csv', 'w');
    
    % Write header
    fprintf(fid, 'type,value\n');
    
    % Write model type
    fprintf(fid, 'type,%s\n', model.type);
    
    % Write variance
    fprintf(fid, 'variance,%f\n', model.var);
    
    % Write latent variables
    fprintf(fid, 'latent_variables,%s\n', mat2str(model.lvs));
    
    % Write scores matrix
    fprintf(fid, 'scores,');
    for i = 1:size(model.scores, 1)
        for j = 1:size(model.scores, 2)
            fprintf(fid, '%f', model.scores(i, j));
            if j < size(model.scores, 2)
                fprintf(fid, ',');
            end
        end
        if i < size(model.scores, 1)
            fprintf(fid, ';');
        end
    end
    fprintf(fid, '\n');
    
    % Write loadings matrix
    fprintf(fid, 'loadings,');
    for i = 1:size(model.loads, 1)
        for j = 1:size(model.loads, 2)
            fprintf(fid, '%f', model.loads(i, j));
            if j < size(model.loads, 2)
                fprintf(fid, ',');
            end
        end
        if i < size(model.loads, 1)
            fprintf(fid, ';');
        end
    end
    fprintf(fid, '\n');
    
    % Close the file
    fclose(fid);
    
    fprintf('Results saved to pcaEig_results.csv\n');
end

function tf = isoctave()
    % Check if the environment is Octave
    tf = exist('OCTAVE_VERSION', 'builtin') ~= 0;
end

pcaEig_run();