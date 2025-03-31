function vasca_run(varargin)
    % This script runs the vasca function and saves the results to a JSON file.
    %
    % It takes command-line arguments to specify the input data and optional parameters:
    %
    % Usage: vasca_run <parglmVS> <siglev>
    %   - <parglmVS>: The output structure from the parglmVS function (in JSON format).
    %   - <siglev>: Significance level (0.01 by default).
    %
    % Example usage in MATLAB:
    %   matlab -nodisplay -r "vasca_run('parglmVS_output.json', 0.05)"
    %   matlab -nodisplay -r "vasca_run('parglmVS_output.json')"  % uses default siglev of 0.01
    %
    % Outputs:
    %   - Saves the results as 'vasca_results.json'
    
    % Get command-line arguments
    if isoctave()
        args = argv();
    else
        args = varargin;
    end
    
    % Check number of arguments (at least parglmVS must be provided)
    if numel(args) < 1
        error(['Not enough arguments. Usage: vasca_run <parglmVS> <siglev>\n']);
    end
    
    % Parse parglmVS (path to the JSON file)
    argParglmVS = args{1};
    if exist(argParglmVS, 'file') == 2
        % It's a file, try to load it as a JSON file
        try
            fid = fopen(argParglmVS, 'r');
            if fid == -1
                error('Unable to open file: %s', argParglmVS);
            end
            raw = fread(fid, inf);
            str = char(raw');
            fclose(fid);
            parglmoVS = jsondecode(str); % Decode the JSON content
            fprintf('Loaded parglmVS from: %s\n', argParglmVS);
        catch ME
            error('Error loading parglmVS from JSON file: %s', ME.message);
        end
    else
        error('parglmVS must be a valid file.');
    end

    % Parse siglev (if provided, otherwise use default 0.01)
    if numel(args) > 1
        siglev = str2double(args{2});
        if isnan(siglev)
            error('The significance level must be a valid number.');
        end
        fprintf('Using significance level: %f\n', siglev);
    else
        siglev = 0.01;  % Default value
        fprintf('Using default significance level: %f\n', siglev);
    end

    % Add the vasca function to the path
    addpath('../matlab');

    % Call the vasca function
    vascao = vasca(parglmoVS, siglev);
    
    % Save results to JSON
    fid = fopen('vasca_matlab.json', 'w');
    if fid == -1
        error('Unable to open file to save results.');
    end
    fwrite(fid, jsonencode(vascao)); 
    fclose(fid)
end

function tf = isoctave()
    % Check if the environment is Octave
    tf = exist('OCTAVE_VERSION', 'builtin') ~= 0;
end

vasca_run();