function plotVec_run(varargin)
    % This script runs the plotVec function and saves the plot as an image.
    %
    % It takes command-line arguments to specify the input data and optional parameters:
    %
    % Usage: plotVec_run <vec> [Name-Value Pairs]
    %   - <vec>: String representation of the vector data OR path to a CSV file.
    %   - [Name-Value Pairs]: Optional parameters for plotVec.
    %
    % Optional Parameters:
    %   - 'EleLabel': [Nx1] name of the vector elements (numbers are used by default)
    %   - 'ObsClass': [Nx1, str(N), {N}] groups for different visualization (a single group by default)
    %   - 'XYLabel': {2} xlabel and ylabel (nothing by default)
    %   - 'LimCont': [NxL or Lx1] L control limits (nothing by default)
    %   - 'PlotType': str
    %       'Lines': line plot
    %       'Bars': bar plot (by default)
    %   - 'ClassType': str
    %       'Numerical': plot for numerical classes (consistent with a colorbar)
    %       'Categorical': plot for categorical classes (consistent with a legend)
    %   - 'VecLabel': [Mx1] name of the vectors (numbers are used by default)
    %   - 'Multiplicity': [Nx1] multiplicity of each row (1s by default)
    %   - 'Markers': [1x3] thresholds for the different marker sizes (20, 50 and 100 by default)
    %   - 'Color': Choose a color for your data.
    %       'hsv' for hsv palette
    %       'parula' for parula palette
    %       'okabeIto' for color blindness (by default for multiple classes)
    %
    % Example usage in MATLAB:
    %   matlab -nodisplay -r "plotVec_run('randn(100,3)')"
    %   matlab -nodisplay -r "plotVec_run('randn(100,3)', 'XYLabel', {'Functions','Time'}, 'LimCont', [1, -1, 3], 'Color', 'parula')"
    %
    % Example usage in Octave:
    %   octave --no-gui -q plotVec_run.m "randn(100,3)"
    %   octave --no-gui -q plotVec_run.m "randn(100,3)" "XYLabel" "{'Functions','Time'}" "LimCont" "[1, -1, 3]" "Color" "parula"
    %
    % Also, you can parse the data from a CSV file
    %
    % Outputs:
    %   - Saves the plot as 'plotVec_matlab.png'

    if isoctave()
        args = argv();
    else
        args = varargin;
    end

    if numel(args) < 1
        error('Not enough arguments. Usage: plotVec_run <vec> [Name-Value Pairs]');
    end

    argVec = args{1};

    if exist(argVec, 'file') == 2
        vec = dlmread(argVec, ',');
    else
        vec = eval(argVec);
    end

    % Initialize optional parameters
    params = {};

    % Process optional parameters if provided
    if length(args) > 1
        i = 2;
        while i <= length(args)
            paramName = args{i};
            if i + 1 <= length(args)
                paramValue = args{i + 1};
                % Try to evaluate expressions instead of just converting numbers
                try
                    paramValue = eval(paramValue);
                catch
                    if strcmpi(paramValue, 'true')
                        paramValue = true;
                    elseif strcmpi(paramValue, 'false')
                        paramValue = false;
                    end
                end
                params = [params, paramName, paramValue];
                i = i + 2;
            else
                error('Parameter %s has no assigned value.', paramName);
            end
        end
    end

    addpath('../matlab');

    figure;
    plotVec(vec, params{:});
    saveas(gcf, 'plotVec_matlab.png');
    close;
end

function tf = isoctave()
    tf = exist('OCTAVE_VERSION', 'builtin') ~= 0;
end

plotVec_run()