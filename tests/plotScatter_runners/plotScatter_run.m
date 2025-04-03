function plotScatter_run(varargin)
    % This script runs the plotScatter function and saves the plot as an image.
    %
    % It takes command-line arguments to specify the input data and optional parameters:
    %
    % Usage: plotScatter_run <X> [Name-Value Pairs]
    %   - <X>: String representation of the X data OR path to a CSV file.
    %   - [Name-Value Pairs]: Optional parameters for plotScatter.
    %
    % Optional Parameters:
    %   - 'EleLabel': [Nx1] name of the elements (numbers are used by default)
    %   - 'ObsClass': [Nx1, str(N), {N}] groups for different visualization (a single group by default)
    %   - 'XYLabel': {2} xlabel and ylabel (nothing by default)
    %   - 'LimCont': {2} control limits on x and y axis (nothing by default)
    %   - 'FilledMarkers': bool, only when 'PlotMult' is 'none'
    %       false: empty marks (by default)
    %       true: filled marks
    %   - 'PlotMult': str
    %       'none': do not plot multiplicity (by default)
    %       'size': plot multiplicity info in the size of the markers.
    %       'shape': plot multiplicity info in the shape of the markers.
    %       'zaxis': plot multiplicity information in the Z axis.
    %       'zsize': plot multiplicity info in the size of the markers and classes in Z-axis
    %   - 'ClassType': str
    %       'Numerical': plot for numerical classes (consistent with a colorbar)
    %       'Categorical': plot for categorical classes (consistent with a legend)
    %   - 'Multiplicity': [Nx1] multiplicity of each row (1s by default)
    %   - 'Markers': [1x3] thresholds for the different multiplicity levels
    %       maxv(1): threshold between very low and low multiplicity (20 by default)
    %       maxv(2): threshold between low and medium multiplicity (50 by default)
    %       maxv(3): threshold between medium and high multiplicity (100 by default)
    %   - 'BlurIndex': [1x1] to avoid blur when adding labels. It reflects the
    %       minimum distance (normalized to [0,1]) where a cluttered label is allowed to be visualized.
    %       By default 0.3 is chosen.
    %   - 'Color': Choose a color for your data.
    %       'hsv' for hsv palette
    %       'parula' for parula palette
    %       'okabeIto' for color blindness (by default for multiple classes)
    %
    % Example usage in MATLAB:
    %   matlab -nodisplay -r "plotScatter_run('[rand(100,1), rand(100,1)]')"
    %   matlab -nodisplay -r "plotScatter_run('[rand(100,1), rand(100,1)]', 'Color', 'r', 'PlotMult', 'size')"
    %
    % Example usage in Octave:
    %   octave --no-gui -q plotScatter_run.m "[rand(100,1), rand(100,1)]"
    %   octave --no-gui -q plotScatter_run.m "[rand(100,1), rand(100,1)]" "Color" "r" "PlotMult" "size"
    %
    % Also, you can parse the data from a CSV file
    %
    % Outputs:
    %   - Saves the plot as 'plotScatter_matlab.png'

    if isoctave()
        args = argv();
    else
        args = varargin;
    end

    if numel(args) < 1
        error('Not enough arguments. Usage: plotScatter_run <X> [Name-Value Pairs]');
    end

    argX = args{1};

    if exist(argX, 'file') == 2
        X = dlmread(argX, ',');
    else
        X = eval(argX);
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
    plotScatter(X, params{:});
    saveas(gcf, 'plotScatter_matlab.png');
    close;
end

function tf = isoctave()
    tf = exist('OCTAVE_VERSION', 'builtin') ~= 0;
end

plotScatter_run()