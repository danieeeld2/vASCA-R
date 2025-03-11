function run_preprocess2D()
    % Get arguments
    args = argv();

    % Check number of arguments (We need one dataset)
    if numel(args) < 1
        error('Not enough arguments. Usage: preprocess2D_run <dataset>');
    end

    dataset = args{1};  % Get dataset

    % Check that it is a CSV file
    if ~strcmp(dataset(end-3:end), '.csv')
        error('Dataset must be a CSV file');
    end

    % Add path to preprocess2D.m script
    addpath('../matlab');

    % Read dataset from CSV file
    X = csvread(dataset);

    % Call preprocess2D
    [Xcs, av, sc] = preprocess2D(X);

    % Save results to CSV files
    csvwrite('preprocess2D_matlab.csv', Xcs);
    csvwrite('average_matlab.csv', av);
    csvwrite('scale_matlab.csv', sc);
end

run_preprocess2D();