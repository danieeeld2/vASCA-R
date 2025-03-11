function run_preprocess2D(dataset)
    % Read dataset from CSV file
    X = csvread(dataset);

    % Call preprocess2D
    [Xcs, av, sc] = preprocess2D(X);

    % Save results to CSV files
    csvwrite('preprocess2D_matlab.csv', Xcs);
    csvwrite('average_matlab.csv', av);
    csvwrite('scale_matlab.csv', sc);
end