function result = compute_weighted_stats(average, err)
%% Find NaN weight
ind = isnan(err);

%% Remove value having NaN weight
average(ind) = [];
err(ind) = [];

%% The number of samples
result.sample_size = length(average);

%% Define weight
weight = 1./err.^2;

%% Calculate weighted mean
weighted_mean = sum(average .* weight)/sum(weight);

%% Calculate weighted standard error or standard deviation
weighted_error = sqrt(sum(weight .* (average - weighted_mean).^2)/sum(weight));

result.mean = weighted_mean;
end