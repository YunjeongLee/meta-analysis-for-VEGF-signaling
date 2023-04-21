function result = compute_weighted_stats(average, err)
%% Find NaN weight
ind = isnan(err);

%% Remove value having NaN weight
average(ind) = [];
err(ind) = [];

%% The number of samples
result.size = length(average);

%% Define weight
weight = 1./err.^2;
result.weight = weight;

%% Calculate weighted mean
weighted_mean = sum(average .* weight)/sum(weight);

%% Calculate weighted standard error or standard deviation
weighted_sd = sqrt(sum(weight .* (average - weighted_mean).^2)/sum(weight));

result.mean = weighted_mean;
result.sd = weighted_sd;

end