function J = cost_diss(data, params, theta)
%% Assign Parameter
names = params(:,1);
isEstimated = cell2mat(params(:,3));
assert(sum(isEstimated) == length(theta));
params(isEstimated, 2) = num2cell(theta);
for k = 1:size(params,1)
    cmd_string = sprintf('%s_ = params{%d,2};', names{k}, k);
    eval(cmd_string);
end

%% Data
time_stamp = data(:,1);
measurements = data(:,2);

%% Model
predict = R0_ * exp(-kd_ * time_stamp);

J = sum( (measurements - predict).^2 );

end