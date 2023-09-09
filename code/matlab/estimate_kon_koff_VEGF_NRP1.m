clear; close all; clc;

%% Data list
data = readtable("../../data/VEGFA165_NRP1_02.3-39nm_Herve2008.xlsx", ...
    "ReadVariableNames",true,'Range','A1:J52');
for i = 1:(size(data, 2)/2)
    data_set = table2array(data(:, (2*i-1):(2*i)));
    cmd_string = sprintf('data_vcon%d = data_set(isfinite(data_set(:, 1)), :);', i);
    eval(cmd_string);
end

%% Chop data
[vcon1_ass, vcon1_diss] = split_data(data_vcon1);
[vcon2_ass, vcon2_diss] = split_data(data_vcon2);
[vcon3_ass, vcon3_diss] = split_data(data_vcon3);
[vcon4_ass, vcon4_diss] = split_data(data_vcon4);
[vcon5_ass, vcon5_diss] = split_data(data_vcon5);

%% Fit data
data_set_list = {vcon1_diss, vcon2_diss, vcon3_diss, vcon4_diss, vcon5_diss};
conc = [2.3, 4.6, 9.2, 19.5, 39]*1e-9;
kd_est = zeros(length(data_set_list), 1);
for i = 1:length(data_set_list)
    data_frame = data_set_list{i};
    % Define params
    params = {...
        'R0', max(data_frame(:, 2)), false; ...
        'conc', conc(i), false;
        'ka', 1e6, false; ...
        'kd', 1e-4, true; ...
        };
    [kd_est(i), cost, ~] = minimizer(data_frame, @cost_diss, params, 1e-3, 0, inf);
end

%% Plot
colors = turbo(5);
legends = {};
figure;
hold on;
for i = 1:length(data_set_list)
    data_frame = data_set_list{i};
    % Define params
    params = {...
        'R0', max(data_frame(:, 2)), false; ...
        'conc', conc(i), false;
        'ka', 1e6, false; ...
        'kd', kd_est(i), true; ...
        };
    ydata = params{1, 2} * exp(- kd_est(i) * data_frame(:, 1));
    plot(data_frame(:, 1), data_frame(:, 2), '.', 'markersize', 20, 'color', colors(i, :));
    plot(data_frame(:, 1), ydata, 'color', colors(i, :));
    legends{end+1} = sprintf('Data (%.1f nM)', conc(i)*1e9);
    legends{end+1} = sprintf('Fitted (%.1f nM)', conc(i)*1e9);
end
hold off;
legend(legends, 'Location','bestoutside')

%% Fit
data_set_list = {vcon1_ass, vcon2_ass, vcon3_ass, vcon4_ass, vcon5_ass};
conc = [2.3, 4.6, 9.2, 19.5, 39]*1e-9;
ka_est = zeros(length(data_set_list), 1);
for i = 1:length(data_set_list)
    data_frame = data_set_list{i};
    % Define params
    params = {...
        'R0', max(data_frame(:, 2)), false; ...
        'conc', conc(i), false;
        'ka', 1e6, true; ...
        'kd', kd_est(i), false; ...
        };
    [ka_est(i), cost, ~] = minimizer(data_frame, @cost_ass, params, 1e6, 0, inf);
end

%% Plot
legends = {};
figure;
hold on;
for i = 1:length(data_set_list)
    data_frame = data_set_list{i};
    % Define params
    params = {...
        'R0', max(data_frame(:, 2)), false; ...
        'conc', conc(i), false;
        'ka', ka_est(i), true; ...
        'kd', kd_est(i), true; ...
        };
    ydata = params{1, 2} * conc(i)/(kd_est(i)/ka_est(i) + params{2,2}) ...
        * (1 - exp((- ka_est(i) * conc(i) + kd_est(i))*data_frame(:, 1)));
    plot(data_frame(:, 1), data_frame(:, 2), '.', 'markersize', 20, 'color', colors(i, :));
    plot(data_frame(:, 1), ydata, 'color', colors(i, :));
    legends{end+1} = sprintf('Data (%.1f nM)', conc(i)*1e9);
    legends{end+1} = sprintf('Fitted (%.1f nM)', conc(i)*1e9);
end
hold off;
legend(legends, 'Location','bestoutside')

