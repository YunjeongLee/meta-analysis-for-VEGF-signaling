function [ass, diss] = split_data(data)

% Remove t<0 data
data = data(data(:, 1)>=0, :);

% Find the time when Rmax reaches
[~, max_idx] = max(data(:, 2));

% Split data
ass = data(1:max_idx, :);
diss = data(max_idx:end, :);
diss(:, 1) = diss(:, 1) - diss(1, 1);
end
