function [theta, cost_val, cal_time] = minimizer(data, cost, params, theta0, lb, ub)
options = optimset();
tic;
if(nargin < 5) || ( isempty(lb) && ( isempty(ub) ) )
    [theta, ~] = fminsearch(@(theta) cost( data, params, theta ), theta0, options );
else
    [theta, ~] = fminsearchbnd(@(theta) cost( data, params, theta ), theta0, lb, ub, options );
end
cost_val = cost( data, params, theta );
cal_time = toc;