function [tval, pval, RejectOrNot] = students_t_test(sample1, sample2, alpha, OneSideOrTwoSide)
%% Calculate tvalue
mean_difference = sample1.mean - sample2.mean;
if contains(who(), 'cbm')
    % If the sample is CBM, then calculate tval differently
    variance = sqrt(sample1.error^2/sample1.sample_size + sample2.error^2/sample2.sample_size);
    df = (sample1.error^2/sample1.sample_size + sample2.error^2/sample2.sample_size)^2 ...
        / (sample1.error^4/sample1.sample_size^2/(sample1.sample_size - 1) ...
        + sample2.error^4/sample2.sample_size^2/(sample2.sample_size - 1));
else
    variance = sqrt(sample1.error^2 + sample2.error^2);
    df = (sample1.error^2 + sample2.error^2)^2 ...
        / (sample1.error^4/(sample1.sample_size - 1) + sample2.error^4/(sample2.sample_size - 1));
end

% Round down
df = floor(df);

tval = mean_difference/variance;

%% Find critical value
if OneSideOrTwoSide == "one-side"
    pval = tcdf(-tval, df);
    if pval <= alpha
        RejectOrNot = "reject";
    else
        RejectOrNot = "accept";
    end
elseif OneSideOrTwoSide == "two-side"
    pval = 2 * tcdf(-abs(tval), df);
else
    sprintf('You should put either "one-side" or "two-side" into "OneSideOrTwoSide" input.');
end
