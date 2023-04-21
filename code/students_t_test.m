function [tval, pval, RejectOrNot] = students_t_test(sample1, sample2, alpha, OneSideOrTwoSide)
%% Calculate tvalue
mean_difference = sample1.mean - sample2.mean;
variance = sqrt(sample1.sd^2/sample1.size + sample2.sd^2/sample2.size);
df = (sample1.sd^2/sample1.size + sample2.sd^2/sample2.size)^2 ...
    / (sample1.sd^4/sample1.sample_size^2/(sample1.sample_size - 1) ...
    + sample2.sd^4/sample2.sample_size^2/(sample2.sample_size - 1));

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
    if pval <= alpha
        RejectOrNot = "reject";
    else
        RejectOrNot = "accept";
    end
else
    sprintf('You should put either "one-side" or "two-side" into "OneSideOrTwoSide" input.');
end
