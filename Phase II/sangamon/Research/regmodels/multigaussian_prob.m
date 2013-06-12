%compute the probability of multiple dimension gaussian
%mu(1,m) the mean. var(m,m). The variance. x(k,m) - k the number of variables. m the dimension
function [p] = multigaussian_prob( mu, var, x )
k = size(x,1);
xus = x - ones(k,1)*mu;
p = 1 / sqrt( det(2*pi*var) );
p = p * exp( sum( -1/2 * xus*var^-1 .* xus, 2 ) );