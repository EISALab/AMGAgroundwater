%the function returns the mean and std of multiple dimension gaussian.
%x(n,m) matrix. n is the number of samples. m is the dimension of the variable.
%mean(m,1) std(m,m) are in matrix form.
function [mu, var] = multigaussian( x )
[n, m] = size(x);
mu = mean(x);

var = zeros(m,m);
for i=1:n
    var = var + (mu-x(i,:))' * (mu-x(i,:));
end
var = var / (n-1);


