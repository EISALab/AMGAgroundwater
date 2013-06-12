%x is a vector matrix (n, pts);
%y is a vector (n)
%return distance array from each point to y.
function [d] = mydist( x, y )
n = size(x, 2);
d = zeros(1,n);
for i=1:n
    d(i) = sum( (x(:,i)-y).^2 );
end
d = sqrt(d);