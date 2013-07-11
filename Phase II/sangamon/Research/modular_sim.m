function [ y ] = modular_sim( net, x )
y = x' * net.w;
u = x' * net.a;
u = exp( u );

temp = sum( u, 2 );
n = size( x, 2 );
g = zeros( n, 2 );
for i=1:n
    g(i,:) = u(i,:) ./ temp(i);
end
t = sum( y .* g, 2 );

y = sum( y.*u, 2 );
y = y ./ sum( u, 2 );
y = y';