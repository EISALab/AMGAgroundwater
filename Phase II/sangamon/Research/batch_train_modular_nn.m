function [net] = batch_train_modular_nn( net, x, d, yeta )

% n - number of training examples.
% k - number of experts.
n = size( x, 2 );
k = size( net.w, 2 );

%u(k) = x' * a(k)      u is the gating network's outputs.
u = x' * net.a;

%g(k) = exp( u(k) )/sum[ exp( u(k) ) ]  1..k of experts.
u = exp( u );
g = u ./ ( sum(u, 2)*ones(1,k) );

%y(k) = x' * w(k)   1..k of experts.
y = x'*net.w;

% e(k) = d - y(k)   1..k of experts.
e = d'*ones(1,k) - y;
    
% h(k) = g(k)/theta(k)*exp( -1/2/theta(k)^2 * [d(k) - y(k)]^2 ) / sum(% ... )
theta = ones(n,1) * net.theta;
h = g./ theta .* exp( -e.^2 / 2 ./ (theta.^2 ) );
h = h ./ ( sum(h,2)*ones(1,k) );

%update w   w(k) = w(k) + yeta * h(k)/theta(k)^2 * e(k) * x;
coef = yeta * ( h ./ (theta.^2) .* e );
net.w  = net.w  + x * coef;
    
%updat a    a(k) = a(k) + yeta * [h(k) - g(k)] * x;
coef = yeta * (h - g );
net.a = net.a + x * coef;

%update theta   theta(k)^2 = theta(k)^2 + yeta* h(k)/2/theta(k)^4 * ( e(k)^2 - theta(k)^2 )
coef = yeta* ( h / 2 ./ (theta.^4) );
theta2 = theta.^2;
theta2 = theta2(1,:) + sum( coef .* ( e.^2 - theta2 ) );
for j=1:k
    if( theta2(j)<=0 )
        theta2(j)=0.0001;
    end
end
net.theta = theta2.^(0.5);
