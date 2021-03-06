%cluster_mx is a matrix indicating the clusters to which a point belongs 
%cluster_mx(n_points, n_networks) eg. for a point of (1 1 0) the point
%belong to the first and second cluster.
%function [gate_w] = region_train_gate( regnn, train_in, train_out, test_in, test_out )
function [gate_w] = region_svm_train_gate( regnn, train_in, train_out, train_cluster, test_in, test_out, test_cluster )
n_inputs = size( train_in, 1 );
n_points = size( train_in, 2 );
n_outputs = size( train_out, 1 );
n_networks = size( regnn, 1 );

%alpha is learning rate, beta is momentum correction rate
alpha = 0.02;
beta = 0.1;

% a is the weight matrix of the gating network
a = rand( n_inputs, n_networks );

y_net = [];
y_net_temp = [];
for i=1:n_networks
    predict = svmpredict( train_out', train_in', regnn(i).net );
    y_net = [y_net; predict' ];
    predict = svmpredict( test_out', test_in', regnn(i).net );
    y_net_temp = [y_net_temp; predict' ];
end

epoch = 3700;
mse = [];
a_save = a;
a_deta = zeros( size(a) );      %a_deta is the momentum. initalized 0.
mse_save = 1e10;
for i=1:epoch
    if( mod(i, 600)==0 )
        a = rand(n_inputs, n_networks);
        a_deta = zeros( size(a) );
    end
    u = exp( a' * train_in ) .* train_cluster';
    g = u  ./ ( ones(n_networks, 1)*sum(u, 1) );
    y = sum( g.*y_net, 1 );
    
    %compute training error
    mse_train = sum( (y-train_out).^2 )/size(train_out,2);
    
    %compute testing mse error
    u_temp = exp( a' * test_in ) .* test_cluster';
    g_temp = u_temp ./ ( ones(n_networks, 1)*sum(u_temp, 1) );
    y_temp = sum( g_temp .* y_net_temp, 1 );
    mse_test =  sum( (y_temp-test_out).^2 )/size(test_out,2);
    mse = [mse; mse_train mse_test];
    if( mse_save > mse_test )
        a_save = a;
        mse_save = mse_test;
    end
    
    e = train_out - y;

    % deta1 = 2*e*g(k)*( y - y(k) )* x
    coef1 = 2*( ones(n_networks,1)*e ) .* g .* ( (ones(n_networks,1)*y) - y_net ); %.* cluster_mx' ;
    %coef1 = 2*( ones(n_networks,1)*e ) .* ( (ones(n_networks,1)*y) - y_net ); % .* cluster_mx' ;
    % deta2 = 2*g(k)^2(1-g(k)) * x
    %coef2 = 2 .* (g.^2) .* (1-g) .* (1-cluster_mx');
    %coef2 = g .* (1-cluster_mx');
    a_new = a - alpha * train_in * coef1' + beta*a_deta;
    %a_new = a - alpha * train_in * (coef1'+coef2') + beta*a_deta;
%    a_new = a - alpha * train_in * (coef2') + beta*a_deta;
    
    %save the momentum;
    a_deta = a_new - a;
    a = a_new;
end

figure(3);
plot( [1:epoch]', mse(:,1), 'b-', [1:epoch], mse(:,2), 'r:' );
mse_save
gate_w = a_save;




