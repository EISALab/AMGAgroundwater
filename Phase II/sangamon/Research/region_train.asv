%train_in are input data set [m_in, n];
%train_out are output data set [m_out, n];
%m_in is the number of inputs. m_out is the number of outputs.
function [regnn] = region_train( nodes, train_in, train_out, center, minpts, minaxpn, test_in, test_out )
n_inputs = size(train_in, 1);
n_outputs = size( train_out, 1);
n_count = size(train_in, 2);

n_centers = size(center, 2);
for i=1:n_centers
    %create data set pool and compute distance to the center.
    data = [train_in; train_out];
    d = distance( train_in, center(:,i) );

    %sort the points by distance.
    temp_train = sortrows( [d; data]', 1 );
    temp_train = temp_train';
    
    data = [test_in; test_out];
    d = distance( test_in, center(:,i) );
    temp_test = sortrows( [d; data]', 1 );
    temp_test = temp_test';
    
    if( i==1 )
        regnn = recursive_train( nodes, temp_train(2:n_inputs+1, :), temp_train(n_inputs+2:n_inputs+n_outputs+1, :), minpts, minaxpn, temp_test(2:n_inputs+1,:), temp_test(n_inputs+2:n_inputs+n_outputs+1,:) );
    else
        regnew = recursive_train( nodes, temp_train(2:n_inputs+1, :), temp_train(n_inputs+2:n_inputs+n_outputs+1, :), minpts+20, minaxpn, temp_test(2:n_inputs+1,:), temp_test(n_inputs+2:n_inputs+n_outputs+1,:) );
        regnn = [regnn; regnew(2)];
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               sub functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [regnn] = recursive_train( nodes, train_in, train_out, minpts, minaxpn, test_in, test_out )
count = size(train_in, 2);

reg = calc_region( train_in' );

%train a neural network at this region.
funcs = { 'tansig', 'tansig' };
net = newff( minaxpn, nodes, funcs );

vv.P = test_in;
vv.T = test_out;

net.trainParam.show = 50;
net.trainParam.lr = 0.05;
net.trainParam.mc = 0.9;
net.trainParam.epochs = 100;
net.trainParam.goal = 1e-5;

minmse = 1e10;
savenet = net;
for i=1:5
%    [net, tr] = train( net, train_in, train_out );
    [net, tr] = train( net, train_in, train_out, [], [], vv );
    mse = testnn( net, train_in, train_out );
    if( minmse>mse )
        minmse = mse;
        savenet = net;
    end
    net = init(net);
end
reg.net = savenet;

if( count>minpts + 20 )
    regs = recursive_train( nodes, train_in(:, 1:count-minpts), train_out(:, 1:count-minpts), minpts, minaxpn, test_in(:,1:count-minpts), test_out(:, 1:count-minpts) );
    regnn = [reg; regs];
else
    regnn = [reg];
end

function [mse]=testnn( net, p, t )
tp = sim( net, p );
%calculate the MSE
sz = size( tp );
mse = sum( (t-tp).^2, 1 );
mse = sum( mse ) / sz(1) / sz(2);

