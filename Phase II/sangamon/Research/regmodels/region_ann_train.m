%train_in are input data set [m_in, n];
%train_out are output data set [m_out, n];
%m_in is the number of inputs. m_out is the number of outputs.
function [regnn] = region_ann_train( nodes, train_idx, train_in, train_out, minaxpn, test_idx, test_in, test_out )
n_inputs = size(train_in, 1);
n_outputs = size( train_out, 1);
n_count = size(train_in, 2);

n_networks = size( train_idx, 2 );
temp_train_in = [];
temp_train_out = [];
temp_test_in = [];
temp_test_out = [];
regnn = [];

save_train_in = [];
save_train_out = [];
save_test_in = [];
save_test_out = [];
for i=1:n_networks

%{    
    if( i>1 )
        temp_train_in = save_train_in;
        temp_train_out = save_train_out;
        temp_test_in = save_test_in;
        temp_test_out = save_test_out;
    end
%}
    if (i>1)
%        nodes = [2,1];
    end

    idx = train_idx{1, i};
    n = size(idx,1);
    for j=1:n
        temp_train_in = [temp_train_in train_in(:, idx(j))];
        temp_train_out = [temp_train_out train_out(:, idx(j))];
    end
    idx = test_idx{1, i};
    n = size(idx, 1);
    for j=1:n
        temp_test_in = [temp_test_in test_in(:, idx(j))];
        temp_test_out = [temp_test_out test_out(:, idx(j))];
    end
   
    reg = reg_train( nodes, temp_train_in, temp_train_out, minaxpn, temp_test_in, temp_test_out );
    regnn = [regnn; reg];

    if( i==1 )
        save_train_in = temp_train_in;
        save_train_out = temp_train_out;
        save_test_in = temp_test_in;
        save_test_out = temp_test_out;
    end
        
    temp_train_in = [];
    temp_train_out = [];
    temp_test_in = [];
    temp_test_out = [];

end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               sub functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [reg] = reg_train( nodes, train_in, train_out, minaxpn, test_in, test_out )
count = size(train_in, 2);

reg = calc_region( train_in' );

%train a neural network at this region.
funcs = { 'tansig', 'tansig' };
net = newff( minaxpn, nodes, funcs );
%net = newff( minaxpn, nodes, funcs, 'trainbr' );

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

