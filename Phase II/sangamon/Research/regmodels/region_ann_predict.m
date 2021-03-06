%data must be (m,n) matrix.
%each column is a input vector or a (input+output) vector. There are n vectors.
%If the vectors contain outputs, the function computes mse.
function [predict, mse] = region_predict( regnn, data )
n_inputs = size( regnn(1).center, 2 );
count = size(data, 2);

%check if the data has test output in it
calc_mse = false;
if( size(data,1)>n_inputs )
    calc_mse = true;
end

%the input data matrix
data_in = data(1:n_inputs, :);

%compute output for each input vector.
for i=1:count
    out = do_predict( regnn, data_in(:,i) );
    if( i==1 )
        predict = out;
    else
        predict = [ predict out ];
    end
end

%if needs to compute mse, do it.
if( calc_mse )
    n_outputs = size(predict,1);
    true_out = data( n_inputs+1:n_inputs+n_outputs, : )
    mse = sum( (predict-true_out).^2, 1 );
    mse = sum( mse ) / n_outputs / count;
end
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [out] = do_predict( regnn, x )
nn_count = size(regnn, 1);

p = zeros(nn_count,1);
for i=1:nn_count
    %note, calc_trustprob requires x be a row vector.
    p(i) = calc_trustprob( regnn(i), x' );
%    p(i) = calc_trustprob_gaussian( regnn(i), x' );
    if (i==1)
        y = sim( regnn(i).net, x );
    else
        y = [y sim( regnn(i).net, x )];
    end
end

%disp( p' );

%the first ann must do prediction, since it is the global ANN
if( p(1)<=0 )
    p(1) = 1e-3;
end

if( size(p, 1)>1 )
    for i=2:size(p,1)
        if( p(i)>0 )
            p(i) = 1.0*p(i);
        end
    end
end

%if( size(p,1)>1 & p(2)>0 )
%    p(2) = 5*p(2);
%end

%if( size(p,1)>2 & p(3)>0 )
%    p(3) = 5*p(3);
%end

%p
%p = p>=(max(p));

%compute the weighted average of the outputs.
out = zeros( size(y,1), 1 );
p_sum = sum(p);
for i=1:nn_count
    out = out + y(:,i)*p(i)/p_sum;
end

