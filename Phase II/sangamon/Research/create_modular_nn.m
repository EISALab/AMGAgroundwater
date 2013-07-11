function net = create_modular_nn( ninputs, experts )
net.w = rand( ninputs, experts );
net.a = rand( ninputs, experts );
net.theta = rand(1, experts).^2;

