using DataFrames
using Distributions

function lessThanQ(x,q)
	if x<=q
		return 1
	else
		return 0
	end
end

function LorenzOrd(inc, ordinate)
	w = [1.0 for i=1:length(inc)]
	N = length(inc)
	xi_p = quantile(inc, ordinate)
	N_hat = sum(w)
	mu_hat = mean(inc)
	I_vec = [lessThanQ(i, xi_p) for i=inc]
	L_hat=(1/(N_hat.*mu_hat))*sum(w.*inc.*I_vec)
	return L_hat
end
		
	
df  = readtable("/home/john/CPS2000.csv")
MI = df[:(State.=="Michigan"),:]["cellmean_equivinc"]
CA = df[:(State.=="California"),:]["cellmean_equivinc"]


bootreps = [0.0 for i=1:500, j=1:10]
j_indx=0
@time for j=linspace(0.1, 1, 10)
	j_indx+=1
	for i=1:500
		bootreps[i,j_indx] = LorenzOrd(sample(MI, length(MI)), j) - LorenzOrd(sample(CA, length(CA)), j)
	end
end

