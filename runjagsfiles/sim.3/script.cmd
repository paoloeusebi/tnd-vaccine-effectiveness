model in "model.txt"
data in "data.txt"
compile, nchains(1)
parameters in "inits3.txt", chain(1)
initialize
adapt 1000
update 2000
monitor Se, thin(1)
monitor Sp, thin(1)
monitor pi, thin(1)
monitor OR, thin(1)
update 10000
parameters to "out3.Rdump", chain(1)
coda *, stem(sim.3/CODA)
samplers to sim.3/samplers.csv
update 0
model clear
exit
