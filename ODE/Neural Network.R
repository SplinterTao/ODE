library(neuralnet)

nn <- neuralnet(identify ~., stepmax=10000, hidden=c(50,30,10,5),linear.output=FALSE, threshold=0.01,data=combined_data)
nn$result.matrix
plot(nn)

neuralnet(identify ~., stepmax=10000, hidden=c(50,30,10,5),linear.output=FALSE, threshold=0.01,data=combined_data)
