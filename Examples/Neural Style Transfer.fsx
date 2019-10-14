(*
# Neural Style Transfer
Ported from https://github.com/apache/incubator-mxnet/tree/225f71f744ac5e7bd29868b6d3ba0e4fe2527c43/example/neural-style
*)

#load "load.fsx"
open MXNetSharp


let data = Symbol.Variable "data"
let conv1_1 = Operators.Convolution(data, Symbol.Empty, Symbol.Empty, numFilter = 64, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv1_1")
let relu1_1 = Operators.Activation(conv1_1, actType = Relu, Name = "relu1_1")
let conv1_2 = Operators.Convolution(relu1_1, Symbol.Empty, Symbol.Empty, numFilter = 64, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv1_2")
let relu1_2 = Operators.Activation(conv1_2, actType = Relu, Name = "relu1_2")
let pool1 = Operators.Pooling(relu1_2, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = Avg, Name = "pool1")

let conv2_1 = Operators.Convolution(pool1, Symbol.Empty, Symbol.Empty, numFilter = 128, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv2_1")
let relu2_1 = Operators.Activation(conv2_1, actType = Relu, Name = "relu2_1")
let conv2_2 = Operators.Convolution(relu2_1, Symbol.Empty, Symbol.Empty, numFilter = 128, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv2_2")
let relu2_2 = Operators.Activation(conv2_2, actType = Relu, Name = "relu2_2")
let pool2 = Operators.Pooling(relu2_2, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = Avg, Name = "pool2")

let conv3_1 = Operators.Convolution(pool2, Symbol.Empty, Symbol.Empty, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_1")
let relu3_1 = Operators.Activation(conv3_1, actType = Relu, Name = "relu3_1")
let conv3_2 = Operators.Convolution(relu3_1, Symbol.Empty, Symbol.Empty, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_2")
let relu3_2 = Operators.Activation(conv3_2, actType = Relu, Name = "relu3_2")
let conv3_3 = Operators.Convolution(relu3_2, Symbol.Empty, Symbol.Empty, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_3")
let relu3_3 = Operators.Activation(conv3_3, actType = Relu, Name = "relu3_3")
let conv3_4 = Operators.Convolution(relu3_3, Symbol.Empty, Symbol.Empty, numFilter = 256, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv3_4")
let relu3_4 = Operators.Activation(conv3_4, actType = Relu, Name = "relu3_4")
let pool3 = Operators.Pooling(relu3_4, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = Avg, Name = "pool3")

let conv4_1 = Operators.Convolution(pool3, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_1")
let relu4_1 = Operators.Activation(conv4_1, actType = Relu, Name = "relu4_1")
let conv4_2 = Operators.Convolution(relu4_1, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_2")
let relu4_2 = Operators.Activation(conv4_2, actType = Relu, Name = "relu4_2")
let conv4_3 = Operators.Convolution(relu4_2, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_3")
let relu4_3 = Operators.Activation(conv4_3, actType = Relu, Name = "relu4_3")
let conv4_4 = Operators.Convolution(relu4_3, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv4_4")
let relu4_4 = Operators.Activation(conv4_4, actType = Relu, Name = "relu4_4")
let pool4 = Operators.Pooling(relu4_4, pad = [0;0], kernel = [2;2], stride = [2;2], poolType = Avg, Name = "pool4")

let conv5_1 = Operators.Convolution(pool4, Symbol.Empty, Symbol.Empty, numFilter = 512, pad = [1;1], kernel = [3;3], stride = [1;1], noBias = false, workspace = 1024L, Name = "conv5_1")
let relu5_1 = Operators.Activation(conv5_1, actType = Relu, Name = "relu5_1")


let style = Symbol.Group([relu1_1, relu2_1, relu3_1, relu4_1, relu5_1])
content = mx.sym.Group([relu4_2])
return style, content
