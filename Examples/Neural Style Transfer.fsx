﻿(*
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
let pool1 = Operators.Pooling(relu1_2, pad = [0;0], kernel = [2;2]; stride = [2;2], poolType = Avg, Name = "pool1")
pool1 = mx.symbol.Pooling(name='pool1', data=relu1_2 , pad=(0,0), kernel=(2,2), stride=(2,2), pool_type='avg')
conv2_1 = mx.symbol.Convolution(name='conv2_1', data=pool1 , num_filter=128, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu2_1 = mx.symbol.Activation(name='relu2_1', data=conv2_1 , act_type='relu')
conv2_2 = mx.symbol.Convolution(name='conv2_2', data=relu2_1 , num_filter=128, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu2_2 = mx.symbol.Activation(name='relu2_2', data=conv2_2 , act_type='relu')
pool2 = mx.symbol.Pooling(name='pool2', data=relu2_2 , pad=(0,0), kernel=(2,2), stride=(2,2), pool_type='avg')
conv3_1 = mx.symbol.Convolution(name='conv3_1', data=pool2 , num_filter=256, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu3_1 = mx.symbol.Activation(name='relu3_1', data=conv3_1 , act_type='relu')
conv3_2 = mx.symbol.Convolution(name='conv3_2', data=relu3_1 , num_filter=256, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu3_2 = mx.symbol.Activation(name='relu3_2', data=conv3_2 , act_type='relu')
conv3_3 = mx.symbol.Convolution(name='conv3_3', data=relu3_2 , num_filter=256, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu3_3 = mx.symbol.Activation(name='relu3_3', data=conv3_3 , act_type='relu')
conv3_4 = mx.symbol.Convolution(name='conv3_4', data=relu3_3 , num_filter=256, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu3_4 = mx.symbol.Activation(name='relu3_4', data=conv3_4 , act_type='relu')
pool3 = mx.symbol.Pooling(name='pool3', data=relu3_4 , pad=(0,0), kernel=(2,2), stride=(2,2), pool_type='avg')
conv4_1 = mx.symbol.Convolution(name='conv4_1', data=pool3 , num_filter=512, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu4_1 = mx.symbol.Activation(name='relu4_1', data=conv4_1 , act_type='relu')
conv4_2 = mx.symbol.Convolution(name='conv4_2', data=relu4_1 , num_filter=512, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu4_2 = mx.symbol.Activation(name='relu4_2', data=conv4_2 , act_type='relu')
conv4_3 = mx.symbol.Convolution(name='conv4_3', data=relu4_2 , num_filter=512, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu4_3 = mx.symbol.Activation(name='relu4_3', data=conv4_3 , act_type='relu')
conv4_4 = mx.symbol.Convolution(name='conv4_4', data=relu4_3 , num_filter=512, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu4_4 = mx.symbol.Activation(name='relu4_4', data=conv4_4 , act_type='relu')
pool4 = mx.symbol.Pooling(name='pool4', data=relu4_4 , pad=(0,0), kernel=(2,2), stride=(2,2), pool_type='avg')
conv5_1 = mx.symbol.Convolution(name='conv5_1', data=pool4 , num_filter=512, pad=(1,1), kernel=(3,3), stride=(1,1), no_bias=False, workspace=1024)
relu5_1 = mx.symbol.Activation(name='relu5_1', data=conv5_1 , act_type='relu')

# style and content layers
style = mx.sym.Group([relu1_1, relu2_1, relu3_1, relu4_1, relu5_1])
content = mx.sym.Group([relu4_2])
return style, content
