## Examples
### [Temporal Convolutional Networks (TCN)](TCN)
F# implementation of [An Empirical Evaluation of Generic Convolutional and Recurrent Networks for Sequence Modeling](https://arxiv.org/abs/1803.01271) by Shaojie Bai, J. Zico Kolter and Vladlen Koltun.
* Adding Problem [(adding.fsx)](TCN/adding.fsx)
* Sequntial MNIST [(MNIST.fsx)](TCN/MNIST.fsx)
### [DQN](Examples/DQN)
F# deep reinforcement learning example. Simple DQN trained on a toy game using epsilon-greedy based decisions with memory replay. The task is simple, player (blue) can move left/right to collect rewards (green) and avoid obstacles (red). Once 10 (default setting) green rewards are missed the game resets.
### [GAN](Examples/GAN)
#### [CGAN](GAN/CGAN.fsx) 
Conditional Generative Adversarial Nets (https://arxiv.org/abs/1411.1784) Mehdi Mirza, Simon Osindero. Adapted from https://github.com/eriklindernoren/Keras-GAN/tree/master/cgan
### [Variational Autoencoders](VAE)
#### [MNIST](VAE/MNIST%20VAE.fsx)
Basic VAE trained on MNIST dataset.
### [Style transfer](Style)
#### [Basic neural style transfer](Style/Neural%20Style%20Transfer.fsx)
Neural Style Transfer example ported from https://github.com/apache/incubator-mxnet/tree/225f71f744ac5e7bd29868b6d3ba0e4fe2527c43/example/neural-style
