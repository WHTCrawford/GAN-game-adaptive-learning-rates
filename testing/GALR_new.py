import tensorflow as tf
import numpy as np
import pandas as pd
import os
import seaborn as sns
import matplotlib.pyplot as plt
import time
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'


# Structure: loop through a big sample by taking minibatches, do this for a number of epochs . Do all of this for
# however many trials,randomising the learning rate parameters each time
number_of_trails = 200
number_of_epochs = 100
mini_batch_size = 10
sample_size = 2000
learn_steps = (number_of_epochs*sample_size) / mini_batch_size
hidden_layer_size_d = 6
hidden_layer_size_g = 5

# define actual distribution
real_mean = 6
real_sd = 1


# discriminator and generator NNs
def discriminator(input, parameters):
    pre_0 = tf.to_float(input)
    activ_1 = tf.add(tf.matmul(pre_0, parameters[0]), parameters[1])
    pre_1 = tf.tanh(activ_1)
    activ_2 = tf.add(tf.matmul(pre_1, parameters[2]), parameters[3])
    pre_2 = tf.tanh(activ_2)
    activ_3 = tf.add(tf.matmul(pre_2, parameters[4]), parameters[5])
    output = tf.sigmoid(activ_3)
    return output


def generator(input, parameters):
    pre_0 = tf.to_float(input)
    activ_1 = tf.add(tf.matmul(pre_0, parameters[0]), parameters[1])
    pre_1 = tf.tanh(activ_1)
    output = tf.add(tf.matmul(pre_1, parameters[2]), parameters[3])
    return output


# Create weights and biases variables
weight_d_1 = tf.Variable(tf.random_uniform([1, hidden_layer_size_d], minval=0, maxval=1, dtype=tf.float32))
bias_d_1 = tf.Variable(tf.random_uniform([hidden_layer_size_d], minval=0, maxval=1, dtype=tf.float32))
weight_d_2 = tf.Variable(tf.random_uniform([hidden_layer_size_d, hidden_layer_size_d], minval=0, maxval=1, dtype=tf.float32))
bias_d_2 = tf.Variable(tf.random_uniform([hidden_layer_size_d], minval=0, maxval=1, dtype=tf.float32))
weight_d_3 = tf.Variable(tf.random_uniform([hidden_layer_size_d, 1], minval=0, maxval=1, dtype=tf.float32))
bias_d_3 = tf.Variable(tf.random_uniform([1], minval=0, maxval=1, dtype=tf.float32))

d_parameters = [weight_d_1,bias_d_1, weight_d_2, bias_d_2,weight_d_3, bias_d_3]

weight_g_1 = tf.Variable(tf.random_uniform([1, hidden_layer_size_g], minval=0, maxval=1, dtype=tf.float32))
bias_g_1 = tf.Variable(tf.random_uniform([hidden_layer_size_g], minval=0, maxval=1, dtype=tf.float32))
weight_g_2 = tf.Variable(tf.random_uniform([hidden_layer_size_g, 1], minval=0, maxval=1, dtype=tf.float32))
bias_g_2 = tf.Variable(tf.random_uniform([1], minval=0, maxval=1, dtype=tf.float32))



g_parameters = [weight_g_1,bias_g_1, weight_g_2, bias_g_2]


# losses
x = tf.placeholder(tf.float32, shape=(None, 1))
z = tf.placeholder(tf.float32, shape=(None, 1))
with tf.variable_scope("Discrim") as scope:
    D1 = discriminator(x, d_parameters)
    scope.reuse_variables()
    D2 = discriminator(generator(z, g_parameters), d_parameters)
loss_d = tf.reduce_mean(-tf.log(D1) - tf.log(1 - D2))
loss_g = tf.reduce_mean(-tf.log(D2))

# Game Adaptive Learning Rate
phi_g = tf.placeholder(tf.float32)
phi_d = tf.placeholder(tf.float32)
gamma = tf.placeholder(tf.float32)
adjuster1 = (1/2 * tf.log(phi_d/(2*phi_g +phi_d))) / tf.log(0.25)
adjuster = tf.maximum(0.0, adjuster1)

V = tf.minimum(tf.reduce_mean(tf.log(D1)+tf.log(1-D2)),0)
V1 = tf.where(tf.is_nan(V), tf.zeros_like(V), V)

learning_rate_d = gamma-phi_d*tf.tanh(adjuster*V1)
learning_rate_g = gamma + phi_g*(1 + tf.tanh(adjuster*V1))

# Train step

# train_g = tf.train.AdamOptimizer(learning_rate_g).minimize(loss_g, var_list=g_parameters)
# train_d = tf.train.AdamOptimizer(learning_rate_d).minimize(loss_d, var_list=d_parameters)

# train_g = tf.train.GradientDescentOptimizer(learning_rate_g).minimize(loss_g, var_list=g_parameters)
# train_d = tf.train.GradientDescentOptimizer(learning_rate_d).minimize(loss_d, var_list=d_parameters)

train_g = tf.train.MomentumOptimizer(learning_rate_g,0.6).minimize(loss_g, var_list=g_parameters)
train_d = tf.train.MomentumOptimizer(learning_rate_d,0.6).minimize(loss_d, var_list=d_parameters)

# train_g = tf.train.MomentumOptimizer(learning_rate_g,0.9).minimize(loss_g, var_list=g_parameters)
# train_d = tf.train.MomentumOptimizer(learning_rate_d,0.9).minimize(loss_d, var_list=d_parameters)

data_directory = '/Users/Billy/PycharmProjects/GALR/data/momentum0.6'
os.chdir(data_directory)

start_time = time.time()

for it in range(1,number_of_trails+1):
    # sample parameters
    gamma_vec = np.random.uniform(0.00001,0.1,4)
    phi_vec = np.random.uniform(0.00001, 0.1, 4)
    phi_vec[0] = 0.0 # min(gamma_vec)*0.000000001 # make sure the 'zero' phi is many times smaller than the smallest gamma
                                          # dont want to set to zero to avoid potentialy dividing by zero in adjuster

    res_matrix = np.zeros((len(gamma_vec) * len(phi_vec), sample_size))
    gamma_out_vec, phi_out_vec = np.zeros((len(gamma_vec) * len(phi_vec))), np.zeros((len(gamma_vec) * len(phi_vec)))

    row =0
    for i, p in enumerate(phi_vec):
        for j, k in enumerate(gamma_vec):
            # sample data
            generator_input = np.random.uniform(0, 1, (sample_size, 1))
            real_dist = np.random.normal(real_mean, real_sd, (sample_size, 1))

            print 'Trial: {}/{}'.format(it,number_of_trails)
            print 'Step: {}/{}'.format(row+1, len(gamma_vec) * len(phi_vec))
            print 'Phi: {0}'.format(p)
            print 'Gamma: {0}'.format(k)

            with tf.Session() as sess:
                tf.global_variables_initializer().run()
                # writer = tf.summary.FileWriter('./graphs', sess.graph)
                for step in range(1, learn_steps):
                    start_index = ((step - 1) * mini_batch_size) % sample_size
                    end_index = (step * mini_batch_size) % sample_size

                    x_minibatch = real_dist[start_index:end_index,:]
                    z_minibatch = generator_input[start_index:end_index,:]

                    sess.run(train_d, feed_dict={x: x_minibatch, z: z_minibatch, phi_g: p,phi_d:p, gamma:k})
                    sess.run(train_g, feed_dict={x: x_minibatch, z: z_minibatch, phi_g: p,phi_d:p,gamma:k})

                generated = sess.run(generator(generator_input,g_parameters))
                res_matrix[row] = generated.reshape(sample_size)
                gamma_out_vec[row] = k
                phi_out_vec[row] = p
                row = row+1
                # writer.close()

                # sns.distplot(generated, hist=False, rug=False)
                # sns.distplot(real_dist, hist=False, rug=False)
                # plt.show()

    res_dataframe = pd.DataFrame(data=res_matrix.astype(float))
    gamma_dataframe = pd.DataFrame(data=gamma_out_vec.astype(float))
    phi_dataframe = pd.DataFrame(data=phi_out_vec.astype(float))

    output_dataframe1 = pd.concat([gamma_dataframe.reset_index(drop=True), phi_dataframe], axis=1)
    output_dataframe2 = pd.concat([output_dataframe1.reset_index(drop=True), res_dataframe], axis=1)

    with open("output.csv", 'a') as f:
        output_dataframe2.to_csv(f, sep=',', header=False, float_format='%.9f', index=False)


print 'Total time taken: {0} seconds'.format(time.time()- start_time)



os.system('afplay /System/Library/Sounds/Sosumi.aiff')
