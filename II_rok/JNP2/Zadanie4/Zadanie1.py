def Discriminator():
    initializer = tf.random_normal_initializer(0., 0.02)
    # edge_clouded_nir = tf.keras.layers.Input(shape=[256, 256, 1], name='edge_clouded_nir')
    cloud_free_rgb = tf.keras.layers.Input(shape=[256, 256, 3], name='cloud_free_rgb')
    edge_cloud_free_rgb = tf.keras.layers.Input(shape=[256, 256, 1], name='edge_cloud_free_rgb')
    # gray_edge_clouded_rgb = tf.keras.layers.Input(shape=[256, 256, 1], name='gray_edge_clouded_rgb')
    x = tf.keras.layers.concatenate([cloud_free_rgb, edge_cloud_free_rgb])
    down1 = downsample(64, 4, False)(x)
    down2 = downsample(128, 4)(down1)
    down3 = downsample(128, 4)(down2)
    down4 = downsample(256, 4)(down3)
    down5 = downsample(256, 4)(down4)
    down6 = downsample(512, 4)(down5)
    down7 = downsample(512, 4)(down6)
    zero_pad1 = tf.keras.layers.ZeroPadding2D()(down7)
    conv = tf.keras.layers.Conv2D(512, 4, strides=1,
                                  kernel_initializer=initializer,
                                  use_bias=False)(zero_pad1)
    batchnorm1 = tf.keras.layers.BatchNormalization()(conv)
    leaky_relu = tf.keras.layers.LeakyReLU()(batchnorm1)
    zero_pad2 = tf.keras.layers.ZeroPadding2D()(leaky_relu)
    layer10 = tf.keras.layers.Conv2D(1, 3, strides=1,
                                     kernel_initializer=initializer)(zero_pad2)
    flatten_layer = tf.keras.layers.Flatten()(layer10)
    dense = tf.keras.layers.Dense(1)(flatten_layer)
    return tf.keras.Model(inputs=[cloud_free_rgb, edge_cloud_free_rgb], outputs=tf.keras.activations.sigmoid(dense))

#loss_object = tf.keras.losses.BinaryCrossentropy(from_logits=True)

def discriminator_loss(disc_real_output, disc_generated_output):
    real_loss = loss_object(tf.ones_like(disc_real_output), disc_real_output)
    generated_loss = loss_object(tf.zeros_like(disc_generated_output), disc_generated_output)
    total_disc_loss = real_loss + generated_loss
    return total_disc_loss