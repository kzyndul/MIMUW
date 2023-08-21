def Discriminator():
    initializer = tf.random_normal_initializer(0., 0.02)
    cloud_free_rgb = tf.keras.layers.Input(shape=[256, 256, 3], name='cloud_free_rgb')
    x = cloud_free_rgb
    down1 = downsample(64, 3, False)(x)
    down2 = downsample(128, 3)(down1)
    down3 = downsample(128, 3)(down2)
    down4 = downsample(256, 3)(down3)
    down5 = downsample(256, 3)(down4)
    down6 = downsample(512, 3)(down5)
    down7 = downsample(512, 3)(down6)
    zero_pad1 = tf.keras.layers.ZeroPadding2D()(down7)
    conv = tf.keras.layers.Conv2D(512, 3, strides=1,
                                  kernel_initializer=initializer,
                                  use_bias=False)(zero_pad1)
    batchnorm1 = tf.keras.layers.BatchNormalization()(conv)
    leaky_relu = tf.keras.layers.LeakyReLU()(batchnorm1)
    zero_pad2 = tf.keras.layers.ZeroPadding2D()(leaky_relu)
    layer10 = tf.keras.layers.Conv2D(1, 3, strides=1,
                                     kernel_initializer=initializer)(zero_pad2)
    flatten_layer = tf.keras.layers.Flatten()(layer10)
    dense = tf.keras.layers.Dense(1)(flatten_layer)
    return tf.keras.Model(inputs=cloud_free_rgb, outputs=tf.keras.activations.sigmoid(dense))



@tf.function
def train_step(clouded_rgb, target_cloudfree_rgb, clouded_nir, epoch):
    with tf.GradientTape() as gen_tape, tf.GradientTape() as disc_tape:
        generated_cloudfree_rgb = generator([clouded_rgb[None, :, :, :], clouded_nir[None, :, :, None]], training=True)

        disc_real_output = discriminator(target_cloudfree_rgb[None, :, :, :], training=True)
        disc_generated_output = discriminator(generated_cloudfree_rgb, training=True)

        gen_total_loss, gen_gan_loss, gen_l1_loss = generator_loss(disc_generated_output, generated_cloudfree_rgb,
                                                                   target_cloudfree_rgb)
        disc_loss = discriminator_loss(disc_real_output, disc_generated_output)

    generator_gradients = gen_tape.gradient(gen_total_loss, generator.trainable_variables)
    discriminator_gradients = disc_tape.gradient(disc_loss, discriminator.trainable_variables)

    generator_optimizer.apply_gradients(zip(generator_gradients, generator.trainable_variables))
    discriminator_optimizer.apply_gradients(zip(discriminator_gradients, discriminator.trainable_variables))

    with summary_writer.as_default():
        tf.summary.scalar('gen_total_loss', gen_total_loss, step=epoch)
        tf.summary.scalar('gen_gan_loss', gen_gan_loss, step=epoch)
        tf.summary.scalar('gen_l1_loss', gen_l1_loss, step=epoch)
        tf.summary.scalar('disc_loss', disc_loss, step=epoch)

# Rest of the code remains the same
