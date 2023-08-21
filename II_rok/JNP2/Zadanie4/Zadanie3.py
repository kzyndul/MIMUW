def Generator():
    clouded_rgb = tf.keras.layers.Input(shape=[256, 256, 3], name='clouded_rgb')
    clouded_nir = tf.keras.layers.Input(shape=[256, 256, 1], name='clouded_nir')
    down_stack = [
        downsample(64, 4, apply_batchnorm=False),
        downsample(128, 4),
        downsample(256, 4),
        downsample(512, 4),
        downsample(512, 4),
        downsample(512, 4),
        downsample(512, 4),
        downsample(512, 4),
    ]
    up_stack = [
        upsample(512, 4, apply_dropout=True),
        upsample(512, 4, apply_dropout=True),
        upsample(512, 4, apply_dropout=True),
        upsample(512, 4),
        upsample(256, 4),
        upsample(128, 4),
        upsample(64, 4),
    ]
    initializer = tf.random_normal_initializer(0., 0.02)
    last = tf.keras.layers.Conv2DTranspose(OUTPUT_CHANNELS, 4,
                                           strides=2,
                                           padding='same',
                                           kernel_initializer=initializer,
                                           activation='tanh')  # (bs, 256, 256, 3)
    x = tf.keras.layers.concatenate([clouded_rgb, clouded_nir])
    # Downsampling through the model
    for down in down_stack:
        x = down(x)
    # Upsampling
    for up in up_stack:
        x = up(x)
    x = last(x)
    return tf.keras.Model(inputs=[clouded_rgb, clouded_nir], outputs=x)
