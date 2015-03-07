# # Load syuzhet; compute sentiment & foundation for Portrait
library(syuzhet)
path_to_a_text_file <- system.file("extdata", "portrait.txt", package = "syuzhet")
joyces_portrait <- get_text_as_string(path_to_a_text_file)
poa_v <- get_sentences(joyces_portrait)
bing_sent <- get_sentiment(poa_v, method="bing")
bing_foundation <- get_transformed_values(bing_sent, low_pass_size=3, scale_vals=TRUE)
n_terms <- length(bing_sent)

# Some helper functions

# Draw a legend with the standard labels and colors we're going to use
make_legend <- function(include_portrait=TRUE) {
    labels <- c("Emotional Valence", '"Foundation Shape"')
    colors <- c('black', 'blue')
    widths <- c(1,7)
    if (include_portrait){
        labels <- c(labels, expression('"Foundation" of '*italic('Portrait')))
        colors <- c(colors, 'orange')
        widths <- c(widths, 5)
    }
    legend('topleft', labels, lty=1, col=colors, lwd=widths, bty='o', cex=.75)
}

# Our standard image format, shape, and font size
start_image <- function(filename) {
    png(sprintf("%s.png", filename), width=640, height=480, pointsize=18)
}

# This function plots an emotional valence and its "foundation shape",
# optionally compared to the foundation shape of Portrait.
plot_foundation <- function(signal, main_title=NULL, do_scale=FALSE, include_portrait=TRUE){
    domain <- seq(1,100, length.out=length(signal))
    data <- signal
    if (do_scale){
        data <- scale(data)
    }
    plot(domain, data, type='l', col="black",
        xlab="Narrative Time",
        ylab="Emotional Valence",
        main=main_title,
        xlim=c(0, 100),
        ylim=c(-8, 8))
    # We scale the foundation shapes up by 2 to make them easier to see.
    lines(2*get_transformed_values(signal, low_pass_size=3, scale_vals=TRUE), col='blue', lwd=7)
    if (include_portrait) {
       lines(2*bing_foundation, col='orange', lwd=5)
    }
    make_legend(include_portrait)
}


#################################################
# Some variations on the sentiment for Portrait #
#################################################

# Settings for a single plot
par(
    mfrow=c(1,1), # Just one plot
    oma=c(0,0,0,0), # No outer margin
    mar=c(5,5,3.5,3)+.1 # Normal image margin
)

# Variation 1: zero out the last third of the book.
signal = rep(bing_sent)
signal[(2*n_terms/3):n_terms] = 0
start_image("portrait_no_last_third")
plot_foundation(signal, expression("Neutralizing the last third of "*italic("Portrait")))
dev.off()

# Variation 2: zero out all but the middle 20 sentences
signal = rep(bing_sent)
signal[1:2563] = 0
signal[2584:length(signal)] = 0
start_image("portrait_middle_20")
plot_foundation(signal, expression("The middle 20 sentences of "*italic("Portrait")))
dev.off()

# Variation 3: zero out all but the middle 2 sentences
signal = rep(bing_sent)
signal[1:2572] = 0
signal[2575:length(signal)] = 0
start_image("portrait_middle_2")
plot_foundation(signal, expression("The middle 2 sentences of "*italic("Portrait")))
dev.off()

# Variation 4: Add strong positive spikes at the beginning and end
signal = rep(bing_sent)
signal[1:200] <- signal[1:200] + round((200:1)/25)
signal[(n_terms-199):n_terms] <- signal[(n_terms-200):n_terms] + round((1:200)/25)
start_image("portrait_wings")
plot_foundation(signal, expression("Extra positives at start and end"))
dev.off()


##########################################################
# Artificial sentiments with Portrait's foundation shape #
##########################################################

# This function constructs an emotional valence from its FFT and plots it
# along with its foundation shape. If include_portrait is true, it also
# includes Portrait's foundation shape for comparison
plot_from_fft <- function(fft, include_portrait=TRUE) {
    signal <- Re(fft(fft_signal, inverse=T))
    # The scale of the signal doesn't actually affect the result, so we scale
    # it for visibility
    signal <- signal/max(abs(signal))*5
    domain <- seq(1,100, length.out=length(signal))
    plot(domain, signal, type='l', col="black",
        xlim=c(0, 100),
        ylim=c(-8, 8),
        xlab='',
        ylab='')
    lines(2*get_transformed_values(signal, low_pass_size=3, scale_vals=TRUE), col='blue', lwd=7)
    if (include_portrait) {
        lines(2*bing_foundation, col='orange', lwd=5)
    }
}

# This function randomizes all terms of a fourier series above `keep`. The
# random noise falls off linearly, with max magnitude equal to the number of
# randomized terms.
rand_higher <- function(fft_signal, scale=1, keep=3){
    n_terms = length(fft_signal)
    n_junk <- length(fft_signal)-keep
    noise <- (rnorm(n_junk) + rnorm(n_junk)*1i) * (n_junk:1)
    fft_signal[4:n_terms] <- noise*scale
    return(fft_signal)
}

set.seed(1) # Set random seed so this is repeatable

# Take the fft of bing's sentiment
fft_signal <- fft(bing_sent)

# Plot one: Four sentiments that share Portrait's foundation shape exactly
start_image("like_portrait")
# Settings for a 2x2 grid of plots
par(
    mfrow=c(2,2),  # 2x2 grid
    oma=c(0,1,2,1), # Outer margin leaves room for title
    mar=c(4,2,0,1) # Small graph margins (we're not labeling axes either)
)

# weakly randomize the upper harmonics; add strong 4th, 5th, and 7th harmonics
fft_signal <- rand_higher(fft_signal, .001)
fft_signal[4:7] <- c(-10, -5i, 0, -5i)*40
plot_from_fft(fft_signal, include_portrait=F)

# No need for a legend on every chart, they're all the same.
make_legend(include_portrait=F)

# randomize all upper harmonics
fft_signal <- rand_higher(fft_signal, 1)
plot_from_fft(fft_signal, include_portrait=F)

# weakly randomize the upper harmonics; add an overpowering 4th harmonic
fft_signal <- rand_higher(fft_signal, .001)
fft_signal[4] <- -5000
plot_from_fft(fft_signal, include_portrait=F)

# moderately randomize the upper harmonics; add strong 5th-7th harmonics
fft_signal <- rand_higher(fft_signal, .01)
fft_signal[5:8] <- c(5-1i, 6, 0, 5i)*1000
plot_from_fft(fft_signal, include_portrait=F)

# Add an outer title and save the image
title(main=expression("Four graphs sharing a foundation shape"), outer=T)
dev.off()


##################################################################
# One artificial sentiment with four different foundation shapes #
##################################################################

# Plot two: Four nearly-identical sentiments with wildly different foundation
# shapes.

# We reuse the signal from the fourth graph above, but this time we do
# whatever we want with the first three harmonics (really only the second and
# third - Syuzhet's scaling effectively wipes out the first term anyway)
start_image("varying_foundations")
# Settings for a 2x2 grid of plots
par(
    mfrow=c(2,2),  # 2x2 grid
    oma=c(0,1,2,1), # Outer margin leaves room for title
    mar=c(4,2,0,1) # Small graph margins (we're not labeling axes either)
)

# Use Portrait's foundation shape ("Man in a hole")
fft_signal[1:3] <- fft(bing_sent)[1:3]
plot_from_fft(fft_signal, include_portrait=F)
make_legend(include_portrait=F)

# Invert Portrait's foundation shape  ("Man on a hill")
fft_signal[1:3] <- -fft_signal[1:3]
plot_from_fft(fft_signal, include_portrait=F)

# Set the first three terms to something else
fft_signal[1:3] <- c(0, -1i, 0)
plot_from_fft(fft_signal, include_portrait=F)

# Set the first three terms to something else
fft_signal[1:3] <- c(0, 1+2i, 2i)
plot_from_fft(fft_signal, include_portrait=F)

title(main=expression("One graph with four different foundation shapes"), outer=T)
dev.off()
