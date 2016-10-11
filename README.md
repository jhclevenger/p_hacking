# p_hacking

This code simulates two different types of experiments: one where a single sample is collected from two groups and one where samples are collected, tested for significant, and then more data is collected if there is no significant difference. This latter process goes by various names including: data peeking, optional stopping, and p-hacking. These simulations demonstrate the effect of p-hacking on false alarm rates (finding a significant difference between conditions when there is no actual difference) and on effect size estimates (an estimate of the size of the population difference based on the samples collected). 

TL;DR: p_hacking increases false alarm rate and inflates effect size estimates. It's bad, don't do it.
