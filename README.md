# Probability of Differentiation Reveals Brittleness of Homogeneity Bias in Large Language Models

Paper and related materials for [Lee](https://lee-messi.github.io/) and [Lai](https://sites.wustl.edu/calvinlai/) (submitted). The abstract for the paper is as follows:

> Homogeneity bias in Large Language Models (LLMs) refers to their tendency to homogenize the representations of some groups compared to others. Previous studies documenting this bias have predominantly used encoder models, which may have inadvertently introduced biases. To address this limitation, we prompted GPT-4 to generate single word/expression completions with regards to 18 situation cues - specific, measurable elements of environments that influence how individuals perceive situations and compared the variability of these completions using *probability of differentiation*. This approach directly assesses homogeneity bias from the model's outputs, bypassing encoder models. Across five studies, we find that homogeneity bias is highly volatile across situation cues and writing prompts, suggesting that the bias observed in past work may reflect that of encoder models rather than LLMs. Furthermore, these results suggest that homogeneity bias in LLMs is brittle, as even minor and arbitrary changes in prompts can significantly alter the expression of bias. Future work should further explore how variations in syntactic features and topic choices in longer text generations influence homogeneity bias in LLMs. 

This paper is under review, but comments are still very welcome. Please feel free to send us an [email](mailto:hojunlee@wustl.edu), or open an "Issue" here. For detailed instructions for reproducing analysis, refer to the [Instructions.md](Instructions.md) document.


## Data Availability Statement

All code and data are available on this repository. 