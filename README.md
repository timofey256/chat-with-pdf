# Chat with PDF

Chat-with-PDF is a Haskell project that makes it easier to use PDF files. If you have lecture notes and want to study better for exams, need to quickly get the main points from a research paper, or have a long conference transcript and want to know what was discussed, this project is for you.

## How to use it?
Firstly, you are going to need `cabal`, a package to manage Haskell projects.

Then, clone the repo, specify your **API key** in `Config.hs` and build the project with `cabal build`. It will also install necessary dependencies.

Finally, you can run the program with `cabal run chat-with-paper -- -f /path/to/pdf-file`

## Example usage
Let's say that you want to explore [Mistral 7B](https://arxiv.org/abs/2310.06825) paper. It's included in repo as an example pdf.

Run `cabal run chat-with-paper -- -f mistral-7b.pdf`

```
Welcome to the Chat with PDF bot!

You can ask whatever questions you have regarding your file. For example, you might ask:
 - What the document is about?
 - What are main takeaways from this paper?
 - What research methods were used?  

You are using mistralai/Mixtral-8x7B-Instruct-v0.1 model.

Enter question: What the paper is about?
The paper introduces Mistral 7B, a 7-billion-parameter language model that outperforms other models in various benchmarks, including reasoning, mathematics, and code generation. It uses grouped-query attention and sliding window attention to improve inference speed and handle sequences of arbitrary length with reduced inference cost. The model is released under the Apache 2.0 license, and there is a fine-tuned version available for instruction following, Mistral 7B â Instruct, which surpasses Llama 2 13B â chat model on human and automated benchmarks.

Enter question: What are main architectural decision that Mistral uses to enhance its performance?
Mistral 7B, a 7-billion-parameter language model, employs several architectural decisions to enhance its performance and efficiency. Here are the main ones:

1. **Grouped-Query Attention (GQA)**: This mechanism significantly accelerates the inference speed and reduces memory requirements during decoding, allowing for higher batch sizes and throughput. It's particularly beneficial for real-time applications.

2. **Sliding Window Attention (SWA)**: SWA is designed to handle longer sequences more effectively at a reduced computational cost. It exploits the stacked layers of a transformer to attend information beyond the window size, thereby alleviating a common limitation in LLMs.

3. **Sliding Window Attention with Rolling Buffer Cache**: This technique reduces the cache memory usage by 8x without impacting the model quality. It's particularly useful for handling long sequences.

4. **Pre-fill and Chunking**: When generating a sequence, Mistral 7B pre-fills the (k, v) cache with the prompt and chunks it into smaller pieces if the prompt is very large. This allows for more efficient memory usage during sequence generation.

These architectural decisions contribute to Mistral 7B's enhanced performance and efficiency, making it outperform other models like Llama 2 13B and approach the coding performance of Code-Llama 7B without sacrificing performance on non-code related benchmarks.

Enter question: ...
```

## How it works?

The logic is very simple: the program reads the specified PDF file and then feeds it to an LLM with the user's prompt, then return a response. The default model is Mixture-of-Experts Mixtral-8x7B-Instruct-v0.1